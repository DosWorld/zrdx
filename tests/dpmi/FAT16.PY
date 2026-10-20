"""Read FAT16 images and put test files into an owned scratch image copy."""
import argparse
from pathlib import Path
import runpy
import struct


class Fat16:
    def __init__(self, path, writable=False):
        self.path = Path(path)
        self.file = self.path.open("r+b" if writable else "rb")
        mbr = self.read(0, 512)
        self.start = struct.unpack_from("<I", mbr, 454)[0] * 512
        boot = self.read(self.start, 512)
        self.sector = struct.unpack_from("<H", boot, 11)[0]
        self.cluster = boot[13] * self.sector
        self.reserved = struct.unpack_from("<H", boot, 14)[0]
        self.copies = boot[16]
        self.entries = struct.unpack_from("<H", boot, 17)[0]
        self.fatbytes = struct.unpack_from("<H", boot, 22)[0] * self.sector
        self.fatpos = self.start + self.reserved * self.sector
        self.rootpos = self.fatpos + self.copies * self.fatbytes
        rootbytes = ((self.entries * 32 + self.sector - 1) // self.sector) * self.sector
        self.datapos = self.rootpos + rootbytes
        total = struct.unpack_from("<H", boot, 19)[0] or struct.unpack_from("<I", boot, 32)[0]
        self.count = (total * self.sector - (self.datapos - self.start)) // self.cluster
        if mbr[510:512] != b"\x55\xaa" or boot[510:512] != b"\x55\xaa" or not 4085 <= self.count < 65525:
            raise ValueError("not a partitioned FAT16 image")
        self.fat = bytearray(self.read(self.fatpos, self.fatbytes))

    def read(self, offset, size):
        self.file.seek(offset)
        data = self.file.read(size)
        if len(data) != size:
            raise ValueError("short image read")
        return data

    def write(self, offset, data):
        self.file.seek(offset)
        if self.file.write(data) != len(data):
            raise ValueError("short image write")

    def chain(self, cluster):
        seen = set()
        while cluster < 0xfff8:
            if not 2 <= cluster < self.count + 2 or cluster in seen:
                raise ValueError("invalid/cyclic FAT chain")
            seen.add(cluster)
            yield cluster
            cluster = struct.unpack_from("<H", self.fat, cluster * 2)[0]

    def position(self, cluster):
        return self.datapos + (cluster - 2) * self.cluster

    @staticmethod
    def name83(name):
        parts = name.upper().split(".")
        if len(parts) > 2 or not 1 <= len(parts[0]) <= 8 or (len(parts) == 2 and not 1 <= len(parts[1]) <= 3):
            raise ValueError("name must use DOS 8.3")
        return (parts[0].ljust(8) + (parts[1] if len(parts) == 2 else "").ljust(3)).encode("ascii")

    def directory(self, cluster=0):
        chunks = [(self.rootpos, self.entries * 32)] if not cluster else [(self.position(c), self.cluster) for c in self.chain(cluster)]
        for base, size in chunks:
            data = self.read(base, size)
            for offset in range(0, size, 32):
                yield base + offset, data[offset:offset + 32]

    def find(self, path):
        cluster = 0
        parts = path.replace("\\", "/").strip("/").split("/")
        for index, name in enumerate(parts):
            target = self.name83(name)
            for offset, entry in self.directory(cluster):
                if entry[0] not in (0, 0xe5) and entry[11] != 15 and entry[:11] == target:
                    break
            else:
                raise FileNotFoundError(path)
            if index != len(parts) - 1 and not entry[11] & 16:
                raise ValueError("path component is not a directory")
            cluster = struct.unpack_from("<H", entry, 26)[0]
        return offset, entry

    def get(self, path):
        _, entry = self.find(path)
        size = struct.unpack_from("<I", entry, 28)[0]
        cluster = struct.unpack_from("<H", entry, 26)[0]
        data = b"" if not size else b"".join(self.read(self.position(c), self.cluster) for c in self.chain(cluster))
        if len(data) < size:
            raise ValueError("truncated file chain")
        return data[:size]

    def put_root(self, name, data):
        # Mutations are restricted to the scratch root; never modify the user's disk.
        runpy.run_path(str(Path(__file__).with_name("ZRCFG.PY")))["check_scratch"](self.path)
        target = self.name83(name)
        try:
            offset, old = self.find(name)
            if old[11] & (8 | 16):
                raise ValueError("cannot replace directory or volume label")
        except FileNotFoundError:
            offset = next(pos for pos, entry in self.directory() if entry[0] in (0, 0xe5))
            old = bytes(32)
        needed = (len(data) + self.cluster - 1) // self.cluster
        free = [c for c in range(2, self.count + 2) if struct.unpack_from("<H", self.fat, c * 2)[0] == 0][:needed]
        if len(free) != needed:
            raise ValueError("image has insufficient free clusters")
        oldcluster = struct.unpack_from("<H", old, 26)[0]
        oldchain = list(self.chain(oldcluster)) if oldcluster else []
        for index, cluster in enumerate(free):
            chunk = data[index * self.cluster:(index + 1) * self.cluster]
            self.write(self.position(cluster), chunk.ljust(self.cluster, b"\0"))
            struct.pack_into("<H", self.fat, cluster * 2, free[index + 1] if index + 1 < len(free) else 0xffff)
        for cluster in oldchain:
            struct.pack_into("<H", self.fat, cluster * 2, 0)
        for copy in range(self.copies):
            self.write(self.fatpos + copy * self.fatbytes, self.fat)
        entry = bytearray(32)
        entry[:11] = target
        entry[11] = 32
        struct.pack_into("<H", entry, 26, free[0] if free else 0)
        struct.pack_into("<I", entry, 28, len(data))
        self.write(offset, entry)
        self.file.flush()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("image", type=Path)
    parser.add_argument("path", nargs="?")
    args = parser.parse_args()
    volume = Fat16(args.image)
    if args.path:
        import sys
        sys.stdout.buffer.write(volume.get(args.path))
    else:
        for _, entry in volume.directory():
            if entry[0] not in (0, 0xe5) and entry[11] != 15:
                print(entry[:11].decode("ascii", errors="replace"))
