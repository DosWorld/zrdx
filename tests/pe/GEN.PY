"""Build small PE32 fixtures from HELLO.BIN and RELOC.BIN.

RELOC.BIN/RELOC2.BIN/RELOCH.BIN are one program linked at
0x401000, 0x501000 and 0x10001000; the bytes that differ between
the first two are its relocation sites. No DLL/import features.
"""
from pathlib import Path
import struct

VALID = ("GOOD", "SEC30", "MULTI", "BASEHI", "MODS", "EXPS",
         "EXPB", "HOST",
         "CHAIN", "CYCLE", "ORD", "ICYC", "RT", "RF", "FWDA",
         "FWDO", "BOUND", "LOWMEM",
         "LACK", "HOSTC")
PMXONLY = ("MODS", "EXPS", "EXPB", "HOSTC")
PE = 0x80
RELOC = 0x42000040


def roundup(n, a):
    return (n + a - 1) // a * a


def reloc_blocks(sites):
    """One HIGHLOW block per 4 KB page, padded to 4 bytes."""
    out = b""
    for page in sorted({s >> 12 for s in sites}):
        ent = [0x3000
               | (s & 0xfff) for s in sorted(sites) if s >> 12
               == page]
        ent += [0] * (len(ent) % 2)
        out += struct.pack("<II", page << 12, 8 + 2 * len(ent))
        out += b"".join(struct.pack("<H", e) for e in ent)
    return out


def sites_of(lo, hi, rva):
    """RVAs of dwords that differ between two links (byte 2
    changes).
    """
    return [rva + i - 2 for i in range(len(lo)) if lo[i]
            != hi[i]]


def make_pe(secs, base, chars=0x102):
    """secs: (name, rva, data, flags); .reloc is the reloc
    directory.
    """
    image = bytearray(0x200)
    def w(off, val):
        struct.pack_into("<H", image, off, val)
    def d(off, val):
        struct.pack_into("<I", image, off, val)
    image[:2] = b"MZ"
    w(2, 0x80)
    w(4, 1)
    w(8, 4)
    w(24, 0x40)
    d(0x3c, PE)
    image[PE:PE+4] = b"PE\0\0"
    w(PE+4, 0x14c)
    w(PE+6, len(secs))
    w(PE+20, 224)
    w(PE+22, chars)
    w(PE+24, 0x10b)
    d(PE+40, 0x1000)
    d(PE+52, base)
    d(PE+56, 0x1000)
    d(PE+60, 0x200)
    d(PE+80,
      max(r + roundup(len(x), 0x1000) for _, r, x, _ in secs))
    d(PE+84, 0x200)
    d(PE+96, 0x10000)
    d(PE+116, 16)
    for n, (name, rva, data, flags) in enumerate(secs):
        off = PE + 248 + 40 * n
        raw = roundup(len(data), 0x200)
        image[off:off+len(name)] = name
        d(off+8, len(data))
        d(off+12, rva)
        d(off+16, raw)
        d(off+20, len(image))
        d(off+36, flags)
        if name == b".reloc":
            d(PE+160, rva)
            d(PE+164, len(data))
        image += data.ljust(raw, b"\0")
    return image


def with_sections(image, extra):
    """image with EXTRA more empty sections: a wider header."""
    img = bytearray(image)
    n = struct.unpack_from("<H", img, PE + 6)[0]
    hsz = roundup(PE + 248 + 40 * (n + extra), 0x200)
    img[0x200:0x200] = bytes(hsz - 0x200)
    for i in range(n):
        off = PE + 248 + 40 * i + 20
        ptr = struct.unpack_from("<I", img, off)[0]
        struct.pack_into("<I", img, off, ptr + hsz - 0x200)
    top = struct.unpack_from("<I", img, PE + 80)[0]
    struct.pack_into("<I", img, PE + 84, hsz)
    struct.pack_into("<H", img, PE + 6, n + extra)
    struct.pack_into("<I", img, PE + 80, top + 0x1000 * extra)
    for k in range(extra):
        off = PE + 248 + 40 * (n + k)
        img[off:off + 3] = b".s" + bytes([97 + k % 26])
        struct.pack_into("<IIIII", img, off + 8, 0x1000,
                         top + 0x1000 * k, 0, 0, 0)
        struct.pack_into("<I", img, off + 36, 0xC0000080)
    return bytes(img)


def reloc_cases(folder):
    """Valid multi-block images, a stripped one and damaged
    blocks.
    """
    lo, hi, high = (
        (folder / n).read_bytes() for n in ("RELOC.BIN",
            "RELOC2.BIN", "RELOCH.BIN"))
    sites = sites_of(lo[:0x1000], hi[:0x1000], 0x1000)
    sites += sites_of(lo[0x1000:], hi[0x1000:], 0x2000)
    rel = reloc_blocks(sites)
    def build(text, base, reloc=rel, chars=0x102):
        secs = [(b".text", 0x1000, text[:0x1000], 0x60000020),
                (b".data", 0x2000, text[0x1000:], 0xc0000040)]
        if reloc:
            secs.append((b".reloc", 0x3000, reloc, RELOC))
        return bytes(make_pe(secs, base, chars))
    cases = {"MULTI": build(lo, 0x400000),
             "BASEHI": build(high, 0x10000000),
             "NOREL": build(lo, 0x7f000000, b"", 0x103)}
    second = rel.index(struct.pack("<I", 0x2000))
    damaged = bytearray(rel)
    struct.pack_into("<I", damaged, second + 4, 7)
    cases["BAD2ND"] = build(lo, 0x400000, bytes(damaged))
    damaged = bytearray(rel)
    struct.pack_into("<H", damaged, 8, 0x1001)
    cases["TYPEHI"] = build(lo, 0x400000, bytes(damaged))
    damaged = bytearray(rel)
    struct.pack_into("<H", damaged, 8, 0xa001)
    cases["TYPE10"] = build(lo, 0x400000, bytes(damaged))
    return cases


JOBS = (("MODS", "MODS.ASM", ()), ("EXPS", "EXPS.ASM", ()),
        ("EXPB", "EXPB.ASM", ()),
        ("DRV8", "DRV.ASM", ("-DEXPECT=8",)),
        ("DRV41", "DRV.ASM", ("-DEXPECT=41",)),
        ("DRV42", "DRV.ASM", ("-DEXPECT=42",)),
        ("FNCALL", "FN.ASM", ("-DCALLIMP",)),
        ("FN40", "FN.ASM", ("-DVALUE=40",)),
        ("FN42", "FN.ASM", ("-DVALUE=42",)),
        ("FN55", "FN.ASM", ("-DVALUE=55",)),
        ("DRV55", "DRV.ASM", ("-DEXPECT=55",)),
        ("DRV40", "DRV.ASM", ("-DEXPECT=40",)),
        ("INIT", "INIT.ASM", ()),
        ("DMA", "DM.ASM", ("-DLETTER=65",)),
        ("DMB", "DM.ASM", ("-DLETTER=66",)),
        ("DMC", "DM.ASM", ("-DLETTER=67",)),
        ("DMP", "DM.ASM", ("-DLETTER=80",)),
        ("DMQ", "DM.ASM", ("-DLETTER=81",)),
        ("DMX", "DM.ASM", ("-DLETTER=88",)),
        ("DMY", "DM.ASM", ("-DLETTER=89", "-DRESULT=0")),
        ("DMF", "DM.ASM", ("-DLETTER=70", "-DRESULT=0")),
        ("FN99", "FN.ASM", ("-DVALUE=99",)),
        ("RT", "RT.ASM", ("-I../../sdk/",)),
        ("PLUGX", "PLUGX.ASM", ()),
        ("REENT", "REENT.ASM", ()),
        ("RF", "RT.ASM",
         ("-I../../sdk/", "-DTAG='unload'", "-DRETEXIT")),
        ("PLUGF", "PLUGF.ASM", ()),
        ("EXITD", "EXITD.ASM", ()),
        ("LOWMEM", "LOWMEM.ASM", ("-I../../sdk/",)),
        ("LACK", "LACK.ASM", ("-I../../sdk/",)))
GRAPH = ("HOST", "CHAIN", "CYCLE", "FWDA", "FWDO", "BOUND")
INITS = ("ORD", "ICYC")
RUNTIME = ("RT",)
UNLOAD = ("RF",)
LOWMEMS = ("LOWMEM",)

BAD = "bad PE format"
EXPECT = {"EXPBAD": BAD, "EXPSHORT": BAD, "IMPBAD": BAD,
          "IMPSIZE": BAD,
          "IMPORT": BAD, "SEC33": BAD,
          "IMPOK": "bad PE import table",
          "ALIGN": BAD, "ALIGN2": BAD, "DLLFLAG": BAD,
          "PE32P": "unsupported PE format",
          "ARM": "unsupported PE format",
          "NODLL": "can't open PE DLL: MISSING.DLL",
          "NOSYM": "unresolved PE import: C.DLL",
          "COLL": "PE module name conflict: DUP.DLL",
          "NOTDLL": "bad PE DLL image: X.DLL",
          "ORD": ("CBA", "PE init: ok"),
          "ICYC": ("QP", "PE init: ok"),
          "IFAIL": ("XYx",
                    "PE DLL initialization failed: IY.DLL"),
          "RT": ("FR", "PE runtime: ok"),
          "RF": ("CBAabcCBAabcQPpq", "PE unload: ok",
                 "PE exit detach"),
          "FWDLOOP": "unresolved PE import: LOOP1.DLL",
          "FWDMISS": "can't open PE DLL: NOPE.DLL",
          "BOUNDX": "bad PE import table",
          "TRUNCD": "can't open PE DLL: TRUNC.DLL",
          "ARMD": "bad PE DLL image: ARM.DLL",
          "BRELD": "bad PE DLL image: BREL.DLL",
          "HUGED": "out of DPMI memory",
          "LOWMEM": "PE low memory: ok",
          "LACK": "PE lack: skip", "HOSTC": "PE C: ok",
          "TLSD": "bad PE DLL image: TLSD.DLL",
          "DELAY": "PE directory is not supported"}


def export_blob(rva, modname, badnames=False):
    """Export directory: base 5, 6 slots (1 empty, 1 forwarder).
    """
    names = [b"Alpha", b"Beta", b"Fwd", b"Gamma", b"alpha"]
    order = [0, 2, 4, 3, 5]
    strs = bytearray(modname + b"\0")
    where = []
    for n in names + [b"C.Fc"]:
        where.append(rva + 96 + len(strs))
        strs += n + b"\0"
    funcs = [0x1010, 0, 0x1020, 0x1030, where[-1], 0x1040]
    blob = struct.pack("<IIHHIIIIIII", 0, 0, 0, 0, rva + 96, 5,
                       6, 5,
                       rva + 40,
                       0x5000 if badnames else rva + 64,
                       rva + 84)
    blob += b"".join(struct.pack("<I", f) for f in funcs)
    blob += b"".join(struct.pack("<I", w) for w in where[:-1])
    blob += b"".join(struct.pack("<H", o) for o in order)
    return (blob + bytes(96 - len(blob)) + bytes(strs))


def export_case(folder, stem, badnames=False):
    lo = (folder / (stem + ".BIN")).read_bytes()
    hi = (folder / (stem + "2.BIN")).read_bytes()
    blob = export_blob(0x2000, (stem + ".EXE").encode(),
                       badnames)
    image = bytearray(make_pe(
        [(b".text", 0x1000, lo, 0x60000020),
         (b".edata", 0x2000, blob, 0x40000040),
         (b".reloc", 0x3000,
          reloc_blocks(sites_of(lo, hi, 0x1000)), RELOC)],
        0x400000))
    struct.pack_into("<II", image, PE + 120, 0x2000, len(blob))
    return bytes(image)


def module(folder, job, modname, exports=(), imports=(),
           dll=False, oft=True,
           bound=False):
    """PE with one .rdata at 2000h: imports (IAT 2400h, lookup
    2600h, names from 2800h) and exports at 2C00h."""
    lo = (folder / (job + ".BIN")).read_bytes()
    hi = (folder / (job + "2.BIN")).read_bytes()
    secs = [(b".text", 0x1000, lo, 0x60000020)]
    imp = None
    if imports:
        blob = bytearray(0x800 + 0x400)
        desc, iat, npos = b"", 0x400, 0x800
        for dname, syms in imports:
            dpos = npos
            blob[npos:npos + len(dname)
                 + 1] = dname.encode() + b"\0"
            npos += (len(dname) + 2) & ~1
            first = iat
            for s in syms:
                if isinstance(s, int):
                    val = 0x80000000 | s
                else:
                    val = 0x2000 + npos
                    blob[npos:npos + 2 + len(s)
                         + 1] = b"\0\0" + s.encode() + b"\0"
                    npos += (len(s) + 4) & ~1
                struct.pack_into("<I", blob, iat, val)
                struct.pack_into("<I", blob, iat + 0x200, val)
                iat += 4
            iat += 4
            if bound and oft:
                for k in range((iat - first) // 4 - 1):
                    struct.pack_into("<I", blob, first + 4 * k,
                                     0x77000000 + k)
            desc += struct.pack("<IIIII",
                                0x2600 + first
                                - 0x400 if oft else 0,
                                0xFFFFFFFF if bound else 0, 0,
                                0x2000 + dpos,
                                0x2000 + first)
        blob[:len(desc)] = desc
        imp = bytes(blob[:npos])
    exp = None
    rdata = bytearray(imp or b"")
    if exports:
        funcs = sorted(exports)
        strs = bytearray(modname.encode() + b"\0")
        where = []
        for n, _ in funcs:
            where.append(0x2C00 + 96 + len(strs))
            strs += n.encode() + b"\0"
        frva = []
        for _, r in funcs:
            if isinstance(r, str):
                frva.append(0x2C00 + 96 + len(strs))
                strs += r.encode() + b"\0"
            else:
                frva.append(r)
        exp = struct.pack("<IIHHIIIIIII", 0, 0, 0, 0,
                          0x2C00 + 96, 1, len(funcs),
                          len(funcs), 0x2C00 + 40,
                          0x2C00 + 40 + 4 * len(funcs),
                          0x2C00 + 40 + 8 * len(funcs))
        exp += b"".join(struct.pack("<I", r) for r in frva)
        exp += b"".join(struct.pack("<I", w) for w in where)
        exp += b"".join(struct.pack("<H",
                                    i) for i in range(
                                        len(funcs)))
        exp = exp + bytes(96 - len(exp)) + bytes(strs)
        rdata = rdata.ljust(0xC00, b"\0") + exp
    if rdata:
        secs.append((b".rdata", 0x2000, bytes(rdata),
                     0xc0000040))
    secs.append((b".reloc", 0x3000,
                 reloc_blocks(sites_of(lo, hi, 0x1000)), RELOC))
    image = bytearray(make_pe(secs, 0x400000,
                              0x2102 if dll else 0x102))
    if imp:
        struct.pack_into("<II", image, PE + 128, 0x2000,
                         20 * (len(imports) + 1))
    if exp:
        struct.pack_into("<II", image, PE + 120, 0x2C00,
                         len(exp))
    return bytes(image)


def graph_cases(folder):
    """Import graph fixtures: EXE cases plus DLL files in folder.
    """
    def dll(name, *args, **kw):
        (folder / name).write_bytes(module(folder, *args, **kw))
    dll("PLUG.DLL", "FNCALL", "PLUG.DLL", [("PlugGo", 0x1000)],
        [("HOST.EXE", ["HostVal"])], dll=True)
    dll("B.DLL", "FNCALL", "B.DLL", [("Fb", 0x1000)],
        [("C.DLL", ["Fc"])],
        dll=True, oft=False)
    dll("C.DLL", "FN40", "C.DLL", [("Fc", 0x1000)], dll=True)
    dll("CYC1.DLL", "FN42", "CYC1.DLL", [("F1", 0x1000)],
        [("CYC2.DLL", ["F2"])], dll=True)
    dll("CYC2.DLL", "FN42", "CYC2.DLL", [("F2", 0x1000)],
        [("CYC1.DLL", ["F1"])], dll=True)
    dll("DUP.DLL", "FN40", "COLL.EXE", [("F", 0x1000)], dll=True)
    dll("X.DLL", "FN40", "X.DLL", [("F", 0x1000)], dll=False)
    f = [("F", 0x1000)]
    dll("IA.DLL", "DMA", "IA.DLL", f,
        [("IB.DLL", ["F"]), ("IC.DLL", ["F"])], dll=True)
    dll("IB.DLL", "DMB", "IB.DLL", f, [("IC.DLL", ["F"])],
        dll=True)
    dll("IC.DLL", "DMC", "IC.DLL", f, dll=True)
    dll("IP.DLL", "DMP", "IP.DLL", f, [("IQ.DLL", ["F"])],
        dll=True)
    dll("IQ.DLL", "DMQ", "IQ.DLL", f, [("IP.DLL", ["F"])],
        dll=True)
    dll("IX.DLL", "DMX", "IX.DLL", f, dll=True)
    dll("IY.DLL", "DMY", "IY.DLL", f, dll=True)
    dll("EXTRA.DLL", "FN99", "EXTRA.DLL", [("Extra", 0x1000)],
        dll=True)
    dll("EXTRAF.DLL", "DMF", "EXTRAF.DLL", f, dll=True)
    dll("REENT.DLL", "REENT", "REENT.DLL", f,
        [("RT.EXE", ["LoadLibrary", "GetLastError"])], dll=True)
    dll("PLUGX.DLL", "PLUGX", "PLUGX.DLL", [("PlugRun", 0x1010)],
        [("RT.EXE",
          ["LoadLibrary", "GetProcAddress", "GetModuleHandle",
                     "FreeLibrary", "GetLastError"])], dll=True)
    dll("EXITD.DLL", "EXITD", "EXITD.DLL", f, dll=True)
    dll("PLUGF.DLL", "PLUGF", "PLUGF.DLL", [("PlugRun", 0x1010)],
        [("RF.EXE",
          ["LoadLibrary", "GetModuleHandle", "FreeLibrary"])],
        dll=True)
    dll("REAL.DLL", "FN55", "REAL.DLL", [("Real", 0x1000)],
        dll=True)
    dll("FWD.DLL", "FN40", "FWD.DLL",
        [("Alias", "REAL.Real"), ("ByOrd", "REAL.#1")], dll=True)
    dll("LOOP1.DLL", "FN40", "LOOP1.DLL", [("A", "LOOP2.A")],
        dll=True)
    dll("LOOP2.DLL", "FN40", "LOOP2.DLL", [("A", "LOOP1.A")],
        dll=True)
    dll("MISS.DLL", "FN40", "MISS.DLL", [("X", "NOPE.Y")],
        dll=True)
    good = module(folder, "FNCALL", "BAD.DLL", [("F", 0x1000)],
                  [("HOST.EXE", ["HostVal"])], dll=True)
    def broken(name, offset, fmt, value):
        b = bytearray(good)
        struct.pack_into(fmt, b, PE + offset, value)
        (folder / name).write_bytes(bytes(b))
    (folder / "TRUNC.DLL").write_bytes(good[:0x300])
    broken("ARM.DLL", 4, "<H", 0x1c0)
    broken("BREL.DLL", 164, "<I", 4)
    broken("HUGE.DLL", 80, "<I", 0x70000000)
    b = bytearray(good)
    struct.pack_into("<II", b, PE + 192, 0x2100, 0x18)
    (folder / "TLSD.DLL").write_bytes(bytes(b))
    exe = lambda job, name, imports, exports=(), **kw: module(
        folder, job, name, exports, imports, **kw)
    return {
        "HOST": exe("DRV8", "HOST.EXE",
                    [("PLUG.DLL", ["PlugGo"])],
                    [("HostVal", 0x1010)]),
        "CHAIN": exe("DRV41", "CHAIN.EXE", [("B.DLL", ["Fb"])]),
        "CYCLE": exe("DRV42", "CYCLE.EXE",
                     [("CYC1.DLL", ["F1"])]),
        "NODLL": exe("DRV8", "NODLL.EXE",
                     [("MISSING.DLL", ["F"])]),
        "NOSYM": exe("DRV8", "NOSYM.EXE", [("C.DLL", ["Nope"])]),
        "COLL": exe("DRV8", "COLL.EXE", [("DUP.DLL", ["F"])]),
        "NOTDLL": exe("DRV8", "NOTDLL.EXE", [("X.DLL", ["F"])]),
        "FWDA": exe("DRV55", "FWDA.EXE",
                    [("FWD.DLL", ["Alias"])]),
        "FWDO": exe("DRV55", "FWDO.EXE",
                    [("FWD.DLL", ["ByOrd"])]),
        "FWDLOOP": exe("DRV8", "FWDLOOP.EXE",
                       [("LOOP1.DLL", ["A"])]),
        "FWDMISS": exe("DRV8", "FWDMISS.EXE",
                       [("MISS.DLL", ["X"])]),
        "BOUND": exe("DRV40", "BOUND.EXE", [("C.DLL", ["Fc"])],
                     bound=True),
        "BOUNDX": exe("DRV40", "BOUNDX.EXE",
                      [("C.DLL", ["Fc"])], oft=False,
                      bound=True),
        "RT": exe("RT", "RT.EXE", [("PLUGX.DLL", ["PlugRun"])],
                  [("LoadLibrary", 0x1020),
                   ("GetModuleHandle", 0x1040),
                   ("GetProcAddress", 0x1060),
                   ("FreeLibrary", 0x1080),
                   ("GetLastError", 0x10A0)]),
        "RF": exe("RF", "RF.EXE", [("PLUGF.DLL", ["PlugRun"])],
                  [("LoadLibrary", 0x1020),
                   ("GetModuleHandle", 0x1040),
                   ("GetProcAddress", 0x1060),
                   ("FreeLibrary", 0x1080),
                   ("GetLastError", 0x10A0)]),
        "LOWMEM": exe("LOWMEM", "LOWMEM.EXE",
                      [("EXTRA.DLL", ["Extra"])]),
        "LACK": exe("LACK", "LACK.EXE",
                    [("EXTRA.DLL", ["Extra"])]),
        "TRUNCD": exe("DRV8", "TRUNCD.EXE",
                      [("TRUNC.DLL", ["F"])]),
        "ARMD": exe("DRV8", "ARMD.EXE", [("ARM.DLL", ["F"])]),
        "BRELD": exe("DRV8", "BRELD.EXE", [("BREL.DLL", ["F"])]),
        "HUGED": exe("DRV8", "HUGED.EXE", [("HUGE.DLL", ["F"])]),
        "TLSD": exe("DRV8", "TLSD.EXE", [("TLSD.DLL", ["F"])]),
        "ORD": exe("INIT", "ORD.EXE", [("IA.DLL", ["F"])]),
        "ICYC": exe("INIT", "ICYC.EXE", [("IP.DLL", ["F"])]),
        "IFAIL": exe("INIT", "IFAIL.EXE",
                     [("IX.DLL", ["F"]), ("IY.DLL", ["F"])]),
    }


def wants_pmx(data):
    """Binder rule (docs/PEABI.MD): DLL flag or non-empty
    imports.
    """
    pe = struct.unpack_from("<I", data, 0x3c)[0]
    flags = struct.unpack_from("<H", data, pe + 22)[0]
    dirs, rva, size = struct.unpack_from("<III", data,
        pe + 116)[0], 0, 0
    if dirs >= 2:
        rva, size = struct.unpack_from("<II", data, pe + 128)
    return bool(flags & 0x2000 or rva or size)


def generate(folder):
    folder = Path(folder)
    code = (folder / "HELLO.BIN").read_bytes()
    pe = PE
    sec = pe + 248
    image = make_pe([(b".text", 0x1000, code, 0x60000020),
                     (b".reloc", 0x2000,
                      struct.pack("<IIHH", 0x1000, 12, 0x3001,
                                  0), RELOC)],
                    0x400000)
    cases = {"GOOD": bytes(image)}
    cases["SEC30"] = with_sections(image, 28)
    cases["SEC33"] = with_sections(image, 31)
    lo = (folder / "MODS.BIN").read_bytes()
    hi = (folder / "MODS2.BIN").read_bytes()
    cases["MODS"] = bytes(make_pe(
        [(b".text", 0x1000, lo, 0x60000020),
         (b".reloc", 0x2000,
          reloc_blocks(sites_of(lo, hi, 0x1000)), RELOC)],
        0x400000))
    cases.update(graph_cases(folder))
    cases["EXPS"] = export_case(folder, "EXPS")
    cases["EXPB"] = export_case(folder, "EXPB", True)
    def change(name, off, val, fmt="<I"):
        b = bytearray(image)
        struct.pack_into(fmt, b, off, val)
        cases[name] = bytes(b)
    change("OPTSHORT", pe+20, 95, "<H")
    change("DIRCOUNT", pe+116, 17)
    change("SECTOVR", sec+8, 0xfffffff0)
    change("SECTHDR", sec+12, 0x100)
    change("OVERLAP", sec+40+12, 0x1000)
    change("RAWOVR", sec+20, 0xfffffff0)
    change("RAWSHORT", sec+20, 0x500)
    change("RELOC4", pe+164, 4)
    change("RELOC7", 0x404, 7)
    change("RELOC9", 0x404, 9)
    b = bytearray(cases["RELOC9"])
    struct.pack_into("<I", b, pe+164, 9)
    cases["RELOC9"] = bytes(b)
    change("RELOCWR", 0x400, 0xffffffff)
    change("TARGET", 0x408, 0x3fff, "<H")
    b = bytearray(cases["TARGET"])
    struct.pack_into("<I", b, 0x400, 0x2000)
    cases["TARGET"] = bytes(b)
    b = bytearray(cases["TARGET"])
    struct.pack_into("<I", b, pe+80, 0x2ffe)
    struct.pack_into("<H", b, 0x408, 0x3ffb)
    cases["BYTEEND"] = bytes(b)
    change("HEADERS", pe+84, 0)
    change("ENTRY", pe+40, 0x3000)
    change("IMPORT", pe+128, 0x1000)
    change("IMPSIZE", pe+132, 8)
    change("DLLFLAG", pe+22,
           struct.unpack_from("<H", image, pe+22)[0] | 0x2000,
           "<H")
    change("EXPBAD", pe+120, 0x2fe0)
    change("EXPSHORT", pe+120, 0x1000)
    change("IMPBAD", pe+128, 0x2ff0)
    change("IMPOK", pe+128, 0x1000)
    change("ALIGN", pe+56, 0x1800)
    change("ALIGN2", pe+60, 0x2000)
    change("PE32P", pe+24, 0x20b, "<H")
    change("ARM", pe+4, 0x1c0, "<H")
    change("DELAY", pe+224, 0x1000)
    change("COM", pe+232, 0x1000)
    change("COMSZ", pe+236, 8)
    cases["TRUNC"] = bytes(image[:pe+250])
    for name, off, val in (("EXPBAD", pe+124, 0x40),
                           ("EXPSHORT", pe+124, 8),
                           ("IMPBAD", pe+132, 0x20),
                           ("IMPOK", pe+132, 20),
                           ("DELAY", pe+228, 0x20)):
        b = bytearray(cases[name])
        struct.pack_into("<I", b, off, val)
        cases[name] = bytes(b)
    cases.update(reloc_cases(folder))
    # Open Watcom C fixtures (task 165), built by c/BUILD.BAT.
    cdir = folder / "c"
    if not (cdir / "PLUGC.DLL").exists():
        raise RuntimeError(
            "c/PLUGC.DLL missing, see c/BUILD.LOG")
    cases["HOSTC"] = (cdir / "HOSTC.EXE").read_bytes()
    dll = (cdir / "PLUGC.DLL").read_bytes()
    if dll.count(b"HOSTCZEXE") != 1:
        raise RuntimeError("PLUGC.DLL import name not found")
    (folder / "PLUGC.DLL").write_bytes(
        dll.replace(b"HOSTCZEXE", b"HOSTC.EXE"))
    for name, data in cases.items():
        (folder / (name + ".PE")).write_bytes(data)
    lines = ["@echo off", "if exist RESULT.LOG del RESULT.LOG"]
    for name in cases:
        pre = 'if "%1"=="P2" ' if name in PMXONLY else ""
        lines += [pre + "R:" + chr(92) + "BIN" + chr(92)
                  + "zrdxbind -y " + name + ".PE " + name
                  + ".EXE > NUL",
                  pre + "if errorlevel 1 echo " + name
                  + " BIND_FAILED >> RESULT.LOG",
                  pre + "copy " + name + ".EXE " + name
                  + ".%1 > NUL",
                  pre + name + ".EXE >> RESULT.LOG"]
        if name in VALID:
            lines += [pre + "if errorlevel 1 echo " + name
                      + " FAILED >> RESULT.LOG",
                      pre + "if not errorlevel 1 echo " + name
                      + " OK >> RESULT.LOG"]
        else:
            lines += ["if errorlevel 255 echo " + name
                      + " OK >> RESULT.LOG",
                      "if not errorlevel 255 echo " + name
                      + " FAILED >> RESULT.LOG"]
    # Unbound run (task 170): the PE is passed to stub-pmx.exe by
    # name.
    ub = ["@echo off", "if exist RESULT.LOG del RESULT.LOG",
          "mkdir UNB > NUL",
          "copy *.DLL UNB > NUL", "set ZRDXROLE=1"]
    for name in cases:
        ub += ["copy " + name + ".PE UNB" + chr(92) + name
               + ".EXE > NUL",
               "R:" + chr(92) + "BIN" + chr(92)
               + "STUB-PMX.EXE UNB" + chr(92) + name
               + ".EXE >> RESULT.LOG"]
        if name in VALID:
            ub += ["if errorlevel 1 echo " + name
                   + " FAILED >> RESULT.LOG",
                   "if not errorlevel 1 echo " + name
                   + " OK >> RESULT.LOG"]
        else:
            ub += ["if errorlevel 255 echo " + name
                   + " OK >> RESULT.LOG",
                   "if not errorlevel 255 echo " + name
                   + " FAILED >> RESULT.LOG"]
    ub += ["set ZRDXROLE="]
    (folder / "UNB.BAT").write_bytes(("\r\n".join(ub)
                                      + "\r\n").encode("ascii"))
    # Same through the client role of zrdxdpmi.exe (task 171,
    # external host).
    un = [x.replace("R:" + chr(92) + "BIN" + chr(92)
                    + "STUB-PMX.EXE",
                    "R:" + chr(92) + "BIN" + chr(92)
                    + "ZRDXDPMI.EXE") for x in ub]
    (folder / "UNBN.BAT").write_bytes(("\r\n".join(un)
                                       + "\r\n").encode("ascii"))
    # Host role of zrdxdpmi.exe (task 172): no marker, the host
    # wraps the PE.
    uh = [x for x in un if "set ZRDXROLE" not in x]
    (folder / "UNBH.BAT").write_bytes(("\r\n".join(uh)
                                       + "\r\n").encode("ascii"))
    # DOS paths contain one backslash, regardless of host string
    # escaping.
    (folder / "RUN.BAT").write_bytes(("\r\n".join(lines)
                                      + "\r\n").encode("ascii"))
    return list(cases)


if __name__ == "__main__":
    generate(Path(__file__).parent)
