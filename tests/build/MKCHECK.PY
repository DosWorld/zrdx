"""Check makefile object dependencies against %include lines."""
from pathlib import Path
import re
import sys

SRC = Path(__file__).resolve().parent.parent.parent / "src"
OPTIONAL = {"debugger.asm"}


def rules(text):
    text = re.sub(r"&\s*\n\s*", " ", text)
    for line in text.splitlines():
        m = re.match(r"^(\S+\.obj):\s*(.+)$", line)
        if m:
            yield m.group(1), m.group(2).split()


def includes(name, found=None):
    found = set() if found is None else found
    path = SRC / name
    if not path.exists():
        return found
    text = path.read_text()
    for inc in re.findall(r'(?m)^%include\s+"([^"]+)"', text):
        if inc not in found:
            found.add(inc)
            includes(inc, found)
    return found


def main():
    text = (SRC / "makefile").read_text().replace("\r\n", "\n")
    bad = []
    seen = 0
    for obj, deps in rules(text):
        top = deps[0]
        want = includes(top) - OPTIONAL
        have = set(deps[1:])
        seen += 1
        for name in sorted(want - have):
            bad.append("%s lacks %s" % (obj, name))
        for name in sorted(have - want):
            bad.append("%s has unused %s" % (obj, name))
    if "zrdxdpmi.obj" not in dict(rules(text)):
        bad.append("no zrdxdpmi.obj rule")
    for line in bad:
        print(line)
    print("makefile deps: %d rules, %d problems"
          % (seen, len(bad)))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
