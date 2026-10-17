"""Generate the 49-call DPMI 0.9 ledger from the supplied standard and coverage input."""
import csv
import json
import runpy
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]


def check_refs(coverage, environment, runs):
    catalog = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))
    names = set().union(*(set(catalog[k]) for k in ("NATIVE", "BOUND", "AUX")))
    names |= {p.stem.upper() for p in (ROOT / "tests/rdf").glob("*") if p.suffix.lower() == ".asm"}
    current = set().union(*(set(r["result"].get("cases", [])) for r in runs.values() if r.get("stage") == "current"))
    reference = set().union(*(set(r["result"].get("clients", {})) for r in runs.values() if r["result"].get("provider") == "HDPMI16/HDPMI32"))
    legacy = [r["result"] for r in runs.values() if r.get("stage") == "current" and r["result"].get("schema") == "legacy-1"]
    def check(value):
        for item in value.split(','):
            if item == "none":
                continue
            if item == "CHECK.PY":
                if not legacy or not any(r.get("completed") is True and r.get("errors") == [] for r in legacy):
                    raise ValueError("legacy runner lacks passing evidence")
                continue
            prefix, _, name = item.rpartition(':')
            if prefix not in ("", "QEMU", "REF", "LFN") or name not in names:
                raise ValueError("unknown fixture reference: " + item)
            if prefix == "LFN" and (name != "T40" or not any(r.get("lfn_passes") == 2 for r in legacy)):
                raise ValueError("LFN fixture lacks passing evidence")
            if name not in (reference if prefix == "REF" else current):
                raise ValueError("fixture lacks passing evidence: " + item)
    for row in coverage.values():
        for key in ("positive32", "negative32", "positive16", "negative16"):
            check(row[key])
    for row in environment:
        check(row["candidate16"])
        check(row["candidate32"])
    with (ROOT / "tests/dpmi/ASSERT.TSV").open() as source:
        assertions = list(csv.DictReader(source, delimiter="\t"))
    seen = set()
    for row in assertions:
        if row["obligation"] in seen:
            raise ValueError("duplicate assertion obligation")
        seen.add(row["obligation"])
        if row.get("kind") not in ("standard", "host-policy", "cpu-contract"):
            raise ValueError("unknown assertion contract kind")
        check(row["fixtures"])
        source = ROOT / row["source"]
        if row["anchor"] not in source.read_text():
            raise ValueError("missing assertion anchor: " + row["obligation"])
        if not row["expectation"]:
            raise ValueError("missing normative expectation")
    return assertions


def generate():
    standard = (ROOT / "docs/DPMI09.HTM").read_text(encoding="cp1252")
    sections = re.split(r'<h[23]><a name="([0-9.]+)">', standard)
    calls = []
    for index in range(1, len(sections), 2):
        section, body = sections[index:index + 2]
        if not 8 <= int(section.split(".")[0]) <= 19:
            continue
        match = re.search(r"AX = ([0-9A-Fa-f]{4})h", body)
        if match:
            title = re.sub("<[^>]+>", "", body.split("</h")[0]).strip()
            calls.append((match[1].upper(), section, title))
    definitions = {}
    for row in calls:
        if row[0] in definitions and not row[1].startswith(definitions[row[0]][1] + "."):
            raise RuntimeError("ambiguous service definition: " + row[0])
        definitions[row[0]] = row
    calls = list(definitions.values())
    if len(calls) != 49 or len(set(row[0] for row in calls)) != 49:
        raise RuntimeError("standard extraction no longer identifies 49 unique services")
    with (ROOT / "tests/dpmi/COVER.TSV").open() as source:
        coverage = {row["function"]: row for row in csv.DictReader(source, delimiter="\t")}
    if set(coverage) != set(row[0] for row in calls):
        raise RuntimeError("coverage input differs from the normative call set")
    lines = ["function\tsection\ttitle\tsource\tpositive32\tnegative32\tpositive16\tnegative16\tstatus"]
    for function, section, title in calls:
        row = coverage[function]
        if not (ROOT / "src" / row["source"]).is_file():
            raise RuntimeError("missing implementation file for " + function)
        lines.append("\t".join([function, section, title] + [row[key] for key in
                     ("source", "positive32", "negative32", "positive16", "negative16", "status")]))
    call_text = "\r\n".join(lines) + "\r\n"
    # Environment obligations are manually indexed, then converted to the
    # same ASCII/CRLF documentation format. Inputs are not test results.
    with (ROOT / "tests/dpmi/ENVCOV.TSV").open() as source:
        reader = csv.DictReader(source, delimiter="\t")
        fields = reader.fieldnames
        environment = list(reader)
    for row in environment:
        if not (ROOT / "src" / row["source"]).is_file():
            raise RuntimeError("missing environment source: " + row["requirement"])
    text = "\r\n".join(["\t".join(fields)] +
                       ["\t".join(row[key] for key in fields) for row in environment]) + "\r\n"
    runs = json.loads((ROOT / "docs/D09RUNS.JSN").read_text())
    check_refs(coverage, environment, runs)
    (ROOT / "docs/D09MAP.TSV").write_bytes(call_text.encode("ascii"))
    (ROOT / "docs/D09ENV.TSV").write_bytes(text.encode("ascii"))
    (ROOT / "docs/D09ASRT.TSV").write_bytes((ROOT / "tests/dpmi/ASSERT.TSV").read_bytes())


if __name__ == "__main__":
    generate()
