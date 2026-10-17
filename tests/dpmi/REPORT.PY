"""Validate archived manifests and current gates, then render D09RUNS.JSN."""
import csv
import hashlib
import json
from pathlib import Path
import re
import runpy

ROOT = Path(__file__).resolve().parents[2]
CATALOG = runpy.run_path(str(ROOT / "tests/dpmi/CATALOG.PY"))


def require(ok, message):
    if not ok:
        raise ValueError(message)


def hashes(value):
    return isinstance(value, dict) and bool(value) and all(
        isinstance(v, str) and re.fullmatch(r"[0-9a-f]{64}", v) for v in value.values())


def expected_cases(kind, profile):
    if kind == "native":
        cases = set(CATALOG["NATIVE"]) | {"HOST"}
        if profile != "vcpi":
            cases |= {"DRSAVE", "A20R" if profile == "raw" else "A20X"}
        return cases
    if kind == "bound":
        return set(CATALOG["BOUND"]) | {"HOST"}
    return set()


def validate(row, data, tools):
    require(isinstance(data, dict) and data, "empty or invalid manifest")
    kind, stage = row["kind"], row["stage"]
    require(stage in ("historical", "current"), "unknown evidence stage")
    require(kind in ("native", "bound", "dosbox", "legacy", "reference"), "unknown runner kind")
    error_key = "errors" if kind == "legacy" else "failures"
    require(data.get(error_key) == [], "missing success evidence or failures")
    require(data.get("build_only", False) is False, "build-only evidence")
    require(data.get("timed_out", False) is False, "timed-out evidence")
    require(data.get("profile", "hdpmi") == row["profile"], "profile mismatch")
    if kind in ("native", "bound", "dosbox"):
        cases = data.get("cases")
        require(isinstance(cases, list) and cases and all(isinstance(c, str) for c in cases), "missing cases")
        require(len(set(cases)) == len(cases), "duplicate cases")
        require(set(cases) == set(filter(None, row["cases"].split(','))), "declared cases mismatch")
    if kind in ("native", "bound"):
        require(isinstance(data.get("qemu"), str) and data["qemu"], "missing QEMU identity")
        require(data.get("timed_out") is False, "missing watchdog outcome")
        require(data.get("native") is (kind == "native"), "runner mode mismatch")
        require(hashes(data.get("inputs")), "missing input hashes")
        require(isinstance(data.get("provider"), str) and "External DPMI 1687: absent" in data["provider"], "wrong provider")
        for service, present in (("VCPI DE00", row["profile"] == "vcpi"), ("XMS 4300", row["profile"] != "raw")):
            require(service + (": present" if present else ": absent") in data["provider"], "wrong backend")
        for case in cases:
            require(any(case + ext in data["inputs"] for ext in (".COM", ".EXE")), "case input missing: " + case)
        if kind == "native":
            require("ZRXDPMI.EXE" in data["inputs"], "missing host identity")
    elif kind == "dosbox":
        require(data.get("build_only") is False and hashes(data.get("clients")), "incomplete DOSBox result")
        require(hashes({"source": data.get("source"), "host": data.get("host")}), "missing DOSBox identity")
        require(set(data["clients"]) == set(cases), "DOSBox cases mismatch")
    elif kind == "legacy":
        require(hashes(data.get("tools")) and isinstance(data.get("provider"), str), "incomplete legacy result")
        require(all(k in data for k in ("core", "lfn", "clipboard", "supplied")), "missing legacy configuration")
    else:
        require(hashes(data.get("hosts")) and hashes(data.get("clients")) and hashes(data.get("tools")), "incomplete reference result")
        require(data.get("provider") == "HDPMI16/HDPMI32", "wrong reference provider")
    if stage == "historical":
        return
    require(kind in ("native", "bound", "legacy"), "unsupported current gate")
    require(data.get("schema") == ("legacy-1" if kind == "legacy" else "qemu-1"), "unknown current schema")
    require(data.get("completed") is True, "campaign not completed")
    require(data.get("source") == tools["source_sha256"], "stale source identity")
    if kind == "native":
        require(data["inputs"]["ZRXDPMI.EXE"] == tools["tools"]["zrxdpmi.exe"], "stale native host")
    else:
        wanted = {k: v for k, v in tools["tools"].items() if k != "zrxdpmi.exe"}
        require(data.get("tools") == wanted, "stale bound tools")
    if kind == "legacy":
        require(row["cases"] == "" and data.get("ok_lines") == 99 and data.get("lfn_passes") == 2, "incomplete legacy coverage")
        require(data.get("lfn") == "true", "LFN gate missing")
    else:
        require(set(cases) == expected_cases(kind, row["profile"]), "partial current campaign")


def generate(root=ROOT):
    tools = json.loads((root / "docs/TOOLS.JSN").read_text())
    results, gates = {}, set()
    with (root / "tests/dpmi/RUNS.TSV").open() as source:
        for row in csv.DictReader(source, delimiter="\t"):
            name = row["run"]
            require(name and name not in results, "duplicate or empty run ID")
            path = Path(row["manifest"])
            require(not path.is_absolute() and ".." not in path.parts, "manifest path must be repository-relative")
            raw = (root / path).read_bytes()
            require(hashlib.sha256(raw).hexdigest() == row["sha256"], "manifest hash mismatch")
            data = json.loads(raw)
            validate(row, data, tools)
            if row["stage"] == "current":
                key = (row["kind"], row["profile"])
                require(key not in gates, "duplicate current gate")
                gates.add(key)
            results[name] = {"manifest": path.as_posix(), "sha256": row["sha256"],
                "stage": row["stage"], "scope": row["kind"] + "/" + row["profile"] + ": " + str(len(data.get("cases", []))) + " cases",
                "notes": row["notes"], "result": data}
    required = {(kind, profile) for kind in ("native", "bound") for profile in ("raw", "xms", "vcpi")} | {("legacy", "xms")}
    require(gates == required, "current release gates are incomplete")
    text = json.dumps(results, indent=2) + "\n"
    (root / "docs/D09RUNS.JSN").write_bytes(text.replace("\n", "\r\n").encode("ascii"))
    return results


if __name__ == "__main__":
    generate()
