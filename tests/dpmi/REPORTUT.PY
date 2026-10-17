"""Negative evidence/schema and coverage-reference regression tests."""
import copy
import csv
import hashlib
import json
from pathlib import Path
import runpy
import tempfile
import struct
import unittest

ROOT = Path(__file__).resolve().parents[2]
R = runpy.run_path(str(ROOT / "tests/dpmi/REPORT.PY"))
H = "a" * 64
TOOLS = {"source_sha256": H, "tools": {"zrxdpmi.exe": H, "stub-le.exe": H}}


def fixture():
    names = sorted(R["expected_cases"]("native", "raw"))
    data = {"schema": "qemu-1", "completed": True, "source": H,
            "profile": "raw", "native": True, "qemu": "QEMU test",
            "timed_out": False, "failures": [], "cases": names,
            "provider": "External DPMI 1687: absent\nVCPI DE00: absent\nXMS 4300: absent",
            "inputs": {n + ".COM": H for n in names}}
    data["inputs"]["ZRXDPMI.EXE"] = H
    row = {"run": "test", "kind": "native", "profile": "raw", "stage": "current", "cases": ','.join(names), "notes": ""}
    return row, data


class Evidence(unittest.TestCase):
    def test_complete(self):
        row, data = fixture()
        R["validate"](row, data, TOOLS)

    def test_missing_and_false_success(self):
        row, data = fixture()
        for value in ({}, [], {"failures": []}):
            with self.subTest(value=value), self.assertRaises(ValueError):
                R["validate"](row, value, TOOLS)
        for key in ("failures", "timed_out", "completed", "source", "inputs", "cases", "schema", "provider"):
            changed = copy.deepcopy(data)
            del changed[key]
            with self.subTest(key=key), self.assertRaises(ValueError):
                R["validate"](row, changed, TOOLS)

    def test_failure_and_stale_identity(self):
        row, data = fixture()
        for key, value in (("failures", ["failed"]), ("timed_out", True), ("build_only", True),
                           ("completed", False), ("schema", "unknown"), ("source", "b"*64), ("profile", "xms")):
            changed = copy.deepcopy(data)
            changed[key] = value
            with self.subTest(key=key), self.assertRaises(ValueError):
                R["validate"](row, changed, TOOLS)
        data["inputs"]["ZRXDPMI.EXE"] = "b"*64
        with self.assertRaises(ValueError):
            R["validate"](row, data, TOOLS)

    def test_partial_even_with_matching_label(self):
        row, data = fixture()
        data["cases"] = data["cases"][:-1]
        row["cases"] = ','.join(data["cases"])
        with self.assertRaises(ValueError):
            R["validate"](row, data, TOOLS)

    def test_duplicate_cases(self):
        row, data = fixture()
        data["cases"] += [data["cases"][0]]
        with self.assertRaises(ValueError):
            R["validate"](row, data, TOOLS)

    def test_index_and_hash_rejection(self):
        row, data = fixture()
        row["stage"] = "historical"
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / 'docs').mkdir()
            (root / 'tests/dpmi').mkdir(parents=True)
            (root / 'docs/TOOLS.JSN').write_text(json.dumps(TOOLS))
            raw = json.dumps(data).encode()
            (root / 'docs/RUN.JSN').write_bytes(raw)
            row.update(manifest='docs/RUN.JSN', sha256=hashlib.sha256(raw).hexdigest())
            def index(rows):
                with (root / 'tests/dpmi/RUNS.TSV').open('w') as out:
                    writer = csv.DictWriter(out, fieldnames=list(row), delimiter='\t')
                    writer.writeheader()
                    writer.writerows(rows)
            index([row, row])
            with self.assertRaisesRegex(ValueError, 'duplicate'):
                R['generate'](root)
            index([row])
            with self.assertRaisesRegex(ValueError, 'incomplete'):
                R['generate'](root)
            (root / 'docs/RUN.JSN').write_bytes(b'{}')
            with self.assertRaisesRegex(ValueError, 'hash'):
                R['generate'](root)
            raw = b'{'
            (root / 'docs/RUN.JSN').write_bytes(raw)
            row['sha256'] = hashlib.sha256(raw).hexdigest()
            index([row])
            with self.assertRaises(json.JSONDecodeError):
                R['generate'](root)
            row['manifest'] = '/private/tmp/not-portable.json'
            index([row])
            with self.assertRaisesRegex(ValueError, 'relative'):
                R['generate'](root)

    def test_archived_formats(self):
        with (ROOT / 'tests/dpmi/RUNS.TSV').open() as source:
            for row in csv.DictReader(source, delimiter='\t'):
                if row['stage'] == 'historical':
                    with self.subTest(run=row['run']):
                        R['validate'](row, json.loads((ROOT / row['manifest']).read_text()), TOOLS)

    def test_unknown_coverage(self):
        ledger = runpy.run_path(str(ROOT / 'tests/dpmi/LEDGER.PY'))
        with self.assertRaisesRegex(ValueError, 'unknown fixture'):
            ledger['check_refs']({'0000': dict.fromkeys(('positive32','negative32','positive16','negative16'), 'MISSING')}, [], {})
        with self.assertRaisesRegex(ValueError, 'lacks passing'):
            ledger['check_refs']({'0000': dict.fromkeys(('positive32','negative32','positive16','negative16'), 'NDOS16')}, [], {})


class Timing(unittest.TestCase):
    def setUp(self):
        self.perf = runpy.run_path(str(ROOT / 'tests/dpmi/PERF.PY'))
        samples = [10000] * 15
        manifest = dict(completed=True, failures=[], timed_out=False,
                        cases=['HOST', 'LATENC'], icount=True, icount_shift=3,
                        profile='raw', qemu='QEMU test', provider='raw',
                        inputs={'LATENC.COM': H, 'ZRXDPMI.EXE': H})
        record = dict(manifest=manifest, samples=samples,
                      sample_sha256=hashlib.sha256(struct.pack('<15Q', *samples)).hexdigest())
        self.data = dict(schema='latency-1', iterations=10000,
                         functions=list(self.perf['FUNCTIONS']),
                         before=copy.deepcopy(record), after=copy.deepcopy(record))

    def test_matched(self):
        self.assertIn('INT00', self.perf['render'](self.data))

    def test_environment_and_inputs(self):
        for key, value in [('qemu', 'other'), ('profile', 'xms'), ('icount_shift', 4),
                           ('completed', False), ('timed_out', True), ('failures', ['fail'])]:
            changed = copy.deepcopy(self.data)
            changed['after']['manifest'][key] = value
            with self.subTest(key=key), self.assertRaises(ValueError):
                self.perf['render'](changed)
        self.data['after']['manifest']['inputs']['LATENC.COM'] = 'b' * 64
        with self.assertRaises(ValueError):
            self.perf['render'](self.data)

    def test_samples(self):
        for samples in ([], [0]*15, [-1]*15, [True]*15, [20000]*15):
            changed = copy.deepcopy(self.data)
            changed['after']['samples'] = samples
            with self.subTest(samples=samples), self.assertRaises(ValueError):
                self.perf['render'](changed)


if __name__ == '__main__':
    unittest.main()
