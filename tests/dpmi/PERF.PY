"""Archive and render matched common-call and software-INT virtual timings."""
import argparse
import hashlib
import json
from pathlib import Path
import runpy
import statistics
import struct

ROOT = Path(__file__).resolve().parents[2]
FUNCTIONS = ('0003', '0604', '0006', 'INT00', 'INT60')


def capture(before, after):
    fat = runpy.run_path(str(ROOT / 'tests/dpmi/FAT16.PY'))['Fat16']
    records = []
    for path in (before, after):
        result = json.loads((path / 'RESULT.JSN').read_text())
        if (result.get('failures') != [] or result.get('timed_out') is not False
                or result.get('completed') is not True or result.get('icount') is not True
                or set(result.get('cases', [])) != {'HOST', 'LATENC'}):
            raise ValueError('require a complete passing LATENC --icount campaign')
        volume = fat(path / 'TEST.IMG')
        try:
            data = volume.get('LATENC.BIN')
        finally:
            volume.file.close()
        if len(data) != len(FUNCTIONS) * 24:
            raise ValueError('expected three 64-bit samples per function')
        records.append({'manifest': result, 'sample_sha256': hashlib.sha256(data).hexdigest(),
                        'samples': list(struct.unpack('<15Q', data))})
    return {'schema': 'latency-1', 'iterations': 10000, 'functions': list(FUNCTIONS),
            'before': records[0], 'after': records[1]}


def render(data):
    if data.get('schema') != 'latency-1' or data.get('functions') != list(FUNCTIONS) or data.get('iterations') != 10000:
        raise ValueError('unknown timing schema')
    old, new = (data[k] for k in ('before', 'after'))
    a, b = old['manifest'], new['manifest']
    for key in ('profile', 'qemu', 'icount_shift', 'provider'):
        if a[key] != b[key]:
            raise ValueError('timing environment differs: ' + key)
    for record in (old, new):
        m = record['manifest']
        if (m.get('completed') is not True or m.get('failures') != [] or m.get('timed_out') is not False
                or m.get('icount') is not True or m.get('icount_shift') != 3
                or set(m.get('cases', [])) != {'HOST', 'LATENC'}):
            raise ValueError('unqualified timing record')
        samples = record['samples']
        if len(samples) != 15 or any(type(v) is not int or v <= 0 for v in samples):
            raise ValueError('invalid timing samples')
        if hashlib.sha256(struct.pack('<15Q', *samples)).hexdigest() != record['sample_sha256']:
            raise ValueError('timing sample hash mismatch')
    inputs = [{k: v for k, v in m['inputs'].items() if k != 'ZRXDPMI.EXE'} for m in (a, b)]
    if inputs[0] != inputs[1]:
        raise ValueError('timing clients or DOS inputs differ')
    lines = ['function\titerations\tbefore_median\tafter_median\tratio\tunit']
    for i, function in enumerate(FUNCTIONS):
        x, y = [statistics.median(r['samples'][i*3:i*3+3]) / data['iterations'] for r in (old, new)]
        lines.append('\t'.join((function, '10000', f'{x:.4f}', f'{y:.4f}', f'{y/x:.6f}',
                                'virtual TSC ticks per iteration; icount shift=3; includes loop overhead')))
    for label, m in (('before', a), ('after', b)):
        for name in ('ZRXDPMI.EXE', 'LATENC.COM'):
            lines.append('\t'.join((label, name, m['inputs'][name])))
    return '\r\n'.join(lines) + '\r\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--before', type=Path)
    parser.add_argument('--after', type=Path)
    parser.add_argument('--input', type=Path, default=ROOT / 'docs/EVID/PERF39.JSN')
    args = parser.parse_args()
    if bool(args.before) != bool(args.after):
        raise ValueError('both timing runs are required')
    data = capture(args.before, args.after) if args.before else json.loads(args.input.read_text())
    output = render(data)
    if args.before:
        args.input.write_bytes((json.dumps(data, indent=2) + '\n').replace('\n', '\r\n').encode('ascii'))
    (ROOT / 'docs/D09PERF.TSV').write_bytes(output.encode('ascii'))


if __name__ == '__main__':
    main()
