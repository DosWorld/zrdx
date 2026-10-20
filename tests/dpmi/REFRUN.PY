"""Compare portable native-entry/bridge/descriptor fixtures with HDPMI."""
import argparse
from pathlib import Path
import hashlib,json,runpy,shutil
root=Path(__file__).resolve().parents[2]
env=runpy.run_path(str(root/'tests/CHECK.PY'))
cfg=env['CFG']
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--work',required=True,type=Path)
parser.add_argument('--fixtures',required=True,type=Path)
parser.add_argument('--bound',type=Path,help='qualified CHECK work tree for bound LE/PE/RDF comparison')
args=parser.parse_args()
w=args.work.resolve()
cfg['check_scratch'](w)
w.mkdir(parents=True,exist_ok=True)
for width in (16,32):
 for name in ('ENTRY','BRG','LIM'):
  shutil.copyfile(args.fixtures/(name+str(width)+'.COM'),w/(name+str(width)+'.COM'))
shutil.copyfile(args.fixtures/'HOST.COM',w/'HOST.COM')
env['config'](w,'xms',False,core='normal')
b=['@echo off'];expected={}
if args.bound:
 for n in env['TOOLS']:shutil.copyfile(args.bound/'bin'/n,w/n)
 for source,target in [('tests/rdf/T02.RDO','LE.RDO'),('tests/rdf/T02.exe','RDF.EXE'),('tests/pe/GOOD.EXE','PE.EXE'),('tests/dpmi/PROBE.EXE','PROBE.EXE')]:
  shutil.copyfile(args.bound/source,w/target)
 b += ['rlink le /zrdx /o=LE.EXE LE.RDO > LE.BLD','if errorlevel 1 goto bad','ZRXBIND -r -n -y LE.EXE >> LE.BLD','if errorlevel 1 goto bad','ZRXSETUP /B0 LE.EXE >> LE.BLD','if errorlevel 1 goto bad','ZRXSETUP /B0 PE.EXE > PE.BLD','if errorlevel 1 goto bad']

for width in (16,32):
 b += [r'C:\HX\BIN\HDPMI'+str(width)+'.EXE -r > HDP'+str(width)+'.LOG','HOST.COM > HOST'+str(width)+'.LOG']
 for name,label in [('ENTRY','native initialization'),('BRG','real mode bridges'),('LIM','descriptor limits and rights')]:
  n=name+str(width);expected[n]=n+' '+label+': ok';b += [n+'.COM > '+n+'.LOG','if errorlevel 1 goto f'+n,'echo 0 > '+n+'.STS','goto d'+n,':f'+n,'echo 1 > '+n+'.STS',':d'+n]
 b += [r'C:\HX\BIN\HDPMI'+str(width)+'.EXE -u > UN'+str(width)+'.LOG']
if args.bound:
 b += [r'C:\HX\BIN\HDPMI32.EXE -r > BOUND.LOG','HOST.COM > BHOST.LOG']
 for n,code,output in [('LE',42,''),('RDF',42,''),('PE',0,'PE single module: ok'),('PROBE',0,'1686 protected detection: PASS\n0A00 unknown vendor rejected: PASS\n0303/0304 callback round trip: PASS')]:
  expected[n]=output
  b += [n+'.EXE > '+n+'.LOG','if errorlevel '+str(code+1)+' goto f'+n]
  if code:b += ['if not errorlevel '+str(code)+' goto f'+n]
  b += ['echo 0 > '+n+'.STS','goto d'+n,':f'+n,'echo 1 > '+n+'.STS',':d'+n]
 b += [r'C:\HX\BIN\HDPMI32.EXE -u > BUN.LOG']
b+=['HOST.COM > AFTER.LOG','echo OK > STATUS.LOG','goto end',':bad','echo FAILED > STATUS.LOG',':end']
env['dos'](w,'\n'.join(b)+'\n',90)
fail=[]
for n,e in expected.items():
 if (w/(n+'.STS')).read_text().strip()!='0' or (w/(n+'.LOG')).read_text().strip()!=e:fail.append(n)
for width in (16,32):
 if 'External DPMI 1687: present' not in (w/('HOST'+str(width)+'.LOG')).read_text():fail.append('HOST'+str(width))
if args.bound and 'External DPMI 1687: present' not in (w/'BHOST.LOG').read_text():fail.append('bound provider')
if 'External DPMI 1687: absent' not in (w/'AFTER.LOG').read_text():fail.append('provider after unload')
r={'provider':'HDPMI16/HDPMI32','core':'normal','failures':fail,'clients':{n:hashlib.sha256((w/(n+('.EXE' if n in ('LE','RDF','PE','PROBE') else '.COM'))).read_bytes()).hexdigest() for n in expected},'hosts':{str(n):hashlib.sha256((cfg['require']('ZRDX_DRIVE_C')/'HX/BIN'/('HDPMI'+str(n)+'.EXE')).read_bytes()).hexdigest() for n in (16,32)}}
if args.bound:r['tools']={n:hashlib.sha256((w/n).read_bytes()).hexdigest() for n in env['TOOLS']}
env['write'](w/'RESULT.JSN' ,json.dumps(r,indent=2)+'\n');print(r)

raise SystemExit(bool(fail))
