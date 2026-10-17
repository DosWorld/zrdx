"""Single expectation catalog for native, bound and auxiliary fixtures."""

NATIVE = {'PREF16': 'PREF ignored and invalid prefixes: ok',
 'PREF32': 'PREF ignored and invalid prefixes: ok',
 'LDT16': 'LDTAIL final slots and exhaustion: ok',
 'LDT32': 'LDTAIL final slots and exhaustion: ok',
 'OOM16': 'OOM16 moved rollback: ok',
 'OOM32': 'OOM32 moved rollback: ok',
 'ONE16': 'OOM16 moved rollback: ok',
 'ONE32': 'OOM32 moved rollback: ok',
 'TAIL16': 'OOM16 moved rollback: ok',
 'TAIL32': 'OOM32 moved rollback: ok',
 'REG16': 'REG16 service register contracts: ok',
 'REG32': 'REG32 service register contracts: ok',
 'CBK16': 'CBK16 all callback slots: ok',
 'CBK32': 'CBK32 all callback slots: ok',
 'ERR16': 'ERR16 failure preservation: ok',
 'ERR32': 'ERR32 failure preservation: ok',
 'COD16': 'COD16 code selector classes: ok',
 'COD32': 'COD32 code selector classes: ok',
 'FRT16': 'FRT16 invalid return cleanup: ok',
 'FRT32': 'FRT32 invalid return cleanup: ok',
 'IRQ16': 'IRQ16 suspended kernel EXEC: ok',
 'IRQ32': 'IRQ32 suspended kernel EXEC: ok',
 'BAS16': 'BAS16 base and register contracts: ok',
 'BAS32': 'BAS32 base and register contracts: ok',
 'BRG16': 'BRG16 real mode bridges: ok',
 'BRG32': 'BRG32 real mode bridges: ok',
 'GAT16': 'GAT16 privileged entry guards: ok',
 'GAT32': 'GAT32 privileged entry guards: ok',
 'MEM16': 'MEM16 exact free memory: ok',
 'MEM32': 'MEM32 exact free memory: ok',
 'LIM16': 'LIM16 descriptor limits and rights: ok',
 'LIM32': 'LIM32 descriptor limits and rights: ok',
 'RAW16': 'RAW16 switches and state: ok',
 'RAW32': 'RAW32 switches and state: ok',
 'KST16': 'KST16 private kernel stacks: ok',
 'KST32': 'KST32 private kernel stacks: ok',
 'NMX16': 'NMX16 actual FPU exception: ok',
 'NMX32': 'NMX32 actual FPU exception: ok',
 'FEX16': 'FEX16 fatal cleanup: ok',
 'FEX32': 'FEX32 fatal cleanup: ok',
 'DEX16': 'DEX16 default exceptions: ok',
 'DEX32': 'DEX32 default exceptions: ok',
 'NDOS16': 'NDOS16 DOS memory arrays: ok',
 'NDOS32': 'NDOS32 DOS memory arrays: ok',
 'NATL16': 'NATL16 locked events: ok',
 'NATLOCK': 'NATLOCK32 locked events: ok',
 'NATM32': 'NATM32 fixed VM bitness: ok',
 'NATMODE': 'NATMODE fixed VM bitness: ok',
 'NEST16': 'NEST16 owned cleanup: ok',
 'NEST32': 'NEST32 owned cleanup: ok',
 'NATERR': 'NATERR rejected entries: ok',
 'ENTRY16': 'ENTRY16 native initialization: ok',
 'ENTRY32': 'ENTRY32 native initialization: ok',
 'STK16': 'STKEDGE bounds and page permissions: ok',
 'STK32': 'STKEDGE bounds and page permissions: ok'}

BOUND = {'LDTAIL': 'LDTAIL final slots and exhaustion: ok',
 'OOMB0': 'OOM bound rollback: ok',
 'OOMB1': 'OOM bound rollback: ok',
 'OOMB32': 'OOM bound rollback: ok',
 'DOSSTK': 'DOSSTK active stack: ok',
 'MAPBND': 'MAPBND shared directory: ok',
 'GDTREAD': 'GDTREAD read and protection: ok',
 'PROBE': '1686 protected detection: PASS\n'
          '0A00 unknown vendor rejected: PASS\n'
          '0303/0304 callback round trip: PASS',
 'CONTROL': 'CONTROL version and virtual IF: ok',
 'DESCR': 'DESCR specific selectors: ok',
 'UNSUP': 'UNSUP unsupported dispatch: ok',
 'WATCH': 'WATCH debug services: ok',
 'DBHIT': 'DBHIT hardware watch: ok',
 'CBACK': 'CBACK callback lifecycle: ok',
 'PHYSMAP': 'PHYSMAP ranges: ok',
 'MAPEXIT': 'PHYSMAP ranges: ok\n'
            'PHYSMAP ranges: ok\n'
            'PHYSMAP ranges: ok\n'
            'PHYSMAP ranges: ok\n'
            'MAPEXIT mapping cleanup: ok',
 'CBEXIT': 'CBACK callback lifecycle: ok\n'
           'CBACK callback lifecycle: ok\n'
           'CBACK callback lifecycle: ok\n'
           'CBACK callback lifecycle: ok\n'
           'CBEXIT callback cleanup: ok'}

AUX = {'DRSAVE': 'BAS32 base and register contracts: ok\n'
           'NEST32 owned cleanup: ok\n'
           'DRSAVE upstream registers: ok',
 'A20R': 'A20RM reset flag: ok\nNATL16 locked events: ok\nA20R normal-off cleanup: ok',
 'LATENC': 'LATENC common calls: ok',
 'A20X': 'BRG16 real mode bridges: ok\n'
         'NATL16 locked events: ok\n'
         'A20X paired host ownership: ok'}
