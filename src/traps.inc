        DefTrap3 DefInt, 100h, 0
        DefTrap3 DefaultExc, 16, 8
        DefTrap3 RetFromExc, 1, 6
        DefTrap3 Iret2KernelAsinc, 1, 0
        DefTrap3 PMCallbackIret, 1, 0
%ifdef VMM
        DefTrap3 Iret2KernelAsincL, 1, 0
        DefTrap3 PMCallbackIretL, 1, 0
        DefTrap3 PassupIretL, 1, 0
        DefTrap3 IretPL, 1, 0
%endif
        DefTrap3 PassupIret, 1, 0
        DefTrap3 PMRawSwitch, 1, 0
        DefTrap3 PMSaveState, 1, 0      ;must be last!
