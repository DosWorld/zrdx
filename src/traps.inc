        DefTrap3 DefInt, 100h, 0
        DefTrap3 DefaultExc, 16, 8
        DefTrap3 RetFromExc, 1, 6
        DefTrap3 Iret2KernelAsinc, 1, 0
        DefTrap3 PMCallbackIret, 1, 0
        DefTrap3 PassupIret, 1, 0
        DefTrap3 PMRawSwitch, 1, 0
%ifdef NativeHost
        DefTrap3 NativeEventIret, 1, 6
%endif
%ifdef VENDORON
%ifndef VENDORPE
        DefTrap3 Vendor, 2, 0
%endif
%endif
        DefTrap3 PMSaveState, 1, 0      ;must be last!
