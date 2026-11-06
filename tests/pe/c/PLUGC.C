/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * Open Watcom C fixture (task 165): DLL that imports HostVal
 * from HOSTC.EXE (import library HOSTC.LIB) and exports PlugVal.
 */
extern int HostVal(void);

int PlugVal(void)
{
    return HostVal() + 35;
}
