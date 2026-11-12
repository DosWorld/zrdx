.ERASE
x.out: altest.mak
	copy ..\..\src\aligner.exe x.out
	..\..\src\aligner.exe x.out 600
