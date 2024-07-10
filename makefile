*f.o:
	f77 -cC *.f
conts3d: *.o
	cc -o conts3d *.o -lftn -lc -lfgl -lgl -lm
