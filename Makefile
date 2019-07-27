all:
	ca65 atkeyboard.s && ld65 -C atkeyboard.cfg atkeyboard.o -o atkeyboard.prg

clean:
	rm atkeyboard.o atkeyboard.prg
