all: ps2keyboard

ps2keyboard:
	ca65 ps2keyboard.s && ld65 -C atkeyboard.cfg ps2keyboard.o -o ps2keyboard.prg

atkeyboard:
	ca65 atkeyboard.s && ld65 -C atkeyboard.cfg atkeyboard.o -o atkeyboard.prg

clean:
	rm atkeyboard.o atkeyboard.prg ps2keyboard.o ps2keyboard.prg
