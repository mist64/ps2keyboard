; ps2keyboard by Michael Steil
;
; based on "AT-Keyboard" by İlker Fıçıcılar

port_ddr = 0  ; 6510 data direction register
port_data = 1  ; 6510 data register

bit_clk  = $08 ; tape: write
bit_data = $10 ; tape: sense

kbdbyte  = $ff ; zero page

kernal_irq_ret = $ea31

modifiers  = $fc
break_flag = $fd
upper_byte = $fe

MODIFIER_CAPS = 16
MODIFIER_WIN = 8
MODIFIER_ALT = 4
MODIFIER_CTRL = 2
MODIFIER_SHIFT = 1

.segment        "LOADADDR"
.addr   *+2
.segment "CODE"

;****************************************
; ACTIVATE
;****************************************
activate:
	sei
	lda #<irq_handler
	sta $0314
	lda #>irq_handler
	sta $0315
.if 1
	lda #1 ; set up raster IRQ
	sta $d01a
	sta $dc0d
	lda #$fa
	sta $d012
	lda $d011
	and #$7f
	sta $d011
.endif

	lda #0
	sta 2
	lda #4
	sta 3

	lda #0
	sta upper_byte
	sta break_flag
	sta modifiers

	cli
	rts

;****************************************
; IRQ HANDLER
;****************************************
irq_handler:
	jsr kbd_driver

	inc $d019 ; ack IRQ
	jmp kernal_irq_ret

hex8:
	pha
	lsr
	lsr
	lsr
	lsr
	jsr hex4
	pla
	and #15
hex4:	tax
	lda hextab,x
putchar:
	ldy #0
	sta (2),y
	inc 2
	bne :+
	inc 3
:	rts
hextab:	.byte "0123456789",1,2,3,4,5,6




;****************************************
; RECEIVE BYTE
; out: A: byte (0 = none)
;      Z: byte available
;           0: yes
;           1: no
;      C:   0: parity OK
;           1: parity error
;****************************************
receive_byte:
; test for badline-safe time window
.if 1
	ldy $d012
	cpy #243
	bcs :+
	cpy #24
	bcc :+
	lda #0 ; in badline area, fail
	sec
	rts
.endif
; set input, bus idle
:	lda port_ddr ; set CLK and DATA as input
	and #$ff-bit_clk-bit_data
	sta port_ddr ; -> bus is idle, keyboard can start sending

	lda #bit_clk+bit_data
.if 0
	ldy #32
:	cpy $d012
	beq lc08c ; end of badline-free area
.else
	ldx #10
:	dex
	beq lc08c
.endif
	bit port_data
	bne :- ; wait for CLK=0 and DATA=0 (start bit)

	lda #bit_clk
lc044:	bit port_data ; wait for CLK=1 (not ready)
	beq lc044
	ldy #9 ; 9 bits including parity
lc04a:	bit port_data
	bne lc04a ; wait for CLK=0 (ready)
	lda port_data
	and #bit_data
	cmp #bit_data
	ror kbdbyte ; save bit
	lda #bit_clk
lc058:	bit port_data
	beq lc058 ; wait for CLK=1 (not ready)
	dey
	bne lc04a
	rol kbdbyte ; get parity bit into C
lc061:	bit port_data
	bne lc061 ; wait for CLK=0 (ready)
lc065:	bit port_data
	beq lc065 ; wait for CLK=1 (not ready)
lc069:	lda port_ddr
	ora #bit_clk+bit_data
	sta port_ddr ; set CLK and DATA as output
	lda port_data
	and #$ff - bit_clk ; CLK=0
	ora #bit_data ; DATA=1
	sta port_data
	lda kbdbyte
	beq lc08b ; zero -> return
	php ; save parity
lc07c:	lsr a ; calculate parity
	bcc lc080
	iny
lc080:	cmp #0
	bne lc07c
	tya
	plp ; transmitted parity
	adc #1
	lsr a ; C=0: parity OK
	lda kbdbyte
lc08b:	rts

lc08c:	clc
	lda #0
	sta kbdbyte
	beq lc069

;****************************************
; RECEIVE SCANCODE:
; out: X: scancode high
;           0: no prefix
;           1: E0 prefix
;           2: E1 prefix
;      A: scancode low (0 = none)
;      C:   0: key down
;           1: key up
;      Z: scancode available
;           0: yes
;           1: no
;****************************************
receive_scancode:
	jsr receive_byte
	bcs rcvsc1 ; parity error
	bne rcvsc2 ; non-zero code
rcvsc1:	lda #0
	rts
rcvsc2:	cmp #$e0 ; extend prefix 1
	beq rcvsc3
	cmp #$e1 ; extend prefix 2
	bne rcvsc4
rcvsc3:	sta upper_byte
	beq receive_scancode
rcvsc4:	cmp #$f0
	bne rcvsc5
	rol break_flag ; set to 1
	bne receive_scancode ; always
rcvsc5:	pha
	lsr break_flag ; break bit into C
	ldx upper_byte
	lda #0
	sta upper_byte
	sta break_flag
	pla ; lower byte into A
	rts


;****************************************
;****************************************
kbd_driver:
	jsr receive_down_scancode_no_modifiers
	beq drv_end

	cpx #0
	bne down_ext

	tay
	lda modifiers
	beq use_tab_unshifted
	lda #MODIFIER_SHIFT
	bit modifiers
	bne use_tab_shift
	lda #MODIFIER_CTRL
	bit modifiers
	bne use_tab_ctrl
	lda #MODIFIER_ALT
	bit modifiers
	bne use_tab_alt

use_tab_unshifted:
	tya
	tax
	lda tab_unshifted,x
	jmp drvcont

use_tab_shift:
	tya
	tax
	lda tab_shift,x
	jmp drvcont

use_tab_ctrl:
	tya
	tax
	lda tab_ctrl,x
	jmp drvcont

use_tab_alt:
	tya
	tax
	lda tab_alt,x
	jmp drvcont

drvcont:
	jsr add_to_buf

down_ext:
drv_end:

	rts

;****************************************
; RECEIVE SCANCODE AFTER MODIFIERS
; * key down only
; * modifiers have been interpreted
;   and filtered
; out: X: scancode high
;           0: no prefix
;           1: E0 prefix
;           2: E1 prefix
;      A: scancode low (0 = none)
;      Z: scancode available
;           0: yes
;           1: no
;****************************************
receive_down_scancode_no_modifiers:
	jsr receive_scancode
	beq no_key

	php
	jsr check_mod
	bcc no_mod
	plp
	bcc key_down
	eor #$ff
	and modifiers
	.byte $2c
key_down:
	ora modifiers
	sta modifiers
key_up:	lda #0 ; no key to return
	rts
no_mod:	plp
	bcs key_up
no_key:	rts ; original Z is retained


check_mod:
	cmp #$11 ; left alt (0011) or right alt (E011)
	beq md_alt
	cmp #$14 ; left ctrl (0014) or right ctrl (E014)
	beq md_ctl
	cpx #0
	bne ckmod2
	cmp #$12 ; left shift (0012)
	beq md_sh
	cmp #$59 ; right shift (0059)
	beq md_sh
ckmod1:	clc
	rts
ckmod2:	cmp #$1F ; left win (001F)
	beq md_win
	cmp #$27 ; right win (0027)
	bne ckmod1
md_win:	lda #MODIFIER_WIN
	.byte $2c
md_alt:	lda #MODIFIER_ALT
	.byte $2c
md_ctl:	lda #MODIFIER_CTRL
	.byte $2c
md_sh:	lda #MODIFIER_SHIFT
	sec
	rts

;****************************************
; ADD CHAR TO KBD BUFFER
;****************************************
add_to_buf:
	pha
	lda $c6 ; length of keyboard buffer
	cmp #10
	bcs :+ ; full, ignore
	inc $c6
	tax
	pla
	sta $0277,x ; store
	rts
:	pla
	rts

PETSCII_F1 = $85
PETSCII_F2 = $89
PETSCII_F3 = $86
PETSCII_F4 = $8A
PETSCII_F5 = $87
PETSCII_F6 = $8B
PETSCII_F7 = $88
PETSCII_F8 = $8C

tab_unshifted:
	.byte $00,$00,$00,PETSCII_F5,PETSCII_F3,PETSCII_F1,PETSCII_F2,$00
	.byte $00,$00,PETSCII_F8,PETSCII_F6,PETSCII_F4,$00,$7e,$00
	.byte $00,$00,$00,$00,$00,'Q','1',$00
	.byte $00,$00,'Z','S','A','W','2',$00
	.byte $00,'C','X','D','E','4','3',$00
	.byte $00,' ','V','F','T','R','5',$00
	.byte $00,'N','B','H','G','Y','6',$00
	.byte $00,$00,'M','J','U','7','8',$00
	.byte $00,',','K','I','O','0','9',$00
	.byte $00,'.','/','L',';','P','-',$00
	.byte $00,$00,$2a,$00,'[','=',$00,$00
	.byte $00,$00,$0d,']',$00,'\',$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00
	.byte $00,'1',$00,'4','7',$00,$00,$00
	.byte '0','.','2','5','6','8',$1b,$00
	.byte $00,'+','3','-','*','9',$00,$00

tab_shift:
	.byte $00,$00,$00,PETSCII_F7,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$1b,$00
	.byte $00,$00,$00,$00,$00,$71,$21,$00
	.byte $00,$00,$7a,$73,$61,$77,$22,$00
	.byte $00,$63,$78,$64,$65,$24,$23,$00
	.byte $00,$20,$76,$66,$74,$72,$25,$00
	.byte $00,$6e,$62,$68,$67,$79,$26,$00
	.byte $00,$00,$6d,$6a,$75,$27,$28,$00
	.byte $00,$3c,$6b,$69,$6f,$30,$29,$00
	.byte $00,$3e,$3f,$6c,$3a,$70,$2d,$00
	.byte $00,$00,$5f,$00,$7b,$2b,$00,$00
	.byte $00,$00,$0d,$7d,$00,$7c,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$14,$00,$00,$00,$00,$00,$00
	.byte $00,$2b,$00,$2d,$00,$00,$00,$00

tab_ctrl:
	.byte $00,$00,$00,$00,$00,$00,$00,$ff
	.byte $00,$00,$00,$00,$00,$00,$7e,$00
	.byte $00,$00,$00,$00,$00,$11,$90,$00
	.byte $00,$00,$1a,$13,$01,$17,$05,$00
	.byte $00,$03,$18,$04,$05,$9f,$1c,$00
	.byte $00,$20,$16,$06,$14,$12,$9c,$00
	.byte $00,$0e,$02,$08,$07,$19,$1e,$00
	.byte $00,$00,$0d,$0a,$15,$1f,$9e,$00
	.byte $00,$2c,$0b,$09,$0f,$92,$12,$00
	.byte $00,$2e,$2f,$0c,$3b,$10,$2d,$00
	.byte $00,$00,$27,$00,$5b,$3d,$00,$00
	.byte $00,$00,$0d,$5d,$00,$5c,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$2b,$00,$2d,$00,$00,$00,$00

tab_alt:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$7e,$00
	.byte $00,$00,$00,$00,$00,$ab,$81,$00
	.byte $00,$00,$ad,$ae,$b0,$b3,$95,$00
	.byte $00,$bc,$bd,$ac,$b1,$97,$96,$00
	.byte $00,$20,$be,$bb,$a3,$b2,$98,$00
	.byte $00,$b6,$bf,$b4,$a5,$b7,$99,$00
	.byte $00,$00,$a7,$b5,$b8,$9a,$9b,$00
	.byte $00,$3c,$a1,$a2,$b9,$30,$29,$00
	.byte $00,$3e,$3f,$b6,$5d,$af,$7c,$00
	.byte $00,$00,$27,$00,$40,$3d,$00,$00
	.byte $00,$00,$0d,$5e,$00,$a8,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$2b,$00,$2d,$00,$00,$00,$00

; References:
; * Microsoft: "Keyboard Scan Code Specification", Revision 1.3a — March 16, 2000

; Make Code     Break Code          Key
;---------------------------------------------
; 01            F0 01               F9
; 03            F0 03               F5
; 04            F0 04               F3
; 05            F0 05               F1
; 06            F0 06               F2
; 07            F0 07               F12
; 09            F0 09               F10
; 0A            F0 0A               F8
; 0B            F0 0B               F6
; 0C            F0 0C               F4
; 0D            F0 0D               Tab
; 0E            F0 0E               `
; 11            F0 11               Left Alt
; 12            F0 12               Left Shift
; 14            F0 14               Left Ctrl
; 15            F0 15               q
; 16            F0 16               1
; 1A            F0 1A               z
; 1B            F0 1B               s
; 1C            F0 1C               a
; 1D            F0 1D               w
; 1E            F0 1E               2
; 21            F0 21               c
; 22            F0 22               x
; 23            F0 23               d
; 24            F0 24               e
; 25            F0 25               4
; 26            F0 26               3
; 29            F0 29               Spacebar
; 2A            F0 2A               v
; 2B            F0 2B               f
; 2C            F0 2C               t
; 2D            F0 2D               r
; 2E            F0 2E               5
; 31            F0 31               n
; 32            F0 32               b
; 33            F0 33               h
; 34            F0 34               g
; 35            F0 35               y
; 36            F0 36               6
; 3A            F0 3A               m
; 3B            F0 3B               j
; 3C            F0 3C               u
; 3D            F0 3D               7
; 3E            F0 3E               8
; 41            F0 41               ,
; 42            F0 42               k
; 43            F0 43               i
; 44            F0 44               o
; 45            F0 45               0
; 46            F0 46               9
; 49            F0 49               .
; 4A            F0 4A               /
; 4B            F0 4B               l
; 4C            F0 4C               ;
; 4D            F0 4D               p
; 4E            F0 4E               -
; 52            F0 52               "
; 54            F0 54               [
; 55            F0 55               =
; 58            F0 58               Caps Lock
; 59            F0 59               Right Shift
; 5A            F0 5A               Enter
; 5B            F0 5B               ]
; 5D            F0 5D               \
; 66            F0 66               Backspace
; 69            F0 69               Keypad 1
; 6B            F0 6B               Keypad 4
; 6C            F0 6C               Keypad 7
; 70            F0 70               Keypad 0
; 71            F0 71               Keypad .
; 72            F0 72               Keypad 2
; 73            F0 73               Keypad 5
; 74            F0 74               Keypad 6
; 75            F0 75               Keypad 8
; 76            F0 76               Esc
; 77            F0 77               Num Lock
; 78            F0 78               F11
; 79            F0 79               Keypad +
; 7A            F0 7A               Keypad 3
; 7B            F0 7B               Keypad -
; 7C            F0 7C               Keypad *
; 7D            F0 7D               Keypad 9
; 7E            F0 7E               Scroll Lock
; 83            F0 83               F7
; E0 11         E0 F0 11            Right Alt
; E0 14         E0 F0 14            Right Ctrl
; E0 1F         E0 F0 1F            Left Win/Super
; E0 27         E0 F0 27            Right Win/Super
; E0 4A         E0 F0 4A            Keypad /
; E0 5A         E0 F0 5A            Keypad Enter
; E0 69         E0 F0 69            End
; E0 6B         E0 F0 6B            Left Arrow
; E0 6C         E0 F0 6C            Home
; E0 70         E0 F0 70            Insert
; E0 71         E0 F0 71            Delete
; E0 72         E0 F0 72            Down Arrow
; E0 74         E0 F0 74            Right Arrow
; E0 75         E0 F0 75            Up Arrow
; E0 7A         E0 F0 7A            Page Down
; E0 7D         E0 F0 7D            Page Up
;
; *** weird ones ***
;
; 84            F0 84               Left/Right Alt + Print Screen
; E0 12 E0 7C   E0 F0 7C E0 F0 12   Print Screen
;                                   like two keys "Alt-Shift" (E0-12) and SysRq
;                                   repeat: E0 7C
; E0 7C         E0 F0 7C            SysRq [Ctrl + Print Screen]
;                                   repeat: E0 7C
; E1 14 77 E1 F0 14 F0 77           Pause (E1-14 down, Num Lock down, E1-14 up, Num Lock up)
;                                   no break code, no repeat
; E0 7E E0 F0 7E                    Ctrl + Pause (like a separate key)
;                                   no break, no repeat
;
; E0 12         E0 F0 12            non-existent key, added to some combinations for compat.
