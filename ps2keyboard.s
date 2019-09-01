; ps2keyboard by Michael Steil

; This code is configured for a PS/2 (or AT) keyboard connected
; to the C64 tape port. As long as bit_data > bit_clk, the following
; four constants can be changed without changing the code.
;
port_ddr = 0  ; 6510 data direction register
port_data = 1  ; 6510 data register
;
               ; TAPE PIN A (GND)   <---> PS/2 PIN 3 (GND)  [AT PIN 4]
               ; TAPE PIN B (VCC)   <---> PS/2 PIN 4 (VCC)  [AT PIN 5]
bit_clk  = $08 ; TAPE PIN E (write) <---> PS/2 PIN 5 (CLK)  [AT PIN 1]
bit_data = $10 ; TAPE PIN F (sense) <---> PS/2 PIN 1 (DATA) [AT PIN 2]

kbdbyte    = $fc ; zero page
prefix     = $fd
break_flag = $fe
shflag     = $ff   ; this is compatible with C64/C128 "shflag"
MODIFIER_SHIFT = 1 ; C64:  Shift
MODIFIER_ALT   = 2 ; C64:  Commodore
MODIFIER_CTRL  = 4 ; C64:  Ctrl
MODIFIER_WIN   = 8 ; C128: Alt
MODIFIER_CAPS  = 16; C128: Caps

; skip UDTIM call at the beginning of the IRQ,
; otherwise the STOP key doesn't work
HACK_STOP = 1

.segment        "LOADADDR"
.addr   *+2
.segment "CODE"

;****************************************
; ACTIVATE
;****************************************
activate:
	sei
	lda $0314
.if HACK_STOP
	clc
	adc #3
.endif
	sta jmp_kernal_irq_ret + 1
	lda $0315
.if HACK_STOP
	adc #0
.endif
	sta jmp_kernal_irq_ret + 2
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
	sta prefix
	sta break_flag
	sta shflag

	cli
	rts

;****************************************
; IRQ HANDLER
;****************************************
irq_handler:
	inc $d019 ; ack IRQ
.if HACK_STOP
	jsr $ffea       ;update jiffy clock
.endif
	jsr kbd_driver
jmp_kernal_irq_ret:
	jmp $ffff

;****************************************
; RECEIVE BYTE
; out: A: byte (0 = none)
;      Z: byte available
;           0: yes
;           1: no
;      C:   0: parity OK
;           1: parity error
;****************************************
; The byte receive function is based on
; "AT-Keyboard" by İlker Fıçıcılar
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
.if 1
	ldy #32
:	cpy $d012
	beq lc08c ; end of badline-free area
.else
	ldy #10
:	dey
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
; out: X: prefix (E0, E1; 0 = none)
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
rcvsc3:	sta prefix
	beq receive_scancode ; always
rcvsc4:	cmp #$f0
	bne rcvsc5
	rol break_flag ; set to 1
	bne receive_scancode ; always
rcvsc5:	pha
	lsr break_flag ; break bit into C
	ldx prefix
	lda #0
	sta prefix
	sta break_flag
	pla ; lower byte into A
	rts

;****************************************
; RECEIVE SCANCODE AFTER shflag
; * key down only
; * modifiers have been interpreted
;   and filtered
; out: X: prefix (E0, E1; 0 = none)
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
	and shflag
	.byte $2c
key_down:
	ora shflag
	sta shflag
key_up:	lda #0 ; no key to return
	rts
no_mod:	plp
	bcs key_up
no_key:	rts ; original Z is retained

; XXX handle caps lock

check_mod:
	cpx #$e1
	beq ckmod1
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
;****************************************
kbd_driver:
	jsr receive_down_scancode_no_modifiers
	beq drv_end

	tay

	cpx #0
	bne down_ext
; *** regular scancodes
	cmp #$83 ; convert weird f7 scancode
	bne not_f7
	lda #$02 ; this one is unused
	tay
not_f7:

	cmp #$0e ; scancodes < $0E and > $68 are independent of modifiers
	bcc is_unshifted
	cmp #$68
	bcc not_numpad
is_unshifted:
	ldx #3 * 2
	bne bit_found ; use unshifted table

not_numpad:
	ldx #0
	lda shflag
find_bit:
	lsr
	bcs bit_found
	inx
	inx
	cpx #3 * 2
	bne find_bit

bit_found:
	lda tables,x
	sta 2
	lda tables + 1,x
	sta 3
	lda (2),y
drv2:	beq drv_end
	jmp add_to_buf

down_ext:
	cpx #$e1 ; prefix $E1 -> E1-14 = Pause/Break
	beq is_stop
	cmp #$4a ; Numpad /
	bne not_4a
	lda #'/'
	bne add_to_buf
not_4a:	cmp #$5a ; Numpad Enter
	beq is_enter
	cpy #$6c ; special case shift+home = clr
	beq is_home
not_5a: cmp #$68
	bcc drv_end
	cmp #$80
	bcs drv_end
nhome:	lda tab_extended-$68,y
	bne add_to_buf
drv_end:
	rts

; or $80 if shift is down
is_home:
	ldx #$13 * 2; home (-> clr)
	.byte $2c
is_enter:
	ldx #$0d * 2 ; return (-> shift+return)
	.byte $2c
is_stop:
	ldx #$03 * 2 ; stop (-> run)
	lda shflag
	lsr ; shift -> C
	txa
	ror
; passes into add_to_buf

;****************************************
; ADD CHAR TO KBD BUFFER
;****************************************
add_to_buf:
	pha
	lda $c6 ; length of keyboard buffer
	cmp #10
	bcs add2 ; full, ignore
	inc $c6
	tax
	pla
	sta $0277,x ; store
	cmp #3 ; stop
	bne add1
	lda #$7f
	.byte $2c
add1:	lda #$ff
	sta $91
	rts
add2:	pla
	rts

tables:
	.word tab_shift-13, tab_alt-13, tab_ctrl-13, tab_unshifted

tab_unshifted:
	.byte $00,$00,$88,$87,$86,$85,$89,$00
	.byte $00,$00,$8c,$8b,$8a

	.byte                     $09,'_',$00
	.byte $00,$00,$00,$00,$00,'Q','1',$00
	.byte $00,$00,'Z','S','A','W','2',$00
	.byte $00,'C','X','D','E','4','3',$00
	.byte $00,' ','V','F','T','R','5',$00
	.byte $00,'N','B','H','G','Y','6',$00
	.byte $00,$00,'M','J','U','7','8',$00
	.byte $00,',','K','I','O','0','9',$00
	.byte $00,'.','/','L',';','P','-',$00
	.byte $00,$00,$27,$00,'[','=',$00,$00
	.byte $00,$00,$0d,']',$00,'\',$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00

	.byte $00,'1',$00,'4','7',$00,$00,$00
	.byte '0','.','2','5','6','8',$1b,$00
	.byte $00,'+','3','-','*','9',$00,$00

tab_shift:
	.byte                     $18,$7e,$00
	.byte $00,$00,$00,$00,$00,'Q'+$80,'!',$00,$00
	.byte $00,'Z'+$80,'S'+$80,'A'+$80,'W'+$80,'@',$00
	.byte $00,'C'+$80,'X'+$80,'D'+$80,'E'+$80,'$','#',$00
	.byte $00,$a0,'V'+$80,'F'+$80,'T'+$80,'R'+$80,'%',$00
	.byte $00,'N'+$80,'B'+$80,'H'+$80,'G'+$80,'Y'+$80,'^',$00
	.byte $00,$00,'M'+$80,'J'+$80,'U'+$80,'&','*',$00
	.byte $00,'<','K'+$80,'I'+$80,'O'+$80,')','(',$00
	.byte $00,'>','?','L'+$80,':','P'+$80,$DD,$00
	.byte $00,$00,'"',$00,'{','+',$00,$00
	.byte $00,$00,$8d,'}',$00,$a9,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$94,$00

tab_alt:
	.byte                     $18,$7e,$00
	.byte $00,$00,$00,$00,$00,$ab,$81,$00
	.byte $00,$00,$ad,$ae,$b0,$b3,$95,$00
	.byte $00,$bc,$bd,$ac,$b1,$97,$96,$00
	.byte $00,$a0,$be,$bb,$a3,$b2,$98,$00
	.byte $00,$aa,$bf,$b4,$a5,$b7,$99,$00
	.byte $00,$00,$a7,$b5,$b8,$9a,$9b,$00
	.byte $00,$3c,$a1,$a2,$b9,$30,$29,$00
	.byte $00,$3e,$3f,$b6,':',$af,$dc,$00
	.byte $00,$00,'"',$00,$00,$3d,$00,$00
	.byte $00,$00,$8d,$00,$00,$a8,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$94,$00

tab_ctrl:
	.byte                     $18,$06,$00
	.byte $00,$00,$00,$00,$00,$11,$90,$00
	.byte $00,$00,$1a,$13,$01,$17,$05,$00
	.byte $00,$03,$18,$04,$05,$9f,$1c,$00
	.byte $00,$00,$16,$06,$14,$12,$9c,$00
	.byte $00,$0e,$02,$08,$07,$19,$1e,$00
	.byte $00,$00,$0d,$0a,$15,$1f,$9e,$00
	.byte $00,$00,$0b,$09,$0f,$92,$12,$00
	.byte $00,$00,$00,$0c,$1d,$10,$00,$00
	.byte $00,$00,$00,$00,$00,$1f,$00,$00
	.byte $00,$00,$00,$00,$00,$1c,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00

tab_extended:
	;         end      lf hom
	.byte $00,$00,$00,$9d,$00,$00,$00,$00 ; @$68 (HOME is special cased)
	;     ins del  dn      rt  up
	.byte $94,$14,$11,$00,$1d,$91,$00,$00 ; @$70
	;             pgd         pgu brk
	.byte $00,$00,$00,$00,$00,$00,$03,$00 ; @$78

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
; 52            F0 52               '
; 54            F0 54               [
; 55            F0 55               =
; 58            F0 58               Caps Lock
; 59            F0 59               Right Shift
; 5A            F0 5A               Enter
; 5B            F0 5B               ]
; 5D            F0 5D               \
; 66            F0 66               Backspace
; 69            F0 69               Numpad 1
; 6B            F0 6B               Numpad 4
; 6C            F0 6C               Numpad 7
; 70            F0 70               Numpad 0
; 71            F0 71               Numpad .
; 72            F0 72               Numpad 2
; 73            F0 73               Numpad 5
; 74            F0 74               Numpad 6
; 75            F0 75               Numpad 8
; 76            F0 76               Esc
; 77            F0 77               Num Lock
; 78            F0 78               F11
; 79            F0 79               Numpad +
; 7A            F0 7A               Numpad 3
; 7B            F0 7B               Numpad -
; 7C            F0 7C               Numpad *
; 7D            F0 7D               Numpad 9
; 7E            F0 7E               Scroll Lock
; 83            F0 83               F7
; E0 11         E0 F0 11            Right Alt
; E0 14         E0 F0 14            Right Ctrl
; E0 1F         E0 F0 1F            Left Win/Super
; E0 27         E0 F0 27            Right Win/Super
; E0 4A         E0 F0 4A            Numpad /
; E0 5A         E0 F0 5A            Numpad Enter
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
