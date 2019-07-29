; AT-Keyboard by İlker Fıçıcılar, 1997
;
; reverse-engineered by Michael Steil <mist64@mac.com>, 2019

port_ddr = 0  ; 6510 data direction register
port_data = 1  ; 6510 data register

bit_clk  = $08 ; tape: write
bit_data = $10 ; tape: sense

kbdbyte  = $ff ; zero page

kernal_irq_ret = $ea31

MODIFIER_CAPS = 8
MODIFIER_ALT = 4
MODIFIER_CTRL = 2
MODIFIER_SHIFT = 1

EXTENDED_PREFIX       = $e0
PAUSE_PREFIX          = $e1
BREAK_PREFIX          = $f0
EXTENDED_BREAK_PREFIX = $f1
; The break prefix for extended codes sent by the keuboard is $e0 $f0.
; This code stores $f1 into the last_prefix byte.

PETSCII_F1 = $85
PETSCII_F2 = $89
PETSCII_F3 = $86
PETSCII_F4 = $8A
PETSCII_F5 = $87
PETSCII_F6 = $8B
PETSCII_F7 = $88
PETSCII_F8 = $8C

.segment        "LOADADDR"
.addr   *+2
.segment "CODE"

j_receive_byte:
	jmp receive_byte

j_decode_regular_key_down:
	jmp decode_regular_key_down

j_decode_regular_key_up:
	jmp decode_regular_key_up

j_decode_extended_key_down:
	jmp decode_extended_key_down

j_decode_extended_key_up:
	jmp decode_extended_key_up

j_add_to_buf:
	jmp add_to_buf

irq_code_jmp:
	jmp irq_code

; SYS 49173
	jmp activate
; SYS 49176
	jmp deactivate

	.byte $4c ; XXX
irq_ptr:
	.addr irq_handler

	nop ; XXX
	nop ; XXX

;****************************************
; RECEIVE BYTE
;****************************************
receive_byte:
; test for badline-safe time window
	ldy $d012
	cpy #243
	bcs :+
	cpy #24
	bcc :+
	lda #0 ; in badline area, fail
	sec
	rts
; set input, bus idle
:	lda port_ddr ; set CLK and DATA as input
	and #$ff-bit_clk-bit_data
	sta port_ddr ; -> bus is idle, keyboard can start sending

	lda #bit_clk+bit_data
	ldy #32
:	cpy $d012
	beq lc08c ; end of badline-free area
	bit port_data
	bne :- ; wait for CLK=0 and DATA=0 (start bit)

	lda #bit_clk
lc044:	bit port_data ; wait for CLK=1 (not ready)
	beq lc044
	ldy #9 ; 9 bits including parity
lc04a:	bit port_data
	bne lc04a ; wait for CLK=0 (ready)
	lda port_data
	and #$10
	cmp #$10
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
	plp ; transmittedparity
	adc #1
	lsr a ; C=0: parity OK
	lda kbdbyte
lc08b:	rts

lc08c:	clc
	lda #0
	sta kbdbyte
	beq lc069

;****************************************
; REGULAR KEY DOWN
;****************************************
; * simple code:   decode into PETSCII and return in .A
; * modifier code: update modifier bitfield, return 0
; * prefix code:   store for later processing, return 0
decode_regular_key_down:
	cmp #EXTENDED_PREFIX
	beq store_prefix
	cmp #BREAK_PREFIX
	beq store_prefix
	cmp #PAUSE_PREFIX
	beq store_prefix
	cmp #$12 ; left shift
	beq shift_down
	cmp #$59 ; right shift
	beq shift_down
	cmp #$14 ; left ctrl
	beq ctrl_down
	cmp #$83 ; f7
	beq key_f7
	cmp #$11 ; left alt
	bne is_regular_character
; alt down
	lda #MODIFIER_ALT
	.byte $2c
ctrl_down:
	lda #MODIFIER_CTRL
	.byte $2c
shift_down:
	lda #MODIFIER_SHIFT
	ora modifier
	and #$0f
	sta modifier
	lda #0
	.byte $2c
key_f7:
	lda #PETSCII_F7
	pha
	lda #0
	sta last_code
	sta last_prefix
	pla
	clc ; OK
	rts

store_prefix:
	sta last_prefix
lc0d7:	lda #0
	sta last_code
	clc ; OK
	rts

;
; look up a regular character from one of the four tables
;
is_regular_character:
	cmp #$80
	bcs lc0d7 ; ignore unknown code
	tay
	lda #0
	sta last_code
	sta last_prefix
	lda modifier
	bne lc0f5
lc0f0:	lda tab_unshifted,y
	clc ; OK
	rts
lc0f5:	cmp #MODIFIER_CAPS + MODIFIER_SHIFT
	beq lc0f0 ; shift cancels caps
	cmp #MODIFIER_CTRL
	bne lc102 ; not ctrl
	lda tab_ctrl,y
	clc ; OK
	rts
lc102:	cmp #MODIFIER_ALT ; alt
	bne lc10b
	lda tab_alt,y
	clc ; OK
	rts
lc10b:	and #MODIFIER_CAPS + MODIFIER_SHIFT
	beq lc0d7 ; ignore
	lda tab_shift,y
	clc ; OK
	rts

;****************************************
; REGULAR KEY UP
;****************************************
; key up handler
; * simple code:   ignore, return 0
; * pause:         ignore, return 0
; * modifier code: update modifier bitfield, return 0
decode_regular_key_up:
	cmp #PAUSE_PREFIX
	beq handle_pause_up
	cmp #$58 ; caps lock
	beq toggle_caps
	cmp #$7e ; scroll lock
	beq scroll_lock
	cmp #$12 ; left shift
	beq handle_shift_up
	cmp #$59 ; right shift
	beq handle_shift_up
	cmp #$14 ; right ctrl
	beq handle_ctrl_up
	cmp #$11 ; left alt
	bne lc13e ; unknown -> ignore
; left alt
	lda #$0f - MODIFIER_ALT
	.byte $2c
handle_ctrl_up:
	lda #$0f - MODIFIER_CTRL
	.byte $2c
handle_shift_up:
	lda #$0f - MODIFIER_SHIFT
	and modifier
	sta modifier
lc13e:	lda #0
	clc
	rts

; skip 7 bytes
handle_pause_up:
	lda #0
	.byte $2c
	lda #3 ; XXX unused
	pha
	txa
	pha
	ldx #7
lc14c:	jsr j_receive_byte
	beq lc14c
	dex
	bne lc14c ; skip 7 non-empty bytes
	pla
	tax
	pla
	clc
	rts

toggle_caps:
	lda modifier
	eor #8
	sta modifier
	nop ; XXX
	nop ; XXX
	nop ; XXX
	lda #0
	clc ; OK
	rts

scroll_lock:
	lda scroll_lock_state
	eor #$80
	sta scroll_lock_state
	nop ; XXX
	nop ; XXX
	nop ; XXX
	lda #$13 ; XOFF (PETSCII: HOME)
	ldy scroll_lock_state
	bmi :+
	lda #$11 ; XON (PETSCII: CSR DOWN)
:	clc
	rts

;****************************************
; EXTENDED KEY DOWN
;****************************************
decode_extended_key_down:
	cmp #$7e ; ctrl+pause
	beq ctrl_pause
	cmp #BREAK_PREFIX
	beq handle_break
; The extended codes are $11/$12/$14/$4A/$5A and $69-$7D,
; so we check for the first set one by one, and then we use
; a table for the second set.
	cmp #$4a; keypad '/'
	beq key_slash
	cmp #$5a ; keypad enter
	beq key_enter
	cmp #$12
	beq handle_printscreen
	cmp #$14 ; right ctrl
	beq handle_rctrl
	cmp #$11 ; right alt
	bne lc1da
; right alt
	lda #MODIFIER_ALT
	.byte $2c
handle_rctrl:
	lda #MODIFIER_CTRL
	ora modifier
	sta modifier
lc1a5:	lda #0
	.byte $2c
key_enter:
	lda #$0d ; RETURN
	.byte $2c
key_slash:
	lda #$2f ; '/'
	clc ; OK
	rts

ctrl_pause: ; skip 6 non-empty bytes
	txa
	pha
	ldx #6
lc1b3:	jsr j_receive_byte
	beq lc1b3
	dex
	bne lc1b3
	pla
	tax
	lda #$ff
	rts

handle_break:
	lda #EXTENDED_BREAK_PREFIX
	sta last_prefix
	lda #0
	sta last_code
	clc
	rts

handle_printscreen: ; skip two bytes
	jsr j_receive_byte
	beq handle_printscreen
lc1d1:	jsr j_receive_byte
	beq lc1d1
	lda #0
	clc ; OK
	rts

lc1da:	cmp #$68 ; check for keypad keys
	bcc lc1a5
	cmp #$80
	bcs lc1a5
	tay
	lda tab_keypad-$68,y
	clc
	rts

;****************************************
; EXTENDED KEY UP
;****************************************
decode_extended_key_up:
	cmp #$14
	beq handle_rctrl_up
	cmp #$11
	beq handle_ralt_up
	cmp #$12 ; print screen
	bne lc20d
lc1f4:	jsr j_receive_byte ; skip 2 bytes
	beq lc1f4
lc1f9:	jsr j_receive_byte
	beq lc1f9
	lda #0
	clc
	rts

handle_rctrl_up:
	lda #$0f - MODIFIER_CTRL
	.byte $2c
handle_ralt_up:
	lda #$0f - MODIFIER_ALT
	and modifier
	sta modifier
lc20d:	lda #0
	clc ; OK
	rts

;****************************************
; ADD CHAR TO KBD BUFFER
;****************************************
add_to_buf:
	pha
	lda $c6 ; length of keyboard buffer
	cmp #10
	bcs lc220 ; full, ignore
	inc $c6
	tax
	pla
	sta $0277,x ; store
	rts

lc220:	pla
	rts

;****************************************
; IRQ CODE
;****************************************
irq_code:
	lda last_code ; we have a code
	bne handle_code
; receive code for decoding next time
	jsr j_receive_byte
	sta last_code
	rts
handle_code:
	lda last_prefix
	bne handle_prefix ; we have a prefix and a code
;****************************************
; IRQ CODE: REGULAR KEY DOWN
;****************************************
; decode simple code and store it in queue
	lda last_code
	jsr j_decode_regular_key_down
	beq :+ ; no PETSCII code -> return
	jsr j_add_to_buf
:	rts
; there as a prefix, and a code
handle_prefix:
	cmp #BREAK_PREFIX
	bne not_break
;****************************************
; IRQ CODE: REGULAR KEY UP
;****************************************
	lda #0
	sta last_prefix
	lda last_code
	jsr j_decode_regular_key_up
	beq lc253
; XXX in theory, a key-up event could return a character
; XXX to put into the queue, but this doesn't happen in
; XXX pratice
	jsr j_add_to_buf
lc253:	lda #0
	sta last_code
	rts

lc259:	lda #0
	sta last_prefix
	sta last_code
	rts

not_break:
	cmp #EXTENDED_PREFIX
	bne not_extended
;****************************************
; IRQ CODE: EXTENDED KEY DOWN
;****************************************
	lda #0
	sta last_prefix
	lda last_code
	jsr j_decode_extended_key_down
	beq lc253
	jsr j_add_to_buf
	lda #0
	sta last_code
	rts

not_extended:
	cmp #EXTENDED_BREAK_PREFIX
	bne lc259
;****************************************
; IRQ CODE: EXTENDED KEY UP
;****************************************
	lda #0
	sta last_prefix
	lda last_code
	jsr j_decode_extended_key_up
	beq lc259
; XXX see above, this is not reached
	jsr j_add_to_buf
	clc
	bcc lc259

;****************************************
; ACTIVATE
;****************************************
activate:
	sei
	lda irq_ptr
	sta $0314
	lda irq_ptr+1
	sta $0315
	lda #1 ; set up raster IRQ
	sta $d01a
	sta $dc0d
	lda #$fa
	sta $d012
	lda $d011
	and #$7f
	sta $d011
	cli
	rts

;****************************************
; IRQ HANDLER
;****************************************
irq_handler:
	jsr irq_code_jmp
	inc $d019 ; ack IRQ
	jmp kernal_irq_ret

;****************************************
; DEACTIVATE
;****************************************
deactivate:
	sei
	lda #<kernal_irq_ret
	sta $0314
	lda #>kernal_irq_ret
	sta $0315
	lda #0 ; restore timer IRQ
	sta $d01a
	lda #0
	sta $dc0d
	sta $dc0e
	sta $dc0f
	lda #$1a
	sta $dc04
	lda #$41
	sta $dc05
	lda #1
	sta $dc0e
	lda #$81
	sta $dc0d
	cli
	rts

	.byte 0,0,0,0,0,0,0 ; XXX

last_prefix:
	.byte $00

last_code:
	.byte $00

scroll_lock_state:
	.byte $80

modifier:
	.byte $00

	.byte 0,0,0,0 ; XXX

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

	.byte 0,0,0,0,0,0,0,0 ; XXX

tab_keypad:
	.byte $00,$00,$00,$9d,$13,$00,$00,$00 ; @$68
	.byte $94,$14,$11,$00,$1d,$91,$00,$00 ; @$70
	.byte $00,$00,$0a,$00,$00,$93,$00,$00 ; @$78

	.byte 0,0,0,0,0,0,0,0 ; XXX
	.byte 0,0,0,0,0,0,0,0 ; XXX
	.byte 0,0,0,0,0,0,0,0 ; XXX
	.byte 0,0,0,0,0,0,0,0 ; XXX
	.byte 5 ; XXX

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
; E0 12 E0 7C   E0 F0 7C E0 F0 12   Print Screen
; E0 14         E0 F0 14            Right Ctrl
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
; E1 14 77 E1   F0 14 F0 77         Pause Break
; E0 7E E0 F0 7E                    Ctrl + Pause
