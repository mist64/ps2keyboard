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

.segment        "LOADADDR"
.addr   *+2
.segment "CODE"

receive_byte_jmp:
	jmp receive_byte

decode_byte_jmp:
	jmp decode_byte

decode_break_jmp:
	jmp decode_break

lc009:	jmp lc17e

lc00c:	jmp lc1e8

add_to_buf_jmp:
	jmp add_to_buf

irq_code_jmp:
	jmp irq_code

	jmp activate
	jmp deactivate

	.byte $4c
irq_ptr:
	.addr irq_handler

	nop
	nop

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

; * simple code:   decode into PETSCII and return in .A
; * modifier code: update modifier bitfield, return 0
; * prefix code:   store for later processing, return 0
decode_byte:
	cmp #$e0 ; extended code
	beq is_prefix
	cmp #$f0 ; break code
	beq is_prefix
	cmp #$e1 ; pause break
	beq is_prefix
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
	lda #$88 ; PETSCII for f7
	pha
	lda #0
	sta last_code
	sta last_prefix
	pla
	clc ; OK
	rts

is_prefix:
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
;
; key up handler
; * simple code:   ignore, return 0
; * pause/break:   ignore, return 0
; * modifier code: update modifier bitfield, return 0
decode_break:
	cmp #$e1 ; pause/break
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
lc14c:	jsr receive_byte_jmp
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
	lda lc2fa
	eor #$80
	sta lc2fa
	nop ; XXX
	nop ; XXX
	nop ; XXX
	lda #$13 ; XOFF (PETSCII: HOME)
	ldy lc2fa
	bmi lc17c
	lda #$11 ; XON (PETSCII: CSR DOWN)
lc17c:	clc
	rts

lc17e:	cmp #$7e ; scroll lock
	beq scroll_lock2
	cmp #$f0 ; break
	beq handle_break
	cmp #$4a; '/'
	beq key_slash
	cmp #$5a ; enter
	beq key_enter
	cmp #$12 ; left shift
	beq handle_lshift
	cmp #$14 ; left ctrl
	beq lc19d
	cmp #$11 ; left alt
	bne lc1da
; left alt
	lda #MODIFIER_ALT
	.byte $2c
lc19d:	lda #MODIFIER_CTRL
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

scroll_lock2:
	txa
	pha
	ldx #6
lc1b3:	jsr receive_byte_jmp
	beq lc1b3
	dex
	bne lc1b3 ; skip 6 non-empty bytes
	pla
	tax
	lda #$ff
	rts

handle_break:
	lda #$f1 ; ???
	sta last_prefix
	lda #0
	sta last_code
	clc
	rts

handle_lshift:
	jsr receive_byte_jmp
	beq handle_lshift
lc1d1:	jsr receive_byte_jmp
	beq lc1d1 ; skip any number of zero bytes
	lda #0
	clc ; OK
	rts

lc1da:	cmp #$68 ; check for keypad keys
	bcc lc1a5
	cmp #$80
	bcs lc1a5
	tay
	lda tab_keypad,y
	clc
	rts

lc1e8:	cmp #$14
	beq lc202
	cmp #$11
	beq lc205
	cmp #$12
	bne lc20d
lc1f4:	jsr receive_byte_jmp
	beq lc1f4
lc1f9:	jsr receive_byte_jmp
	beq lc1f9
	lda #0
	clc
	rts

lc202:	lda #MODIFIER_CAPS+MODIFIER_ALT+MODIFIER_SHIFT
	.byte $2c
lc205:	lda #MODIFIER_CAPS+MODIFIER_CTRL+MODIFIER_SHIFT
	and modifier
	sta modifier
lc20d:	lda #0
	clc ; OK
	rts

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

irq_code:
	lda last_code ; we received a code last time
	bne handle_code
; receive code for decoding next time
	jsr receive_byte_jmp
	sta last_code
	rts
handle_code:
	lda last_prefix
	bne handle_prefix
; decode simple code and store it in queue
	lda last_code
	jsr decode_byte_jmp
	beq :+ ; no PETSCII code -> return
	jsr add_to_buf_jmp
:	rts
; there as a prefix
handle_prefix:
	cmp #$f0 ; break
	bne not_break
; "break" (key up)
	lda #0
	sta last_prefix
	lda last_code
	jsr decode_break_jmp
	beq lc253
	jsr add_to_buf_jmp ; XXX never reached
lc253:	lda #0
	sta last_code
	rts

lc259:	lda #0
	sta last_prefix
	sta last_code
	rts

not_break:
	cmp #$e0 ; extended
	bne lc27c
	lda #0
	sta last_prefix
	lda last_code
	jsr lc009
	beq lc253
	jsr add_to_buf_jmp
	lda #0
	sta last_code
	rts

lc27c:	cmp #$f1
	bne lc259
	lda #0
	sta last_prefix
	lda last_code
	jsr lc00c
	beq lc259
	jsr add_to_buf_jmp
	clc
	bcc lc259

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

irq_handler:
	jsr irq_code_jmp
	inc $d019 ; ack IRQ
	jmp kernal_irq_ret

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

	.byte 0,0,0,0,0,0,0

; e0: "break" (key up)
; e1: pause/break key
; f0: extended key code
; f1: ???
last_prefix:
	.byte $00

last_code:
	.byte $00

lc2fa:
	.byte $80

modifier:
	.byte $00

	.byte $00,$00,$00,$00

tab_unshifted:
	.byte $00,$00,$00,$87,$86,$85,$89,$00
	.byte $00,$00,$8c,$8b,$8a,$00,$7e,$00
	.byte $00,$00,$00,$00,$00,$51,$31,$00
	.byte $00,$00,$5a,$53,$41,$57,$32,$00
	.byte $00,$43,$58,$44,$45,$34,$33,$00
	.byte $00,$20,$56,$46,$54,$52,$35,$00
	.byte $00,$4e,$42,$48,$47,$59,$36,$00
	.byte $00,$00,$4d,$4a,$55,$37,$38,$00
	.byte $00,$2c,$4b,$49,$4f,$30,$39,$00
	.byte $00,$2e,$2f,$4c,$3b,$50,$2d,$00
	.byte $00,$00,$2a,$00,$5b,$3d,$00,$00
	.byte $00,$00,$0d,$5d,$00,$5c,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$14,$00
	.byte $00,$31,$00,$34,$37,$00,$00,$00
	.byte $30,$2e,$32,$35,$36,$38,$1b,$00
	.byte $00,$2b,$33,$2d,$2a,$39,$00,$00

tab_shift:
	.byte $00,$00,$00,$88,$00,$00,$00,$00
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
tab_keypad:
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
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$9d,$13,$00,$00,$00
	.byte $94,$14,$11,$00,$1d,$91,$00,$00
	.byte $00,$00,$0a,$00,$00,$93,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $05
