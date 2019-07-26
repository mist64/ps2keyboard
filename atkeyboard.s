; AT-Keyboard by İlker Fıçıcılar, 1997
;
; reverse-engineered by Michael Steil <mist64@mac.com>, 2019

lc000:	jmp lc020

lc003:	jmp lc093

lc006:	jmp lc114

lc009:	jmp lc17e

lc00c:	jmp lc1e8

lc00f:	jmp lc211

lc012:	jmp lc222

	jmp lc293

	jmp lc2c0

	.byte $4c
lc01c:	.addr lc2b7
	nop
	nop
lc020:	ldy $d012
	cpy #$f3
	bcs lc02f
	cpy #$18
	bcc lc02f
	lda #$00
	sec
	rts

lc02f:	lda $00
	and #$e7
	sta $00
	lda #$18
	ldy #$20
lc039:	cpy $d012
	beq lc08c
	bit $01
	bne lc039
	lda #$08
lc044:	bit $01
	beq lc044
	ldy #$09
lc04a:	bit $01
	bne lc04a
	lda $01
	and #$10
	cmp #$10
	ror $ff
	lda #$08
lc058:	bit $01
	beq lc058
	dey
	bne lc04a
	rol $ff
lc061:	bit $01
	bne lc061
lc065:	bit $01
	beq lc065
lc069:	lda $00
	ora #$18
	sta $00
	lda $01
	and #$f7
	ora #$10
	sta $01
	lda $ff
	beq lc08b
	php
lc07c:	lsr a
	bcc lc080
	iny
lc080:	cmp #$00
	bne lc07c
	tya
	plp
	adc #$01
	lsr a
	lda $ff
lc08b:	rts

lc08c:	clc
	lda #$00
	sta $ff
	beq lc069
lc093:	cmp #$e0
	beq lc0d4
	cmp #$f0
	beq lc0d4
	cmp #$e1
	beq lc0d4
	cmp #$12
	beq lc0b9
	cmp #$59
	beq lc0b9
	cmp #$14
	beq lc0b6
	cmp #$83
	beq lc0c6
	cmp #$11
	bne lc0de
	lda #$04
	.byte $2c
lc0b6:	lda #$02
	.byte $2c
lc0b9:	lda #$01
	ora lc2fb
	and #$0f
	sta lc2fb
	lda #$00
	.byte $2c
lc0c6:	lda #$88
	pha
	lda #$00
	sta lc2f9
	sta lc2f8
	pla
	clc
	rts

lc0d4:	sta lc2f8
lc0d7:	lda #$00
	sta lc2f9
	clc
	rts

lc0de:	cmp #$80
	bcs lc0d7
	tay
	lda #$00
	sta lc2f9
	sta lc2f8
	lda lc2fb
	bne lc0f5
lc0f0:	lda lc300,y
	clc
	rts

lc0f5:	cmp #$09
	beq lc0f0
	cmp #$02
	bne lc102
	lda lc400,y
	clc
	rts

lc102:	cmp #$04
	bne lc10b
	lda lc480,y
	clc
	rts

lc10b:	and #$09
	beq lc0d7
	lda lc380,y
	clc
	rts

lc114:	cmp #$e1
	beq lc142
	cmp #$58
	beq lc159
	cmp #$7e
	beq lc168
	cmp #$12
	beq lc136
	cmp #$59
	beq lc136
	cmp #$14
	beq lc133
	cmp #$11
	bne lc13e
	lda #$0b
	.byte $2c
lc133:	lda #$0d
	.byte $2c
lc136:	lda #$0e
	and lc2fb
	sta lc2fb
lc13e:	lda #$00
	clc
	rts

lc142:	lda #$00
	bit $03a9
	pha
	txa
	pha
	ldx #$07
lc14c:	jsr lc000
	beq lc14c
	dex
	bne lc14c
	pla
	tax
	pla
	clc
	rts

lc159:	lda lc2fb
	eor #$08
	sta lc2fb
	nop
	nop
	nop
	lda #$00
	clc
	rts

lc168:	lda lc2fa
	eor #$80
	sta lc2fa
	nop
	nop
	nop
	lda #$13
	ldy lc2fa
	bmi lc17c
	lda #$11
lc17c:	clc
	rts

lc17e:	cmp #$7e
	beq lc1af
	cmp #$f0
	beq lc1c0
	cmp #$4a
	beq lc1ab
	cmp #$5a
	beq lc1a8
	cmp #$12
	beq lc1cc
	cmp #$14
	beq lc19d
	cmp #$11
	bne lc1da
	lda #$04
	.byte $2c
lc19d:	lda #$02
	ora lc2fb
	sta lc2fb
lc1a5:	lda #$00
	.byte $2c
lc1a8:	lda #$0d
	.byte $2c
lc1ab:	lda #$2f
	clc
	rts

lc1af:	txa
	pha
	ldx #$06
lc1b3:	jsr lc000
	beq lc1b3
	dex
	bne lc1b3
	pla
	tax
	lda #$ff
	rts

lc1c0:	lda #$f1
	sta lc2f8
	lda #$00
	sta lc2f9
	clc
	rts

lc1cc:	jsr lc000
	beq lc1cc
lc1d1:	jsr lc000
	beq lc1d1
	lda #$00
	clc
	rts

lc1da:	cmp #$68
	bcc lc1a5
	cmp #$80
	bcs lc1a5
	tay
	lda lc4a0,y
	clc
	rts

lc1e8:	cmp #$14
	beq lc202
	cmp #$11
	beq lc205
	cmp #$12
	bne lc20d
lc1f4:	jsr lc000
	beq lc1f4
lc1f9:	jsr lc000
	beq lc1f9
	lda #$00
	clc
	rts

lc202:	lda #$0d
	.byte $2c
lc205:	lda #$0b
	and lc2fb
	sta lc2fb
lc20d:	lda #$00
	clc
	rts

lc211:	pha
	lda $c6
	cmp #$0a
	bcs lc220
	inc $c6
	tax
	pla
	sta $0277,x
	rts

lc220:	pla
	rts

lc222:	lda lc2f9
	bne lc22e
	jsr lc000
	sta lc2f9
	rts

lc22e:	lda lc2f8
	bne lc23f
	lda lc2f9
	jsr lc003
	beq lc23e
	jsr lc00f
lc23e:	rts

lc23f:	cmp #$f0
	bne lc262
	lda #$00
	sta lc2f8
	lda lc2f9
	jsr lc006
	beq lc253
	jsr lc00f
lc253:	lda #$00
	sta lc2f9
	rts

lc259:	lda #$00
	sta lc2f8
	sta lc2f9
	rts

lc262:	cmp #$e0
	bne lc27c
	lda #$00
	sta lc2f8
	lda lc2f9
	jsr lc009
	beq lc253
	jsr lc00f
	lda #$00
	sta lc2f9
	rts

lc27c:	cmp #$f1
	bne lc259
	lda #$00
	sta lc2f8
	lda lc2f9
	jsr lc00c
	beq lc259
	jsr lc00f
	clc
	bcc lc259
lc293:	sei
	lda lc01c
	sta $0314
	lda lc01c+1
	sta $0315
	lda #$01
	sta $d01a
	sta $dc0d
	lda #$fa
	sta $d012
	lda $d011
	and #$7f
	sta $d011
	cli
	rts

lc2b7:	jsr lc012
	inc $d019
	jmp $ea31

lc2c0:	sei
	lda #$31
	sta $0314
	lda #$ea
	sta $0315
	lda #$00
	sta $d01a
	lda #$00
	sta $dc0d
	sta $dc0e
	sta $dc0f
	lda #$1a
	sta $dc04
	lda #$41
	sta $dc05
	lda #$01
	sta $dc0e
	lda #$81
	sta $dc0d
	cli
	rts

	.byte 0,0,0,0,0,0,0

lc2f8:	.byte $00
lc2f9:	.byte $00
lc2fa:	.byte $80

lc2fb:	.byte $00,$00,$00,$00,$00

lc300:	.byte $00,$00,$00,$87,$86,$85,$89,$00
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

lc380:	.byte $00,$00,$00,$88,$00,$00,$00,$00
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

lc400:	.byte $00,$00,$00,$00,$00,$00,$00,$ff
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

lc480:	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$7e,$00
	.byte $00,$00,$00,$00,$00,$ab,$81,$00
	.byte $00,$00,$ad,$ae,$b0,$b3,$95,$00
lc4a0:	.byte $00,$bc,$bd,$ac,$b1,$97,$96,$00
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
