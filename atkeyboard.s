; da65 V2.15
; Created:    2019-07-26 10:34:52
; Input file: atkeyboard-c000.bin
; Page:       1


        .setcpu "6502"

LEA31           := $EA31
LC000:  jmp     LC020

LC003:  jmp     LC093

LC006:  jmp     LC114

LC009:  jmp     LC17E

LC00C:  jmp     LC1E8

LC00F:  jmp     LC211

LC012:  jmp     LC222

        jmp     LC293

        jmp     LC2C0

        .byte   $4C
LC01C:  .addr   LC2B7
        nop
        nop
LC020:  ldy     $D012
        cpy     #$F3
        bcs     LC02F
        cpy     #$18
        bcc     LC02F
        lda     #$00
        sec
        rts

LC02F:  lda     $00
        and     #$E7
        sta     $00
        lda     #$18
        ldy     #$20
LC039:  cpy     $D012
        beq     LC08C
        bit     $01
        bne     LC039
        lda     #$08
LC044:  bit     $01
        beq     LC044
        ldy     #$09
LC04A:  bit     $01
        bne     LC04A
        lda     $01
        and     #$10
        cmp     #$10
        ror     $FF
        lda     #$08
LC058:  bit     $01
        beq     LC058
        dey
        bne     LC04A
        rol     $FF
LC061:  bit     $01
        bne     LC061
LC065:  bit     $01
        beq     LC065
LC069:  lda     $00
        ora     #$18
        sta     $00
        lda     $01
        and     #$F7
        ora     #$10
        sta     $01
        lda     $FF
        beq     LC08B
        php
LC07C:  lsr     a
        bcc     LC080
        iny
LC080:  cmp     #$00
        bne     LC07C
        tya
        plp
        adc     #$01
        lsr     a
        lda     $FF
LC08B:  rts

LC08C:  clc
        lda     #$00
        sta     $FF
        beq     LC069
LC093:  cmp     #$E0
        beq     LC0D4
        cmp     #$F0
        beq     LC0D4
        cmp     #$E1
        beq     LC0D4
        cmp     #$12
        beq     LC0B9
        cmp     #$59
        beq     LC0B9
        cmp     #$14
        beq     LC0B6
        cmp     #$83
        beq     LC0C6
        cmp     #$11
        bne     LC0DE
        lda     #$04
        .byte   $2C
LC0B6:  lda     #$02
        .byte   $2C
LC0B9:  lda     #$01
        ora     LC2FB
        and     #$0F
        sta     LC2FB
        lda     #$00
        .byte   $2C
LC0C6:  lda     #$88
        pha
        lda     #$00
        sta     LC2F9
        sta     LC2F8
        pla
        clc
        rts

LC0D4:  sta     LC2F8
LC0D7:  lda     #$00
        sta     LC2F9
        clc
        rts

LC0DE:  cmp     #$80
        bcs     LC0D7
        tay
        lda     #$00
        sta     LC2F9
        sta     LC2F8
        lda     LC2FB
        bne     LC0F5
LC0F0:  lda     LC300,y
        clc
        rts

LC0F5:  cmp     #$09
        beq     LC0F0
        cmp     #$02
        bne     LC102
        lda     LC400,y
        clc
        rts

LC102:  cmp     #$04
        bne     LC10B
        lda     LC480,y
        clc
        rts

LC10B:  and     #$09
        beq     LC0D7
        lda     LC380,y
        clc
        rts

LC114:  cmp     #$E1
        beq     LC142
        cmp     #$58
        beq     LC159
        cmp     #$7E
        beq     LC168
        cmp     #$12
        beq     LC136
        cmp     #$59
        beq     LC136
        cmp     #$14
        beq     LC133
        cmp     #$11
        bne     LC13E
        lda     #$0B
        .byte   $2C
LC133:  lda     #$0D
        .byte   $2C
LC136:  lda     #$0E
        and     LC2FB
        sta     LC2FB
LC13E:  lda     #$00
        clc
        rts

LC142:  lda     #$00
        bit     $03A9
        pha
        txa
        pha
        ldx     #$07
LC14C:  jsr     LC000
        beq     LC14C
        dex
        bne     LC14C
        pla
        tax
        pla
        clc
        rts

LC159:  lda     LC2FB
        eor     #$08
        sta     LC2FB
        nop
        nop
        nop
        lda     #$00
        clc
        rts

LC168:  lda     LC2FA
        eor     #$80
        sta     LC2FA
        nop
        nop
        nop
        lda     #$13
        ldy     LC2FA
        bmi     LC17C
        lda     #$11
LC17C:  clc
        rts

LC17E:  cmp     #$7E
        beq     LC1AF
        cmp     #$F0
        beq     LC1C0
        cmp     #$4A
        beq     LC1AB
        cmp     #$5A
        beq     LC1A8
        cmp     #$12
        beq     LC1CC
        cmp     #$14
        beq     LC19D
        cmp     #$11
        bne     LC1DA
        lda     #$04
        .byte   $2C
LC19D:  lda     #$02
        ora     LC2FB
        sta     LC2FB
LC1A5:  lda     #$00
        .byte   $2C
LC1A8:  lda     #$0D
        .byte   $2C
LC1AB:  lda     #$2F
        clc
        rts

LC1AF:  txa
        pha
        ldx     #$06
LC1B3:  jsr     LC000
        beq     LC1B3
        dex
        bne     LC1B3
        pla
        tax
        lda     #$FF
        rts

LC1C0:  lda     #$F1
        sta     LC2F8
        lda     #$00
        sta     LC2F9
        clc
        rts

LC1CC:  jsr     LC000
        beq     LC1CC
LC1D1:  jsr     LC000
        beq     LC1D1
        lda     #$00
        clc
        rts

LC1DA:  cmp     #$68
        bcc     LC1A5
        cmp     #$80
        bcs     LC1A5
        tay
        lda     LC4A0,y
        clc
        rts

LC1E8:  cmp     #$14
        beq     LC202
        cmp     #$11
        beq     LC205
        cmp     #$12
        bne     LC20D
LC1F4:  jsr     LC000
        beq     LC1F4
LC1F9:  jsr     LC000
        beq     LC1F9
        lda     #$00
        clc
        rts

LC202:  lda     #$0D
        .byte   $2C
LC205:  lda     #$0B
        and     LC2FB
        sta     LC2FB
LC20D:  lda     #$00
        clc
        rts

LC211:  pha
        lda     $C6
        cmp     #$0A
        bcs     LC220
        inc     $C6
        tax
        pla
        sta     $0277,x
        rts

LC220:  pla
        rts

LC222:  lda     LC2F9
        bne     LC22E
        jsr     LC000
        sta     LC2F9
        rts

LC22E:  lda     LC2F8
        bne     LC23F
        lda     LC2F9
        jsr     LC003
        beq     LC23E
        jsr     LC00F
LC23E:  rts

LC23F:  cmp     #$F0
        bne     LC262
        lda     #$00
        sta     LC2F8
        lda     LC2F9
        jsr     LC006
        beq     LC253
        jsr     LC00F
LC253:  lda     #$00
        sta     LC2F9
        rts

LC259:  lda     #$00
        sta     LC2F8
        sta     LC2F9
        rts

LC262:  cmp     #$E0
        bne     LC27C
        lda     #$00
        sta     LC2F8
        lda     LC2F9
        jsr     LC009
        beq     LC253
        jsr     LC00F
        lda     #$00
        sta     LC2F9
        rts

LC27C:  cmp     #$F1
        bne     LC259
        lda     #$00
        sta     LC2F8
        lda     LC2F9
        jsr     LC00C
        beq     LC259
        jsr     LC00F
        clc
        bcc     LC259
LC293:  sei
        lda     LC01C
        sta     $0314
        lda     LC01C+1
        sta     $0315
        lda     #$01
        sta     $D01A
        sta     $DC0D
        lda     #$FA
        sta     $D012
        lda     $D011
        and     #$7F
        sta     $D011
        cli
        rts

LC2B7:  jsr     LC012
        inc     $D019
        jmp     LEA31

LC2C0:  sei
        lda     #$31
        sta     $0314
        lda     #$EA
        sta     $0315
        lda     #$00
        sta     $D01A
        lda     #$00
        sta     $DC0D
        sta     $DC0E
        sta     $DC0F
        lda     #$1A
        sta     $DC04
        lda     #$41
        sta     $DC05
        lda     #$01
        sta     $DC0E
        lda     #$81
        sta     $DC0D
        cli
        rts

        .byte   $00,$00,$00,$00,$00,$00,$00
LC2F8:  .byte   $00
LC2F9:  .byte   $00
LC2FA:  .byte   $80
LC2FB:  .byte   $00,$00,$00,$00,$00
LC300:  .byte   $00,$00,$00,$87,$86,$85,$89,$00
        .byte   $00,$00,$8C,$8B,$8A,$00,$7E,$00
        .byte   $00,$00,$00,$00,$00,$51,$31,$00
        .byte   $00,$00,$5A,$53,$41,$57,$32,$00
        .byte   $00,$43,$58,$44,$45,$34,$33,$00
        .byte   $00,$20,$56,$46,$54,$52,$35,$00
        .byte   $00,$4E,$42,$48,$47,$59,$36,$00
        .byte   $00,$00,$4D,$4A,$55,$37,$38,$00
        .byte   $00,$2C,$4B,$49,$4F,$30,$39,$00
        .byte   $00,$2E,$2F,$4C,$3B,$50,$2D,$00
        .byte   $00,$00,$2A,$00,$5B,$3D,$00,$00
        .byte   $00,$00,$0D,$5D,$00,$5C,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$14,$00
        .byte   $00,$31,$00,$34,$37,$00,$00,$00
        .byte   $30,$2E,$32,$35,$36,$38,$1B,$00
        .byte   $00,$2B,$33,$2D,$2A,$39,$00,$00
LC380:  .byte   $00,$00,$00,$88,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$1B,$00
        .byte   $00,$00,$00,$00,$00,$71,$21,$00
        .byte   $00,$00,$7A,$73,$61,$77,$22,$00
        .byte   $00,$63,$78,$64,$65,$24,$23,$00
        .byte   $00,$20,$76,$66,$74,$72,$25,$00
        .byte   $00,$6E,$62,$68,$67,$79,$26,$00
        .byte   $00,$00,$6D,$6A,$75,$27,$28,$00
        .byte   $00,$3C,$6B,$69,$6F,$30,$29,$00
        .byte   $00,$3E,$3F,$6C,$3A,$70,$2D,$00
        .byte   $00,$00,$5F,$00,$7B,$2B,$00,$00
        .byte   $00,$00,$0D,$7D,$00,$7C,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$14,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$14,$00,$00,$00,$00,$00,$00
        .byte   $00,$2B,$00,$2D,$00,$00,$00,$00
LC400:  .byte   $00,$00,$00,$00,$00,$00,$00,$FF
        .byte   $00,$00,$00,$00,$00,$00,$7E,$00
        .byte   $00,$00,$00,$00,$00,$11,$90,$00
        .byte   $00,$00,$1A,$13,$01,$17,$05,$00
        .byte   $00,$03,$18,$04,$05,$9F,$1C,$00
        .byte   $00,$20,$16,$06,$14,$12,$9C,$00
        .byte   $00,$0E,$02,$08,$07,$19,$1E,$00
        .byte   $00,$00,$0D,$0A,$15,$1F,$9E,$00
        .byte   $00,$2C,$0B,$09,$0F,$92,$12,$00
        .byte   $00,$2E,$2F,$0C,$3B,$10,$2D,$00
        .byte   $00,$00,$27,$00,$5B,$3D,$00,$00
        .byte   $00,$00,$0D,$5D,$00,$5C,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$14,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$2B,$00,$2D,$00,$00,$00,$00
LC480:  .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$7E,$00
        .byte   $00,$00,$00,$00,$00,$AB,$81,$00
        .byte   $00,$00,$AD,$AE,$B0,$B3,$95,$00
LC4A0:  .byte   $00,$BC,$BD,$AC,$B1,$97,$96,$00
        .byte   $00,$20,$BE,$BB,$A3,$B2,$98,$00
        .byte   $00,$B6,$BF,$B4,$A5,$B7,$99,$00
        .byte   $00,$00,$A7,$B5,$B8,$9A,$9B,$00
        .byte   $00,$3C,$A1,$A2,$B9,$30,$29,$00
        .byte   $00,$3E,$3F,$B6,$5D,$AF,$7C,$00
        .byte   $00,$00,$27,$00,$40,$3D,$00,$00
        .byte   $00,$00,$0D,$5E,$00,$A8,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$14,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$2B,$00,$2D,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$9D,$13,$00,$00,$00
        .byte   $94,$14,$11,$00,$1D,$91,$00,$00
        .byte   $00,$00,$0A,$00,$00,$93,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $05
