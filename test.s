; ps2keyboard by Michael Steil

; This code is configured for a PS/2 (or AT) keyboard connected
; to the C64 tape port. As long as bit_data > bit_clk, the following
; four constants can be changed without changing the code.
;
port_ddr = 0  ; 6510 data direction register
port_data = 1  ; 6510 data register
;
               ; TAPE PIN A (VCC)   <---> PS/2 PIN 4 (VCC)  [AT PIN 4]
               ; TAPE PIN B (GNC)   <---> PS/2 PIN 3 (GND)  [AT PIN 5]
bit_clk  = $08 ; TAPE PIN E (write) <---> PS/2 PIN 5 (CLK)  [AT PIN 1]
bit_data = $10 ; TAPE PIN F (sense) <---> PS/2 PIN 1 (DATA) [AT PIN 2]

special = $fa ;XXX
byte = $fb ;XXX

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

	jsr kbdis

.if 0
	;kbdis
	lda port_ddr
	ora #bit_clk+bit_data
	sta port_ddr ; set CLK and DATA as output
	lda port_data
	and #$ff - bit_clk ; CLK=0
	ora #bit_data ; DATA=1
	sta port_data
.endif

	sei

	lda #0
	sta $d011
:	cmp $d012
	bne :-

;	jsr kbinit

	lda #$ed
	jsr send_byte
	lda #2
	jsr send_byte

.if 0
	jsr receive_byte
	beq :-
	sta $0400
:	jsr receive_byte
	beq :-
	sta $0401
	brk
.endif

	ldx #0
:	jsr receive_byte
	sta $0400,x
	inx
	bne :-
	ldx #0
:	jsr receive_byte
	sta $0500,x
	inx
	bne :-
	ldx #0
:	jsr receive_byte
	sta $0500,x
	inx
	bne :-

	lda #$1b
	sta $d011
	brk


.macro kbhighlow
:	lda #bit_clk          ; wait for a low to high to low transition
	bit port_data         ;
	beq :-                ; wait while clk low
:	bit port_data         ;
	bne :-                ; wait while clk is high
.endmacro

send_byte:
	sta byte              ; save byte to send
;	phx                   ; save registers
;	phy                   ;
;	sta lastbyte          ; keep just in case the send fails
	lda port_data         ;
	and #$FF-bit_clk      ; clk low, data high
	ora #bit_data         ;
	sta port_data         ;
	lda port_ddr          ;
	ora #bit_clk+bit_data ; both bits high
	sta port_ddr          ; set outputs, clk=0, data=1
	ldx #20               ; 100 µs
kbsendw:
	dex                   ;
	bne kbsendw           ; 64uS delay
	ldy #0                ; parity counter
	ldx #8                ; bit counter
	lda port_data         ;
	and #$FF-bit_clk-bit_data ; clk low, data low
	sta port_data         ;
	lda port_ddr          ;
	and #$FF-bit_clk      ; set clk as input
	sta port_ddr          ; set outputs

.if 0
	ldx #0
:	lda port_data
	sta $1000,x
	inx
	bne :-
:	lda port_data
	sta $1100,x
	inx
	bne :-
:	lda port_data
	sta $1200,x
	inx
	bne :-
:	lda port_data
	sta $1300,x
	inx
	bne :-
:	lda port_data
	sta $1400,x
	inx
	bne :-
:	lda port_data
	sta $1500,x
	inx
	bne :-
:	lda port_data
	sta $1600,x
	inx
	bne :-
:	lda port_data
	sta $1700,x
	inx
	bne :-

	lda #$1b
	sta $d011
	brk
.endif

	kbhighlow             ;
kbsend1: ror   byte              ; get lsb first
	bcs kbmark            ;
	lda port_data         ;
	and #$FF-bit_data     ; turn off data bit
	sta port_data         ;
	jmp kbnext            ;
kbmark:	lda port_data         ;
	ora #bit_data         ;
	sta port_data         ;
	iny                   ; inc parity counter
kbnext:	kbhighlow             ;
	dex                   ;
	bne kbsend1           ; send 8 data bits
	tya                   ; get parity count
	and #1                ; get odd or even
	bne kbpclr            ; if odd, send 0
	lda port_data         ;
	ora #bit_data         ; if even, send 1
	sta port_data         ;
	jmp kback             ;
kbpclr:	lda port_data         ;
	and #$FF-bit_data     ; send data=0
	sta port_data         ;
kback:	kbhighlow             ;
	lda port_ddr          ;
	and #$FF-bit_clk-bit_data ; set clk & data to input
	sta port_ddr          ;
;	ply                   ; restore saved registers
;	plx                   ;
	kbhighlow             ; wait for ack from keyboard
	bne kberror           ; VERY RUDE error handler - re-init the keyboard

kbsend2:
	lda port_data         ;
	and #bit_clk          ;
	beq kbsend2           ; wait while clk low

kbdis:	lda port_data         ; disable kb from sending more data
	and #$FF-bit_clk      ; clk = 0
	sta port_data         ;
	lda port_ddr          ; set clk to ouput low
	and #$FF-bit_clk-bit_data ; (stop more data until ready)
	ora #bit_clk          ;
	sta port_ddr          ;
	rts                   ;

kberror:
	inc $d020
	rts


.if 0
kbhighlow:     lda   #bit_clk          ; wait for a low to high to low transition
               bit   port_data         ;
               beq   kbhighlow         ; wait while clk low
kbhl1:         bit   port_data         ;
               bne   kbhl1             ; wait while clk is high
               lda   port_data         ;
               and   #bit_data         ; get data line state
               rts                     ;
.endif


kbinit:        lda   #$02              ; init - num lock on, all other off
               sta   special           ;
kbinit1:       lda   #$ff              ; keybrd reset
               jsr   send_byte         ; reset keyboard
               jsr   receive_byte      ;
               cmp   #$FA              ; ack?
               bne   kbinit1           ; resend reset cmd
               jsr   receive_byte             ;
               cmp   #$AA              ; reset ok
               bne   kbinit1           ; resend reset cmd
                                       ; fall into to set the leds
kbsled:        lda   #$ED              ; Set the keybrd LED's from kbleds variable
               jsr   send_byte         ;
               jsr   receive_byte      ;
               cmp   #$FA              ; ack?
               bne   kbsled            ; resend led cmd
               lda   special           ;
               and   #$07              ; ensure bits 3-7 are 0
               jsr   send_byte         ;
               rts                     ;

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
.if 0
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
