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

parity = $fb
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
:	jsr kbget
	sta $0400,x
	inx
	bne :-
	ldx #0
:	jsr kbget
	sta $0500,x
	inx
	bne :-
	ldx #0
:	jsr kbget
	sta $0500,x
	inx
	bne :-

	lda #$1b
	sta $d011
	brk




receive_byte:
; test for badline-safe time window
; set input, bus idle
:	lda port_ddr ; set CLK and DATA as input
	and #$ff-bit_clk-bit_data
	sta port_ddr ; -> bus is idle, keyboard can start sending

	lda #bit_clk+bit_data
	ldy #10
:	dey
	beq lc08c
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




.feature labels_without_colons
;
; send a byte to the keyboard
;
send_byte:
               sta   byte              ; save byte to send
               lda   port_data         ;
               and   #$FF-bit_clk              ; clk low, data high (change if port bits change)
               ora   #bit_data             ;
               sta   port_data         ;
               lda   port_ddr         ;
               ora   #$30              ;  bit bits high (change if port bits change)
               sta   port_ddr         ; set outputs, clk=0, data=1
               ldx   #$10              ; 1Mhz cpu clock delay (delay = cpuclk/62500)
kbsendw        dex                     ;
               bne   kbsendw           ; 64uS delay
               ldy   #$00              ; parity counter
               ldx   #$08              ; bit counter
               lda   port_data         ;
               and   #$FF-bit_clk-bit_data              ; clk low, data low (change if port bits change)
               sta   port_data         ;
               lda   port_ddr         ;
               and   #$FF-bit_clk              ; set clk as input (change if port bits change)
               sta   port_ddr         ; set outputs
               jsr   kbhighlow         ;
kbsend1        ror   byte              ; get lsb first
               bcs   kbmark            ;
               lda   port_data         ;
               and   #$FF-bit_data              ; turn off data bit (change if port bits change)
               sta   port_data         ;
               jmp   kbnext            ;
kbmark         lda   port_data         ;
               ora   #bit_data             ;
               sta   port_data         ;
               iny                     ; inc parity counter
kbnext         jsr   kbhighlow         ;
               dex                     ;
               bne   kbsend1           ; send 8 data bits
               tya                     ; get parity count
               and   #$01              ; get odd or even
               bne   kbpclr            ; if odd, send 0
               lda   port_data         ;
               ora   #bit_data             ; if even, send 1
               sta   port_data         ;
               jmp   kback             ;
kbpclr         lda   port_data         ;
               and   #$FF-bit_data              ; send data=0 (change if port bits change)
               sta   port_data         ;
kback          jsr   kbhighlow         ;
               lda   port_ddr         ;
               and   #$FF-bit_clk-bit_data              ; set clk & data to input (change if port bits change)
               sta   port_ddr         ;
               jsr   kbhighlow         ; wait for ack from keyboard
               bne   kberror2            ; VERY RUDE error handler - re-init the keyboard
kbsend2        lda   port_data         ;
               and   #bit_clk              ;
               beq   kbsend2           ; wait while clk low
               jmp   kbdis             ; diable kb sending

kberror2:
;	jmp kbinit
	inc $d020
	rts


.if 1
;
; KBGET waits for one scancode from the keyboard
;
kberror        lda   #$FE              ; resend cmd
               jsr   send_byte            ;
kbget          lda   #$00              ;
               sta   byte              ; clear scankey holder
               sta   parity            ; clear parity holder
               ldy   #$00              ; clear parity counter
               ldx   #$08              ; bit counter
               lda   port_ddr         ;
               and   #$FF-bit_clk-bit_data              ; set clk to input (change if port bits change)
               sta   port_ddr         ;
kbget1         lda   #bit_clk              ;
               bit   port_data         ;
               bne   kbget1            ; wait while clk is high
               lda   port_data         ;
               and   #bit_data             ; get start bit
               bne   kbget1            ; if 1, false start bit, do again
kbget2         jsr   kbhighlow         ; wait for clk to return high then go low again
               cmp   #$01              ; set c if data bit=1, clr if data bit=0
                                       ; (change if port bits change) ok unless data=01 or 80
                                       ; in that case, use ASL or LSR to set carry bit
               ror   byte              ; save bit to byte holder
               bpl   kbget3            ;
               iny                     ; add 1 to parity counter
kbget3         dex                     ; dec bit counter
               bne   kbget2            ; get next bit if bit count > 0
               jsr   kbhighlow         ; wait for parity bit
               beq   kbget4            ; if parity bit 0 do nothing
               inc   parity            ; if 1, set parity to 1
kbget4         tya                     ; get parity count
               eor   parity            ; compare with parity bit
               and   #$01              ; mask bit 1 only
               beq   kberror           ; bad parity
               jsr   kbhighlow         ; wait for stop bit
               beq   kberror           ; 0=bad stop bit
               lda   byte              ; if byte & parity 0,
               beq   kbget             ; no data, do again
               jsr   kbdis             ;
               lda   byte              ;
               rts                     ;
;
.endif

kbdis          lda   port_data         ; disable kb from sending more data
               and   #$FF-bit_clk              ; clk = 0 (change if port bits change)
               sta   port_data         ;
               lda   port_ddr         ; set clk to ouput low
               and   #$FF-bit_clk-bit_data              ; (stop more data until ready) (change if port bits change)
               ora   #bit_clk              ;
               sta   port_ddr         ;
               rts                     ;

.if 0
;
kbinit         lda   #$02              ; init - num lock on, all other off
               sta   special           ;
kbinit1        lda   #$ff              ; keybrd reset
               jsr   send_byte            ; reset keyboard
               jsr   kbget             ;
               cmp   #$FA              ; ack?
               bne   kbinit1           ; resend reset cmd
               jsr   kbget             ;
               cmp   #$AA              ; reset ok
               bne   kbinit1           ; resend reset cmd
                                       ; fall into to set the leds
kbsled         lda   #$ED              ; Set the keybrd LED's from kbleds variable
               jsr   kbsend            ;
               jsr   kbget             ;
               cmp   #$FA              ; ack?
               bne   kbsled            ; resend led cmd
               lda   special           ;
               and   #$07              ; ensure bits 3-7 are 0
               jsr   send_byte            ;
               rts                     ;
.endif
                                       ;
kbhighlow      lda   #bit_clk              ; wait for a low to high to low transition
               bit   port_data         ;
               beq   kbhighlow         ; wait while clk low
kbhl1          bit   port_data         ;
               bne   kbhl1             ; wait while clk is high
               lda   port_data         ;
               and   #bit_data             ; get data line state
               rts                     ;
