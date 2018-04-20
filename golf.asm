; Name:			Ian Spryn and Nathaniel Specher
; Course:		COMP 252
; Instructor:		Dr. Conlon
; Date started:		March 20, 2018
; Last modification:	March 20, 2018
; Purpose of program:	Clear screen and keybaord input, move puck and paddles

	.CR 6502             		; Assemble 6502
	.LI on,toff      		; Listing on, no timings included
	.TF golf.prg,BIN		; Object filename and format

; Define some constants
;space 	= $20				;ASCII code for space.
  	.OR	$0000			;Start code at address $0000
	jmp start

; Define zero-page storage
first	= $7000				;Address of upper left (home) on video screen
second 	= first+256
third	= second+256
fourth	= third+256

cr	= $0d				;Carriage-return character
lf	= $0a				;Line-feed (newline) character
iobase	= $8800
iostat	= iobase+1
iocmd	= iobase+2
ioctrl	= iobase+3
nmivecl	= $fffa				;NMI (non-maskable interrupt) vector: fffa-fffb
nmivech	= nmivecl+1
rstvecl	= $fffc				;RES (reset) vector:                  fffc-fffd
rstvech	= rstvecl+1
irqvecl	= $fffe				;IRQ (interrupt request)      vector: fffe-ffff
irqvech	= irqvecl+1
inbuff	.BS $20	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check and  see if the tail is one less than the head, then the buffer is full



left_paddle		.DB 5
right_paddle		.DB 5

puckrow	.DB 0
puckcolumn	.DB 0
puckdir	.DB 4				;Set direction to be initially down and to the right
;					__________________
;					\		/
;					 1	       2
;
;					 3	       4
;					/		\
;					__________________
curline	.DW 2


row1	.DW $7000
row2	.DW $7028
row3	.DW $7050
row4	.DW $7078
row5	.DW $70A0
row6	.DW $70C8
row7	.DW $70F0
row8	.DW $7118
row9	.DW $7140
row10	.DW $7168
row11	.DW $7190
row12	.DW $71B8
row13	.DW $71E0
row14	.DW $7208
row15	.DW $7230
row16	.DW $7258
row17	.DW $7280
row18	.DW $72A8
row19	.DW $72D0
row20	.DW $72F8
row21	.DW $7320
row22	.DW $7348
row23	.DW $7370
row24	.DW $7398
row25	.DW $73C0



	.BS $20 ;32-byte circular input buffer
;headptr .DB 0 ;Initialize buffer offsets to zero					;;;;;;;;;;;;;NEW
;tailptr .DB 0										;;;;;;;;;;;;;NEW

	.BS $0300-*
	;.NO	$0300			;New origin. Skip to beginning of program, proper.



start
	cld
	lda #' '
.loop1
	sta first,y
	iny
	bne .loop1
.loop2
	sta second,y
	iny
	bne .loop2
.loop3
	sta third,y
	iny
	bne .loop3
.loop4
	sta fourth,y
	iny
	bne .loop4
	;jsr drawpaddle			;To be used once we remove it from idle1 in order to initialize it
	jsr initirv			;Initialize ACIA and IRQ vectors.
	jmp idle			;Then idle, waiting for interrupt.

;;
;; Infinite idle loop, waiting for interrupt.
;;
;Here you would put your main event loop instead of this code.
;idle and idle1 are used as timers. Change the value of y to increase/decrease time between each run
;dixed delay for puck, no paddle delay
	;if it is time, move puck
;main loop shouldn't ahve thigns that take a lot of time
idle:
	ldx #$ff
	ldy #$28 ;$1F	;28
.idle1	dex
	bne .idle1
	dey
	bne .idle1
	jsr drawpuck
	jsr drawpaddle			;For now, we call this so the puck doesn't overwrite the paddle on the screen
	jmp idle

;;
;;	IRQ handler
;;	This code must precede initirv in assembly-code file.
;;
irhand: pha				;Save register A.
;Save register A.
	lda iobase			;Get the character in the ACIA.
	pha				;Save character in accumulator
.echo: 	lda iostat			;Read the ACIA status
	and #%00010000			;Is the tx register empty?
	beq .echo			;No, wait for it to empty
	pla				;Otherwise, load saved character.
	sta .temp_a
	jsr movepaddle			;Push character to stack, and move the paddle in the main loop
	lda .temp_a
	sta iobase			;Write to output.
	cmp #cr				;If CR, automatically insert LF.
	bne .out
	lda #lf
	pha
	jmp .echo
.temp_a	.DB 0				;Used to solve problem of text being outputted wrong
.out
	pla				;Restore register A.
	cli				;Enable interrupts.
	rti				;and return

irhlo	.DW irhand	;Store address of IRQ handler for init.

;;
;; Intialize interrupt vector.
;;	This code must follow irhand code in assembly-code file.
;;
initirv lda #%00001001
	sta iocmd			;Set command status
	lda #%00011010
	sta ioctrl			;0 stop bits, 8 bit word, 9600 bps
	lda irhlo			;Get low-byte addr of interrupt handler.
	sta irqvecl			;Store in IRQ and NMI vectors.
	sta nmivecl
	lda irhlo+1			;Get high-byte addr of interrupt handler.
	sta irqvech			;Store it in IRQ and NMI vectors.
	sta nmivech
	lda #$00			;Initialize reset vector.
	sta rstvecl			;Store it in reset vector.
	sta rstvech
	cli				;Enable interrupts.
	rts



movepaddle:
;'w' is to move the left paddle up
;'s' is to love the left paddle down
;'p' is to move the right paddle up
;';' is to move the right paddle down
	cmp #'w'
	beq movepaddleleftup
	cmp #'s'
	beq movepaddleleftdown
	cmp #'p'
	beq movepaddlerightup
	cmp #';'
	beq movepaddlerightdown
	rts

return
        rts

;Delete either top or bottom of paddle in preparation of drawing next phase of new paddle position
clearoldpaddle
	lda #' '
        pha
        tya             ;Transfer paddle position to a
        pha
	txa
        pha
        jsr prch
        rts

;Draw a new character 
drawnewpaddle
	pha
	tya				;transfer left or right paddle position  to a
	pha				;push paddle position to stack
	txa				;Load column (0 or 40) into a
	pha
	jsr prch
	rts

movepaddleleftup
        clc				;Prevents overflow
        ldy left_paddle
	cpy #1				;Are we at the top? If so, don't move any higher
	bmi return
	iny
	iny				;Move to bottom of paddle
	ldx #0				;Load column number into x
        jsr clearoldpaddle
	dec left_paddle
	ldy left_paddle			;Reset y to the initial location of the original paddle
	lda #$F6
	jsr drawnewpaddle
        rts

movepaddleleftdown
	clc
	ldy left_paddle
	cpy #22
	bpl return
	ldx #0				;Load column number into x
	jsr clearoldpaddle
	inc left_paddle
	ldy left_paddle
	iny
	iny
	lda #$F6
	jsr drawnewpaddle
	rts

movepaddlerightup
        clc				;Prevents overflow
        ldy right_paddle
	cpy #1				;Are we at the top? If so, don't move any higher
	bmi return
	iny
	iny				;Move to bottom of paddle
	ldx #39				;Load column number into x
        jsr clearoldpaddle
	dec right_paddle
	ldy right_paddle		;Reset y to the initial location of the original paddle
	lda #$F5
	jsr drawnewpaddle
        rts

movepaddlerightdown
	clc
	ldy right_paddle
	cpy #22
	bpl return
	ldx #39				;Load column number into x
	jsr clearoldpaddle
	inc right_paddle
	ldy right_paddle
	iny
	iny
	lda #$F5
	jsr drawnewpaddle
	rts
		
prch:
	;pull off pointer return address from stack
	stx .tempx
	pla
	sta .pcretad           		;pointer return address
	pla
	sta .pcretad+1
	pla                           	;get column, because it's the last thing we pushed
	tay                           	;save column
	pla                           	;get row
	asl				;double it by shifting to the left 1 (which multiples by 2)
	tax             		;save row
	;get address of row
	lda row1,x
	sta curline
	lda row1+1,x
	sta curline+1
	pla				;get character ('*' or ' ')
	sta (curline),y			;y is the column number, and we don't need to double it because it's 1 byte per column

	;restore poiter return adress to stack
	lda .pcretad+1
	pha
	lda .pcretad
	pha
	clc
	ldx .tempx
	rts
.pcretad	.DW $0000		;pointer return address
.tempx	.DB 0

;rtch:	;return char
;return what is in that space
;if the place that doesn't have a space, then the puc should not be placed therea nd soemthing else shoudl happen


drawpaddle:
	lda #$F6
	pha
	lda left_paddle
	pha
	lda #0
	pha
	jsr prch
	lda #$F6
	pha
	lda left_paddle
	clc
	adc #$1
	pha
	lda #0
	pha
	jsr prch
	lda #$F6
	pha
	lda left_paddle
	clc
	adc #$2
	pha
	lda #0
	pha
	jsr prch
	
	lda #$F5
	pha
	lda right_paddle
	pha
	lda #39
	pha
	jsr prch
	lda #$F5
	pha
	lda right_paddle
	clc
	adc #$1
	pha
	lda #39
	pha
	jsr prch
	lda #$F5
	pha
	lda right_paddle
	clc
	adc #$2
	pha
	lda #39
	pha
	jsr prch
	
	rts

drawpuck:
	lda #' '
	pha
	lda puckrow
        pha
        lda puckcolumn
	pha
	jsr prch                   ;Call to draw puckcolumn
	;Update postion of puckrow and puckcolumn
	lda puckdir
	cmp #1
	beq move1
	cmp #2
	beq move2
	cmp #3
	beq move3
	cmp #4
	beq move4
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dec puckcolumn ETC
;TODO add 2, 4, 6, 8

move1:
;puckcolumn--
;puckrow--
;if we hit the left side of the wall, jump to move2 to move the ball up and to the right
	ldx puckcolumn
	cpx #1
	bpl .move1one
	inc puckdir
	jmp move2
.move1one
;if we hit the top side of the wall, jump to move3 to move the ball down and to the left
	lda puckrow
	cmp #1
	bpl .move1two
	inc puckdir
	inc puckdir
	jmp move3
.move1two
	dec puckcolumn
	dec puckrow
	jmp newpuck

move2:
;puckcolumn++
;puckrow--
;if we hit the right side of the wall, jump to move1 to move the ball up and to the left
	ldx puckcolumn
	cpx #39
	bmi .move2one
	dec puckdir
	jmp move1
.move2one
;if we hit the top side of the wall, jump to move4 to move the ball down and to the right
	lda puckrow
	cmp #1
	bpl .move2two
	inc puckdir
	inc puckdir
	jmp move4
.move2two	
	inc puckcolumn
	dec puckrow
	jmp newpuck

move3:
;puckcolumn--
;puckrow++
;if we hit the right side of the wall, jump to move4 to move the ball down and to the right
	ldx puckcolumn
	cpx #1
	bpl .move3one
	inc puckdir
	jmp move4
.move3one
;if we hit the bottom side of the wall, jump to move1 to move the ball up and to the left
	lda puckrow
	cmp #24
	bmi .move3two
	dec puckdir
	dec puckdir
	jmp move1
.move3two
	dec puckcolumn
	inc puckrow
	jmp newpuck

move4:
;puckcolumn++
;puckrow++
;if we hit the right side of the wall, jump to move3 to move the ball down and to the left
	ldx puckcolumn
	cpx #39
	bmi .move4one
	dec puckdir
	jmp move3
.move4one
;if we hit the bottom side of the wall, jump to move2 to move the ball up and to the right
	lda puckrow
	cmp #24
	bmi .move4two
	dec puckdir
	dec puckdir
	jmp move2	
.move4two
	inc puckcolumn
	inc puckrow
	jmp newpuck

newpuck:
	lda #'*'
	pha
	lda puckrow
        pha
        lda puckcolumn
	pha
	jsr prch                   ;Call to draw puckcolumn
	rts