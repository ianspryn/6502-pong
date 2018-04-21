; Name:			Ian Spryn and Nathaniel Sprecher
; Course:		COMP 252
; Instructor:		Dr. Conlon
; Date started:		March 20, 2018
; Last modification:	March 20, 2018
; Purpose of program:	Clear screen and keyboard input, move puck and paddles

	.CR 6502         ; Assemble 6502
	.LI on,toff      ; Listing on, no timings included
	.TF golf.prg,BIN	; Object filename and format

; Define some constants
  	.OR	$0000	;Start code at address $0000
	jmp start

; Define zero-page storage
first	= $7000		;Address of upper left (home) on video screen
second 	= first+256
third	= second+256
fourth	= third+256

cr	= $0d		;Carriage-return character
lf	= $0a		;Line-feed (newline) character
iobase	= $8800
iodata	= iobase
iostat	= iobase+1
iocmd	= iobase+2
ioctrl	= iobase+3
nmivecl	= $fffa		;NMI (non-maskable interrupt) vector: fffa-fffb
nmivech	= nmivecl+1
rstvecl	= $fffc		;RES (reset) vector:                  fffc-fffd
rstvech	= rstvecl+1
irqvecl	= $fffe		;IRQ (interrupt request)      vector: fffe-ffff
irqvech	= irqvecl+1
inbuff	.BS $20

irhlo	.DW irhand	;Store address of IRQ handler for init.
btimer	.DB 0		;Timer used to move ball slower
player1	.DB 0		;player1's (left) score
player2	.DB 0		;player2's (right) score
lpaddle	.DB 5
rpaddle	.DB 5
puckrow	.DB 0		;Used to keep track of the puck's row
puckcol	.DB 0		;Used to keep track of the puck's column
puckdir	.DB 4		;Set direction to be initially down and to the right
;;			__________________
;;			\		/
;;			 1	       2
;;
;;			 3	  *    4
;;			/		\
;;			__________________
curline	.DW 2		;Used for current line where character is being drawn to

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

msg1	.AZ "Welcome! Press 'w' or 's' to move the left paddle, and 'p' or ';' to move the'"

	.BS $20		;32-byte circular input buffer
headptr .DB 0		;Initialize buffer offsets to zero
tailptr .DB 0					

	.BS $0300-*	;New origin. Skip to beginning of program, proper.

;;
;;Clear the screen
;;
start	cld
	lda #' '
.loop1	sta first,y
	iny
	bne .loop1
.loop2	sta second,y
	iny
	bne .loop2
.loop3	sta third,y
	iny
	bne .loop3
.loop4	sta fourth,y
	iny
	bne .loop4
	jsr welcome
	;jsr inipad	;To be used once we remove it from main1 in order to initialize it
	jsr initirv	;Initialize ACIA and IRQ vectors.
	jmp main	;Then main, waiting for interrupt.


welcome	
	rts

;;
;;	Infinite main loop, waiting for interrupt.
;;
main	jsr drwpuck
	jsr inipad	;For now, we call this so the puck doesn't overwrite the paddle on the screen
;;
;;	Get one character from the buffer, if there's one there.
;;
getch	lda tailptr
	cmp headptr	;Check pointers.
	beq empty	;If equal, buffer is empty.
	tax
	lda inbuff,x	;Get the character.
	jsr movepad	;Process the character.
	inc tailptr	;Increment the offset.
	lda tailptr
	and #%00011111	;Clear high 3 bits to make buffer circular.
	sta tailptr
	jmp getch
empty	jmp main

;;
;;	IRQ handler. Invoked by CPU when a byte is ready to be read
;;	This code must precede initirv in assembly-code file.
;;
irhand	pha ;Save registers.
	txa
	pha
	tya
	pha
	lda headptr ;Get buffer head pointer.
	tax ;Set index register value.
	sec
	sbc tailptr
	and #$1f ;Make circular.
	cmp #$1f ;If headptr - tailptr = 31, buffer is full.
	beq out ;Buffer is full. Can't do anything.
	lda iodata ;Get the character from the keyboard.
	sta inbuff,x ;Store it into the buffer.
	inx ;Next buffer address.
	txa
	and #%00011111 ;Clear high 3 bits to make buffer circular.
	sta headptr
out	pla ;Restore registers
	tay
	pla
	tax
	pla
	cli ;Clear interrupt mask (un-disable)
	rti ;Return from interrupt handler.



;;
;;	Intialize interrupt vector.
;;	This code must follow irhand code in assembly-code file.
;;
initirv lda #%00001001
	sta iocmd	;Set command status
	lda #%00011010
	sta ioctrl	;0 stop bits, 8 bit word, 9600 bps
	lda irhlo	;Get low-byte addr of interrupt handler.
	sta irqvecl	;Store in IRQ and NMI vectors.
	sta nmivecl
	lda irhlo+1	;Get high-byte addr of interrupt handler.
	sta irqvech	;Store it in IRQ and NMI vectors.
	sta nmivech
	lda #$00	;Initialize reset vector.
	sta rstvecl	;Store it in reset vector.
	sta rstvech
	cli		;Enable interrupts.
	rts



movepad	cmp #'w'
	beq lpadup	;Move left paddle up
	cmp #'s'
	beq lpaddn	;Move left paddle down
	cmp #'p'
	beq rpadup	;Move right paddle up
	cmp #';'
	beq rpaddn	;Move right paddle down
	rts

return	rts

;Delete either top or bottom of paddle in preparation of drawing next phase of new paddle position
clrpad	lda #' '
        pha
        tya             ;Transfer paddle position to a
        pha
	txa
        pha
        jsr prch
        rts

;Draw a new part of a paddle
drwpad	pha
	tya		;Transfer left or right paddle position  to a
	pha		;Push paddle position to stack
	txa		;Load column (0 or 39) into a
	pha
	jsr prch
	rts

lpadup	ldy lpaddle	;Load the current position of the left paddle
	cpy #1		;Are we at the top? If so, don't move any higher
	bmi return	;if we are at the top, then rts
	iny
	iny		;Move position of y to bottom of paddle
	ldx #0		;Load column number into x
        jsr clrpad	;Clear the bottom part of the paddle
	dec lpaddle	;Move pointer of paddle position up
	ldy lpaddle	;Reset y to the new location of the paddle
	lda #$F6	;Character for left paddle
	ldx #0
	jsr drwpad	;Draw new part of paddle at bottom
        rts

lpaddn	ldy lpaddle	;Load the current position of the left paddle
	cpy #22		;Are we at the bottom? If so, don't go any lower
	bpl return	;If we are at the bottom, then rts
	ldx #0		;Load column number into x
	jsr clrpad	;Clear the top part of the paddle
	inc lpaddle	;Move pointer of the paddle position down
	ldy lpaddle	;Reset y to the new location of the paddle
	iny
	iny		;Move position of y to bottom of paddle
	lda #$F6	;Character for left paddle
	ldx #0
	jsr drwpad	;Draw new part of paddle at bottom
	rts

rpadup	ldy rpaddle	;Load the current position of the right paddle
	cpy #1		;Are we at the top? If so, don't move any higher
	bmi return	;If we are the top, then rts
	iny
	iny		;Move position of y to bottom of paddle
	ldx #39		;Load column number into x
        jsr clrpad	;Clear the bottom part of the paddle
	dec rpaddle	;Move pointer of paddle position up
	ldy rpaddle	;Reset y to the new location of the paddle
	lda #$F5	;Character for right paddle
	ldx #39
	jsr drwpad	;Draw new part of paddle at bottom
        rts

rpaddn	ldy rpaddle	;Load the current position of the right paddle
	cpy #22		;Are we at the bottom? If so, don't go any lower
	bpl return	;If we are at the bottom, then rts
	ldx #39		;Load column number into x
	jsr clrpad	;Clear the top part of the paddle
	inc rpaddle	;Move pointer of the paddle position down
	ldy rpaddle	;Reset y to the new location of the paddle
	iny
	iny		;Move position of y to bottom of paddle
	lda #$F5	;Character for left paddle
	ldx #39
	jsr drwpad	;Draw new part of paddle at bottom
	rts
		
prch:
	;pull off pointer return address from stack=
	pla
	sta .adrs	;Pointer return address
	pla
	sta .adrs+1
	pla		;Get column, because it's the last thing we pushed
	tay		;Save column
	pla		;Get row
	asl		;Double it by shifting to the left 1 (which multiples by 2)
	tax		;Save row
	lda row1,x
	sta curline
	lda row1+1,x
	sta curline+1
	pla		;Get character ('*' or ' ', for example)
	sta (curline),y	;y is the column number, and we don't need to double it because it's 1 byte per column

	;restore pointer return adress to stack
	lda .adrs+1
	pha
	lda .adrs
	pha

	rts
.adrs	.DW $0000		;pointer return address

;return char
;return what is in that space
;if the place that doesn't have a space, then the puc should not be placed therea nd soemthing else shoudl happen
rtch	pla
	sta .adrs	;Pointer return address
	pla
	sta .adrs+1

	pla		;Get column, because it's the last thing we pushed
	tay		;Save column
	pla		;Get row
	asl		;Double it by shifting to the left 1 (which multiples by 2)
	tax		;Save row
	lda row1,x
	sta curline
	lda row1+1,x
	sta curline+1
	lda (curline),y	;Load into a the value stored at passed in parameters
	pha
	
	;restore pointer return adress to stack
	lda .adrs+1
	pha
	lda .adrs
	pha

	rts
.adrs	.DW $0000		;pointer return address

;Initialize paddles
inipad:	lda #$F6
	pha
	lda lpaddle
	pha
	lda #0
	pha
	jsr prch

	lda #$F6
	pha
	lda lpaddle
	clc
	adc #1
	pha
	lda #0
	pha
	jsr prch

	lda #$F6
	pha
	lda lpaddle
	clc
	adc #2
	pha
	lda #0
	pha
	jsr prch
	


	lda #$F6
	pha
	lda rpaddle
	pha
	lda #39
	pha
	jsr prch

	lda #$F6
	pha
	lda rpaddle
	clc
	adc #1
	pha
	lda #39
	pha
	jsr prch

	lda #$F6
	pha
	lda rpaddle
	clc
	adc #2
	pha
	lda #39
	pha
	jsr prch
	
	rts

drwpuck	inc btimer
	lda btimer
	cmp #100
	beq .skip
	rts
.skip	lda #0
	sta btimer
	lda #' '
	pha
	lda puckrow
        pha
        lda puckcol
	pha
	jsr prch	;Call to draw puckcol
	;Update postion of puckrow and puckcol
	lda puckdir
	cmp #1
	beq move1
	cmp #2
	beq move2
	cmp #3
	beq move3
	cmp #4
	beq move4

move1	ldx puckcol
	cpx #1
	bpl .move11
	lda player1
	pha
	jsr collide
	inc puckdir
	jmp move2	;if at left wall, start moving in direction 2
.move11	lda puckrow 
	cmp #1
	bpl .move12
	inc puckdir
	inc puckdir
	jmp move3	;if at ceiling, start moving in direction 3
.move12	dec puckcol
	dec puckrow
	jmp newpuck
;;
;;puckcol++
;;puckrow--
;;if we hit the right side of the wall, jump to move1 to move the ball up and to the left
;;
move2	ldx puckcol
	cpx #39
	bmi .move21
	lda player2
	pha
	jsr collide
	dec puckdir
	jmp move1	;if at right wall, start moving in direction 1
.move21	lda puckrow
	cmp #1
	bpl .move22
	inc puckdir
	inc puckdir
	jmp move4	;if at ceiling, start moving in direction 4
.move22	inc puckcol
	dec puckrow
	jmp newpuck
;;
;;puckcol--
;;puckrow++
;;if we hit the right side of the wall, jump to move4 to move the ball down and to the right
;;
move3	ldx puckcol
	cpx #1
	bpl .move31
	lda player1
	pha
	jsr collide
	inc puckdir
	jmp move4	;if at left wall, start moving in direction 4
.move31	lda puckrow
	cmp #24
	bmi .move32
	dec puckdir
	dec puckdir
	jmp move1	;if at floor, start moving in direction 1
.move32	dec puckcol
	inc puckrow
	jmp newpuck
;;
;;puckcol++
;;puckrow++
;;if we hit the right side of the wall, jump to move3 to move the ball down and to the left
;;
move4	ldx puckcol
	cpx #39
	bmi .move41
	lda player2
	pha
	jsr collide
	dec puckdir
	jmp move3	;if at right wall, start moving in direction 3
.move41	lda puckrow
	cmp #24
	bmi .move42
	dec puckdir
	dec puckdir
	jmp move2	;if at floor, start moving in direction 2
.move42	inc puckcol
	inc puckrow
	jmp newpuck

newpuck	lda #'*'
	pha
	lda puckrow
        pha
        lda puckcol
	pha
	jsr prch		;Call to draw puckcol
	rts
	
;;
;;Collision
;;Branches to score if there is no collision, returns to subroutine otherwise
;;
collide	lda puckrow
	pha
	lda puckcol
	pha
	jsr rtch	;Returns character at that space
	pla		;Pull the character returned from rtch
	cmp #$f6	;Did we hit a paddle?
	beq score	;If no paddle was hit, then increment appropriate score and reset game
	rts		;If a paddle was hit, then continue gameplay
;;
;;Increments either player1's score or player2's score, and redraws the scores
;;
score	lda puckcol
	cmp #1
	beq .p1sc
	jmp .p2sc
.p1sc	inc player1
	;;TODO: draw new p1sc
	jmp restart
.p2sc	inc player2
	;;TODO: draw new p2sc
	jmp restart
	
;Resets the position of the puck to the middle, it's direction to 1, and jumps to main	
restart	lda #19
	sta puckcol
	lda #13
	sta puckrow
	lda #1
	sta puckdir
	jmp main
;;
;;puckcol--
;;puckrow--
;;if we hit the left side of the wall, jump to move2 to move the ball up and to the right
;;