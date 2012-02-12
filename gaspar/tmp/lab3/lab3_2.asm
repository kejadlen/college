; program 3_2

; Sends a walking 0 through ports 2 through 5
; of the outport, and reads the keypress through
; ports 4 through 7 of the inport.  Then displays
; the current key pressed.

porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$b600
	; initialize stack,
	; go to external memory

	lds	#$01ff
	jmp	$2000

	; actual program stuff
	org	$2000

	; set ports A and D to the
	; correct directions
	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c

; get key
start
	; initialize to start at row 1
	ldx	#rowdata
	ldab	#1

gkloop
	ldaa	0,x
	asla
	asla
	staa	outport

	brset	inport,y $f0 noinput
	jmp	convert

noinput
	inx
	addb	#3

	; if we are not on row 4,
	; keep going
	cmpb	#13
	bne	gkloop

	; no input found, defaulting to 0
	ldab	#0
	jmp	display

convert
	; row 4 is a special case
	cmpb	#10
	beq	row4

	; col 4 is a special case
	brclr	inport,y $10 col4

	; find which column is pressed
	brclr	inport,y $80 col1
	brclr	inport,y $40 col2
	brclr	inport,y $20 col3

	; figure out the appropriate output
col3	incb		; 3, 6, or 9
col2	incb		; 2, 5, or 8
col1	jmp	display	; 1, 4, or 7

	; handling row 4,
	; find out which column is pressed
row4	brclr	inport,y $80 r4c1
	brclr	inport,y $40 r4c2
	brclr	inport,y $20 r4c3
	brclr	inport,y $10 r4c4

	; figure out the appropriate output
r4c4	subb	#2	; D
r4c3	addb	#15	; # (E)
r4c2	subb	#14	; 0
r4c1	addb	#4	; * (14)
	jmp	display

col4	ldab	#10
	brclr	0,x $8 c4r1
	brclr	0,x $4 c4r2
	brclr	0,x $2 c4r3

c4r3	incb	; 12
c4r2	incb	; 11
c4r1	jmp display	; 10

display
	; shift key to ports 4-7
	aslb
	aslb
	aslb
	aslb

	stab	porta

	jmp	start	; start over

rowdata
	fcb	7, 11, 13, 14
