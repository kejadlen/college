; ascii to integer, from acca
atoi
	suba	#'0
	cmpa	#10
	bmi	aidone
	suba	#7
	cmpa	#$2a
	bmi	aidone
	suba	#$20
aidone	rts

; gets the address from the buffer,
; returns it in addy
; uses:
;	ACCA, ACCB, IX
; labels:
;	buflen	rmb	1

getadd
	pshx	; store ix

	ldab	#4	; four bytes max

gastart
	ldaa	0,x

	; test for space
	cmpa	#$20
	beq	gacont

	; test for end of buffer
	dec	buflen
	bmi	gacont

	jsr	atoi

	; make sure the nibble is valid
	cmpa	#$0f
	bgt	gabadd
	
	staa	0,x

	inx

	decb
	bgt	gastart

	; the nibbles have now been converted
	; from characters to numbers
gacont
	; figure out how many digits were inputted
	negb
	addb	#4
	beq	gabadd

	; get the original location of IX
	pulx

	clr	addy
	clr	addy+1

galoop
	; shift high byte
	lsl	addy
	lsl	addy
	lsl	addy
	lsl	addy

	; high nibble of low byte
	; into low nibble of high byte
	ldaa	addy+1
	lsra
	lsra
	lsra
	lsra
	adda	addy
	staa	addy

	; shift low byte
	lsl	addy+1
	lsl	addy+1
	lsl	addy+1
	lsl	addy+1

	; put digit into address
	ldaa	0,x
	adda	addy+1
	staa	addy+1

	inx

	decb
	bgt	galoop

	rts

	; bad address
gabadd
	ldab	#1
	stab	error
	pulx
	rts

; prints what IX is pointing to
pradd
	; print high nibble
	ldab	0,x
	lsrb
	lsrb
	lsrb
	lsrb

	jsr	itoa
	jsr	output
	
	; print low nibble
	ldab	0,x
	andb	#$0f

	jsr	itoa
	jsr	output

	rts

; converts ACCB from a number
; to a character
itoa
	cmpb	#10
	bmi	digit
	addb	#7
digit
	addb	#'0
	rts

	end

