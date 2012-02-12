; modify subroutine
; uses:
;	ACCB, IX, IY

; bad address branch located
; here because branches can
; only jump 128 bytes
mbadd
	jsr	badadd
	rts

; actual modify starts here
modify
	jsr	getadd

	; make sure address is valid
	ldy	#error
	brset	0,y	$01	mbadd
	tst	buflen
	bgt	mbadd

	; make sure addy 
	; can be compared
	; 0000 - 01ff or 2000 - 3fff
	ldd	addy
	bmi	mbadd
	subd	#$01ff
	ble	mcont
	subd	#$1e01
	bmi	mbadd
	subd	#$1fff
	bgt	mbadd

mcont
	; print the address
	ldx	#addy
	jsr	pradd
	inx
	jsr	pradd

	ldab	#$20
	jsr	output
	jsr	output

	ldx	addy

	jsr	pradd	; print what's in the address

	ldab	#$20
	jsr	output
	ldab	#'?
	jsr	output
	ldab	#$20
	jsr	output

	jsr	getbuf

	; inelegant -- this must take an input
	; like XX right now.
	beq	mdone
	cmpa	#2
	bne	mbadat

	ldy	#buffer

	; get high nibble and convert to a number
	ldaa	0,y
	jsr	atoi

	; check for invalid nibble
	cmpa	#$0f
	bgt	mbadat

	lsla
	lsla
	lsla
	lsla
	staa	0,y

	; get low nibble and convert to a number
	ldaa	1,y
	jsr	atoi

	; check for invalid nibble
	cmpa	#$0f
	bgt	mbadat

	; create full byte
	adda	0,y

	ldy	addy

	; check to see if byte goes into
	; ram or external memory
	ldx	#addy
	brclr	0,x	$20	mram

	; external -- store and delay
	staa	0,y
	ldx	#50
	ldab	#25
	jsr	delay

mdone
	rts

	; ram -- store and return
mram
	staa	0,y
	rts

	; bad data
mbadat
	ldy	#bdstr
	jsr	outstr
	jsr	crlf
	rts

	end

