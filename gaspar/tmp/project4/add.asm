; Used for branching from add
; to output a bad address error
; message. This is up here because
; of branch command limitations
adderr
	jsr	badadd
	rts

; add function
; Uses IY, IX, ACCA, ACCB
; Locations in memory to be set:
;	error (1 byte)
;	xlim (2 bytes)
add
	; get the first addend
	jsr	getadd
	ldy	#error
	brset	0,y	$01	adderr

	; only argument
	tst	buflen
	bmi	adderr

	; address > 4 bytes
	ldab	0,x
	cmpb	#$20
	bne	adderr

	; get and store first term
	ldy	addy
	ldaa	0,y
	staa	xlim

	; move onto next address
	inx
	
	; same as above code; get address
	; and make sure that it's valid
	jsr	getadd
	ldy	#error
	brset	0,y	$01	adderr

	tst	buflen
	bmi	adderr

	ldab	0,x
	cmpb	#$20
	bne	adderr

	; get second term
	ldy	addy
	ldab	0,y
	stab	xlim+1

	; move onto next address
	inx

	; same as the above...
	jsr	getadd
	ldy	#error
	brset	0,y	$01	adderr

	tst	buflen
	bmi	adderr

	ldx	addy

	; get the old addends back
	ldaa	xlim
	ldab	xlim+1
	aba

	bvc	addgo

	; if there's overflow
	ldy	#ostr
	jsr	outstr
	jsr	crlf

addgo
	; store data
	staa	0,x

	ldab	#'>
	jsr	output

	; output address of sum
	ldx	#addy
	jsr	pradd
	inx
	jsr	pradd

	ldab	#$20
	jsr	output
	jsr	output

	; output the sum
	ldx	addy
	jsr	pradd

	jsr	crlf

	rts

	; strings used
bdstr	fcc	/Invalid Data./
ostr	fcc	/Overflow!./

	end
