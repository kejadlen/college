; examine subroutine
; uses:
;	ACCB, IX, and IY
; labels:
;	errob rmb 1 (set by getadd)
;	buflen	rmb	1 (holds current buffer length)

examine
	; get address
	jsr	getadd

	; make sure address is valid
	ldy	#error
	brset	0,y	$01	exbadd
	tst	buflen
	bgt	exbadd

	; print address
	ldx	#addy
	jsr	pradd
	inx
	jsr	pradd

	ldab	#$20
	jsr	output
	jsr	output

	ldx	addy

	; print four bytes of data
	ldaa	#4
exloop
	; print what's in the address
	jsr	pradd

	ldab	#$20
	jsr	output

	; go to next address
	inx

	deca
	bgt	exloop

	jsr	crlf
	rts

	; branch here to handle bad addresses
exbadd
	jsr	badadd
	rts

	end

