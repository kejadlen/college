; get input from serial, place into buffer
; given buffer in ix, buffer length in a
; iy set to $1000, buffer label at buffer
getbuf
	clra
	ldy	#$1000
	ldx	#buffer

	; get input
gbwait	brclr   $2e,y $20 gbwait
	ldab	$2f,y

	; finish input if CR is detected
	cmpb	#$0d
	beq buffull

	; don't store if 16 characters
	; have already been entered
	inca
	cmpa	#16
	bgt	nostore

	stab	0,x ; store input

nostore jsr output  ; echo input
	inx

	jmp gbwait

	; CR has been hit
buffull
	staa	buflen

	jsr crlf

	rts
	end

