; output crlf
crlf
	ldab	#$0d
	jsr output
	ldab	#$0a
	jsr output
	rts

; output what is in b
output
	pshy
	ldy	#$1000
owait
	brclr   $2e,y   $80 owait
	stab	$2f,y
	puly
	rts

outstr
	ldab	0,y
	cmpb	#'.
	beq	strdone
	jsr	output
	iny
	jmp	outstr
strdone
	rts

	end

