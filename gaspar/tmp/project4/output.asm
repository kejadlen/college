; output crlf
crlf
	; CR
	ldab	#$0d
	jsr	output

	; LF
	ldab	#$0a
	jsr	output

	rts

; output what is in b
output
	pshy
	ldy	#$1000

	; wait until serial buffer is empty
owait
	brclr   $2e,y   $80 owait
	stab	$2f,y
	puly
	rts

; output a string, pointed to
; by IY, ending with '.'

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

