; based on the input string, runs the proper 
; command
; uses:
;	ACCA, ACCB, IX
; labels:
;	buffer	rmb	16
;	buflen	rmb	1

findfunc
	ldx	#buffer

	; check for space
	ldab	#$20
	cmpb	1,x
	bne	gberr

	; we're done with the first two characters
	dec	buflen
	dec	buflen

	ldaa	0,x	; for checking the command
	inx	; pointing at addresses
	inx

	; 'E'xamine
	cmpa	#'E
	beq	gbex

	; 'M'odify
	cmpa	#'M
	beq	gbmod	

	; 'A'dd
	cmpa	#'A
	beq	gbadd

	; bad command
gberr
	jsr	badcom
	rts

	; bad address
gbbadd
	jsr	badadd
	rts

gbex
	jsr	examine
	rts

gbmod
	jsr	modify
	rts

gbadd
	jsr	add
	rts

	end

