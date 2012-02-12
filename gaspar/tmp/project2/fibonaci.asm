fibonaci	ldab	#1
	stab	$2,y

	cmpa	#2
	bmi	fdone
	deca

	stab	$0,y
	stab	$1,y

floop	ldab	$0,y
	addb	$1,y
	stab	$2,y
	deca
	beq	fdone
	ldab	$1,y
	stab	$0,y
	ldab	$2,y
	stab	$1,y
	jmp	floop

fdone	ldaa	$2,y
	rts

	end
