rtiinit
	ldx	#$1000
	bset	tmsk2,x	$40
	cli
	clr	fsec
	clr	fsec+1
	clr	sec
	ldx	#$00EB
	ldaa	#$7e
	staa	0,x
	ldaa	#$b7
	staa	1,x
	ldaa	#$b0
	staa	2,x
	rts
rrtii
	ldx	#$1000
	ldaa	#$40
	staa	tflg2,x
	ldy	fsec
	iny
	sty	fsec
	cpy	#fcnt
	beq	seconds
	rti
seconds	
	clr	fsec
	clr	fsec+1
	ldaa	sec
	inca
	cmpa	#2
	beq	execute
	staa	sec
	rts
execute
	ldx	#$1000
	clr	sec
	ldaa	portb
	coma
	staa	portb

	ldaa	hexdisp
	lsra
	lsra
	lsra
	lsra
	inca
	cmpa	#10
	bmi	contin
	clr	hexdisp
contin	lsla
	lsla
	lsla
	lsla
	staa	$1003
	jsr	outdata
	rts

	end	