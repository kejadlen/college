adinit	
	pshx
	ldx	#$1000
	bset	$39,x $80	;adpu power up
	bclr	$39,x $40	'select E clock
	ldab	#4
	ldx	#6
	jsr	delay
	pulx
	rts

adread	
	pshx
	pshy
	ldx	#$1000
	ldy	adbuff
	
	ldaa	#$01
	anda	#$07
	oraa	#$20
	staa	$30,x

adcont	
	brclr	$30,x $80 adcont
	ldaa	$31,x
	staa	$0,y
	ldaa	$32,x
	staa	$1,y
	ldaa	$33,x
	staa	$2,y
	ldaa	$34,x
	staa	$3,y
	jsr	adavg

	puly
	pulx
	rts

adavg	clra
	clrb
	addb	0,y
	addb	1,y
	bcc	nextone
	inca
nextone	addb	2,y
	bcc	nexttwo
	inca
nexttwo	addb	3,y
	bcc	nextthr
	inca
nextthr	lsrd
	lsrd
	stab	4,y

	rts
	end
