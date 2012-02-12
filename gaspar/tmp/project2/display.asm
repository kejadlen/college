display	ldaa	$0,y
;	beq	dispten

	staa	portb

	ldab	#255
	ldx	#3921
	jsr	delay

dispten	ldaa	$1,y
;	beq	dispone
	staa	portb

	ldab	#255
	ldx	#3921
	jsr	delay

dispone	ldaa	$2,y
	staa	portb

	ldab	#255
	ldx	#3921
	jsr	delay

	rts
	end
