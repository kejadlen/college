debounce	ldaa	porta
	ldab	#255	; set up the delay
	ldx	#871
	jsr	delay		; delay for one second
	cmpa	porta
	beq	ddone
	jmp	debounce

ddone	anda	#$0f
	rts

	end
