; initialize serial transfer
initsr  ldx #$1000
	ldaa	#$30
	staa	$102b   ; baud register
	ldaa	#$00
	staa	$2c,x   ; sccr1 setup
	ldaa	#$0c
	staa	$2d,x   ; sccr2 setup

	ldaa	$102e   ; purge receive flags
	ldaa	$102f   ; receive data

	rts
	end

