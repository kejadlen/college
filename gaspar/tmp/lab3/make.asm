porta	equ	$1000
ddrd	equ	$9
portd	equ	$1008
	org	$120

	clr	$1020
	ldy	#porta
	bset	ddrd,y $3c

loop	ldaa	#$00
	staa	portd
	jmp	loop
	rts
	end
