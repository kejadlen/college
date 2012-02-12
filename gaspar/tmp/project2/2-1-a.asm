porta	equ	$1000
portb	equ	$1004
pactl	equ	$26

	org	$120

	clr	$1020		; next three statements turn PA3 to input
	ldy	#$1000
	bset	pactl,y $80

start	ldaa	#porta
	anda	#$0f		; mask porta with 00001111
	staa	portb
	jmp	start
	rts
	end
