porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$b600

	lds	#$01ff

	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c
	
start	jsr	getkey

	aslb
	aslb
	aslb
	aslb
	stab	porta

	jmp	start

	include <getkey.asm>

	end
