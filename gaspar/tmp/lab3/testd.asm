porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$120

curstate	fcb	1
input	fcb	0

	org	$b600

	lds	#$01ff

	; set ports A and D to the
	; correct directions
	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c

	ldaa	input

	jsr	dsm
	ldaa	curstate
	asla
	asla
	asla
	asla
	staa	porta

	include <dsm.asm>

	end
