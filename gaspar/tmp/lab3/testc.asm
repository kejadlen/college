porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$120

oldchar	fcb	5

	org	$b600

	lds	#$01ff

	; set ports A and D to the
	; correct directions
	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c

start	jsr	getkey
	jsr	compute
	asla
	asla
	asla
	asla
	staa	porta
	jmp	start

	include <getkey.asm>
	include <compute.asm>

	end
