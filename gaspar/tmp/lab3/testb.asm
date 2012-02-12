porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$120

buffer	rmb	8
bufchar	rmb	1
buflen	rmb	1
xlim	rmb	2

	org	$b600

	lds	#$01ff

	; set ports A and D to the
	; correct directions
	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c

start	ldaa	bufchar
	ldab	buflen
	ldy	#buffer

	jsr	buffin

	ldaa	bufchar
	ldab	buflen
	ldy	#buffer

	jsr	buffout

	rts

	include <buffer.asm>
	include <del_sub.asm>

	end
