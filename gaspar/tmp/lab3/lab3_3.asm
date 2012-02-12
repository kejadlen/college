porta	equ	$1000
outport	equ	$1008
ddrd	equ	$9
inport	equ	$a
pactl	equ	$26

	org	$120

curstate	fcb	0
oldchar	fcb	16
bufchar	rmb	1
buffer	rmb	8
buflen	fcb	0
xlim	rmb	2	; for delay

	org	$b600

	lds	#$01ff
	jmp	$2000

	org	$2000

	; set ports A and D to
	; the correct directions
	clr	$1020
	ldy	#porta
	bset	pactl,y $80
	bset	ddrd,y $3c

start
	jsr	getkey	; stores key in ACCB
	jsr	compute	; stores state in ACCA
	jsr	dsm	; stores data in curstate (and b and 0,x)
	jsr	execute	; performs operations based on the state

	; b and ix need to be set for delay here
	; delay for max(make, break)

	ldx	#1307
	ldab	#5
	jsr	delay

	jmp	start

	end

	include <getkey.asm>
	include <compute.asm>
	include <dsm.asm>
	include <execute.asm>
	include <del_sub.asm>
