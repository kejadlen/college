porta	equ	$1000
portb	equ	$1004
pactl	equ	$26

	org	$120

ram	rmb	3
xlim	rmb	2

	org	$b600

	clr	$1020
;	ldy	#$1000
;	bset	pactl,y $80

	ldy	#ram

;loop	jsr	debounce
loop	ldaa	porta
	jsr	fibonaci
	jsr	bintodec
	jsr	display
	jmp	loop

	rts

	end

	include	<debounce.asm>
	include <del_sub.asm>
	include <fibonaci.asm>
	include <bintodec.asm>
	include <display.asm>
