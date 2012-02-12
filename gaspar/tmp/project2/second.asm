porta	equ	$1000
portb	equ	$1004
pactl	equ	$26

	org	$120

	clr	$1020		; next three statements turn PA3 to input
;	ldy	#$1000
;	bset	pactl,y $80

start	jsr	debounce	; as long as the value is the same for at least a second
	staa	portb
	jmp	start

	rts

xlim	rmb	2
	include <debounce.asm>
	include <del_sub.asm>

	end
