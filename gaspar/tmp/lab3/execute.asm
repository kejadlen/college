; execute

; If the current state is state 3, store the
; current character in bufchar.

; If the current state is state 4, perform
; operations based on the keypress
; stored in oldchar.  (Which is actually
; the current character, from compute.)

; Input: IX holds the current state (from dsm)
; Uses: ACCA, ACCB
; Output: N/A

; Presets: buffer	rmb	8
;	buflen	rmb	1
;	bufchar	rmb	1

execute
	; display and store character to be buffered
	brset	0,x 2 xstore

	; if the current state isn't 4, nothing
	; needs to be done here
	brset	0,x 4 xstart
	rts

xstart
	; reset the current state
	ldaa	#0
	staa	curstate

	; for the various buffer functions
	ldaa	bufchar
	ldab	buflen
	ldy	#buffer
	
	; * causes the buffer to be displayed
	cmpa	#14
	beq	buffout

	; # causes the buffer to be zeroed
	; and then displayed
	cmpa	#15
	beq	buf0

	jmp	buffin

buf0	clr	buflen
	rts

xstore	ldab	oldchar
	cmpb	#16
	beq	nochar

	stab	bufchar	; store char
	aslb
	aslb
	aslb
	aslb
	stab	porta	; display char

	rts

nochar	ldab	#0
	stab	porta
	rts

	include <buffer.asm>

	end
