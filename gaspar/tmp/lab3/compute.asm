; compute

; Computes what the input to the dsm
; function should be, based on the key
; received from getkey.  Also changes
; the oldchar to the current character for
; the next instance of compute.

; Output:
;	00	keypress, not the same as before
;	01	keypress, same as the previous
;	10	no keypress

; Input: ACCB holds the current character,
;	oldchar holds the previous character
; Uses: N/A
; Output: ACCA

; Presets:	oldchar	rmb	1

compute
	; start at 0
	ldaa	#0

	cmpb	#16
	beq	nokey

	cmpb	oldchar
	beq	same

	stab	oldchar
	rts	; 00

nokey	inca	; 10
same	inca	; 01

	stab	oldchar
	rts

	end
