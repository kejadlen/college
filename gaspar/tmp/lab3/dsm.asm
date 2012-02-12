; dsm

; This function takes an input, finds what state it should
; go to based on the input and current state through the
; state table, and then sets the current state appropriately.

; Input: ACCA
; Uses: ACCB, IX
; Output: -- curstate

; Presets:	curstate	rmb	1

dsm	
	ldab	curstate

	; next state is at b*4 + a
	; in the state table
	aslb
	aslb
	aba

	tab	; result is in a, but needs
		; to be in b for abx

	; get the next state
	ldx	#statetable
	abx
	ldab	0,x

	stab	curstate	; set the state

	rts

statetable	fcb	1, 1, 0, 0, 0, 2, 0, 0, 0, 2, 3, 3, 0, 0, 4, 4

	end
