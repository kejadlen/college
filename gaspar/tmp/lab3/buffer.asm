; Set of buffer functions.  These
; all assume that buffer is a set of
; 8 bytes in RAM, and buflen is
; a location in RAM which holds
; the number of items currently in
; the buffer.

; IY holds the buffer location,
; ACCB holds the current length
;	of the buffer,
; ACCA holds the current character



; buffin

; Places a value into the buffer.  If the
; buffer is full, remove the oldest character
; and put the new one in.  Afterwards,
; display the buffer.

buffin
	; Case when the buffer is full
	cmpb	#8
	beq	bigbuf	

	; If the buffer isn't full, just add
	; add the current character in
	aby
	staa	0,y
	incb
	stab	buflen

	rts

bigbuf
	; Shift the buffer to make room
	; for the new character.  The
	; oldest character (in the 0th
	; position) is deleted.
	ldab	1,y
	stab	0,y
	ldab	2,y
	stab	1,y
	ldab	3,y
	stab	2,y
	ldab	4,y
	stab	3,y
	ldab	5,y
	stab	4,y
	ldab	6,y
	stab	5,y
	ldab	7,y
	stab	6,y

	; place the new value
	; into the buffer
	staa	7,y

	rts


; buffout

; Outputs each number in the buffer to port A,
; one second at a time.

buffout
	; if nothing in buffer, display 0
	tstb
	beq	bodone

	; delay function uses ACCB, so buflen
	; is placed into ACCA instead
	ldaa	buflen

boloop
	; output the current character
	ldab	0,y
	aslb
	aslb
	aslb
	aslb
	stab	porta

	ldx	#1307
	ldab	#255
	jsr	delay

	iny
	deca

	beq	bodone
	jmp	boloop

bodone	ldab	#0
	stab	porta

	rts

	end
