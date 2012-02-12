; outone

; takes ACCA as input from a/d,
; outputs the ones place, assuming
; a range from 0 to 5

outone	pshx
	pshb
	ldab	#4
	ldx	#onecheck

	; KLUDGE

	tsta
	bmi	onestart
	cmpa	#76
	bgt	onestart

	inx
	inx
	inx

	subb	#3

onestart
	cmpa	0,x
	bpl	onedone
	decb
	beq	onedone
	inx
	jmp	onestart

onedone
;	jsr	output
;	ldab	#'.
	pulx
	pulb
	rts

onecheck	fcb	205,154,102,51

; outdec

; outputs the decimal value of
; IX... range from 0 to 5. Uses
; ACCA

outdec
	pshb
	clra
	
	brclr	0,x	$80	seven
	adda	#50
seven
	brclr	0,x	$40	six
	adda	#25
six
	brclr	0,x	$20	five
	adda	#63
five
	brclr	0,x	$10	four
	adda	#31
four
	brclr	0,x	$08	three
	adda	#16
three
	brclr	0,x	$04	two
	adda	#8
two
	brclr	0,x	$02	one
	adda	#4
one
	brclr	0,x	$01	zero
	adda	#2
zero
	tab

	tsta
	bmi	decsub

	cmpa	#100
	bpl	decsub

	jmp	decdone

decsub	suba	#100
	jmp	zero

decdone	daa
	pulb
	rts

;	clra
;decsub
;	tap
;	subb	#100
;	bmi	decsub
;	cmpb	#100
;	bgt	decsub

	; now B is reduced to the ones
	; and tens place -- do as you wish

;	tba
;	daa
;	rts
	end
