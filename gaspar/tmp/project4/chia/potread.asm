	org	$120
sec	rmb	1
fsec	rmb	2
hexdisp	rmb	1
xlim	rmb	2
adbuff	rmb	5

porta	equ	$00
portb	equ	$04
pactl	equ	$26
tflg2	equ	$25
tmsk2	equ	$24
tmsk1	equ	$22
fcnt	equ	244

strsamp	fcc	/Sample ./
strvolt	fcc	/ Volatge is: ./
stravg	fcc	/The average of the voltage measured on Pin E1 is ./

	org	$b600

	lds	#$01ff
	clr	$1020
	ldy	#$1000
	bset	$26,y $80

	clr	hexdisp
	ldaa	#$FF
	staa	portb,y

	jsr	initsr
	jsr	adinit
	jsr	rtiinit
main	jsr	main		
	rts

outdata	
	ldab	#'1
	ldx	adbuff
loopout	ldy	strsamp
	jsr	outstr
	jsr	output
	ldy	strvolt
	jsr	outstr
	ldaa	0,x
	jsr	outone
	jsr	outdec
	jsr	crlf
	inx
	incb
	cmpb	#'5
	beq	outavg
	jmp	loopout

outavg	ldy	stravg
	ldaa	0,x
	jsr	outone
	jsr	outdec
	jsr	crlf
	jsr	crlf	
			
	end
	
	include <output.asm>
	include <del_sub.asm>
	include <initsr.asm>
	include <chia.asm>
	include <analog.asm>
	include <trii.asm>
