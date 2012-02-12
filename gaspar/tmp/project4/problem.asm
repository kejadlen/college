	org $120

buffer	rmb	16	; holds the input
	fcc	/./
buflen	rmb	1	; length of input
addy	rmb	2	;
error	rmb	1
bcstr	fcc	/Invalid Command./
bastr	fcc	/Invalid Address./
bdstr	fcc	/Invalid Data./
ostr	fcc	/Overflow!./
xlim	rmb	2

	; initialization
	org	$b600

	lds	#$01ff	; initialize stack
	jsr	initsr	; initialize serial connection

	jmp	$2000	; jump to main program

	; main program
	org $2000

main
	clr	error

	; output the carat
	ldab	#'>
	jsr	output

	jsr	getbuf

	tsta
	beq	main

	ldy	#buffer
	jsr	outstr

	jmp	main

	include <getbuf.asm>
	include <initsr.asm>
	include <output.asm>

