	org $120

buffer	rmb	16	; holds the input
buflen	rmb	1	; length of input
addy	rmb	2	; holds the address
error	rmb	1	; error bit for getadd
xlim	rmb	2	; general temporary storage
			;	(also for delay)

	; initialization
	org	$b600

	lds	#$01ff	; initialize stack
	jsr	initsr	; initialize serial connection

	jmp	$2000	; jump to main program

	; modify and del_sub are in RAM because
	; writing to EEPROM are done during these
	; routines, and we cannot read and write
	; from EEPROM at the same time.

	include	<modify.asm>
	include	<del_sub.asm>

	; main program (external memory)
	org $2000

main
	clr	error	; clear the error bit

	; output the carat
	ldab	#'>
	jsr	output

	jsr	getbuf	; get the input

	; if no input, no need to
	; check for commands
	tsta
	beq	main

	jsr	findfunc	; figure out what
				; function to run

	jmp	main



; error messages -- both use IY
; badcom -- "Invalid Command"
; badadd -- "Invalid Address"
badcom
	ldy	#bcstr
	jsr	outstr
	jsr	crlf
	rts

badadd
	ldy	#bastr
	jsr	outstr
	jsr	crlf
	rts

; strings for output -- bounded
; with a period
bastr	fcc	/Invalid Address./
bcstr	fcc	/Invalid Command./

	include	<examine.asm>
	include <getbuf.asm>
	include <getadd.asm>
	include <findfunc.asm>
	include <initsr.asm>
	include <output.asm>
	include <add.asm>
