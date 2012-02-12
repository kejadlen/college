bintodec	clrb
hun	suba	#100
	bmi	hundone
	incb
	jmp	hun
hundone	adda	#100
	stab	$0,y

	clrb
ten	suba	#10
	bmi	tendone
	incb
	jmp	ten
tendone	adda	#10
	stab	$1,y

	clrb
one	suba	#1
	bmi	onedone
	incb
	jmp	one
onedone	adda	#1
	stab	$2,y	

	rts
	end
