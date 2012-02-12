;Delay Subroutine
;Author: J.R. Armstong
;Date: 08-01-03
;Description:
;Delays by an ammount determined by the initial
;contents of the B register and index register IX
;NC is the number of clock cycle delay
;NC = B(6X+10)+10 
;where B and X(IX) are the intial values of these
;registers
;Input Parameters: Contents of B and IX
;Output Parameters: None
;Register Effected: B,IX, and CCR
;Code: 
delay  	stx xlim 	;Store initial IX
xcnt   	ldx xlim 	;Restore initial IX
xloop  	dex      	;inner loop
       	bne xloop
       	decb     	;outer loop
  	bne  xcnt
  	rts           	;return
        end




 
.
    
    
