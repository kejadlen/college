SCISUBS	EQU	1	;to prevent further inclusion for AS11 TER_2.09
*******************************************************************
*       Subroutine to initalize(INIT) SCI for serial communications
*       at 8 data, no parity, 1 stop bit. Directly compatible with
*       TERM in PCbug11 and F7 comm in Iasm11.
*
*       All registers returned to calling conditions.
*
*       CEN     9/22/93         Initial release
*******************************************************************
INIT    PSHX            ;SAVE X
        PSHA            ;SAVE A

        LDX     #$1000
        LDAA    #$30
        STAA    $2B,X   ;BAUD REGISTER
        LDAA    #$00
        STAA    $2C,X   ;SCCR1 SCI CONTROL REG 1 SET UP
        LDAA    #$0C
        STAA    $2D,X   ;SCCR2 SCI CONTROL REG 2 SET UP

        LDAA    $102E   ;PURGE RECEIVE FLAGS
        LDAA    $102F   ;AND RECEIVE DATA

        PULA            ;RESTORE A
        PULX            ;RESTORE X
        RTS
******************************************************************
*       Subroutine to receive(REC) a single character from an initalized
*       SCI serial device.  No echo to screen takes place.
*
*       Received character returned in ACCA.  No other registers modified.
*
*       CEN     9/23/93
*	
*       Modified for AS11 TER_2.09 by RBL
******************************************************************
REC     PSHX                    ;Save X register
        LDX     #$1000          ;Point to register bank
NOCH    BRCLR   $2E,X $20 NOCH  ;Test for RDRF receive character in
                                ;SCSR register, 0 no new character.
				;Changed commas to spaces for last 2 arguments
				;for AS11 TER_2.09
        LDAB    $2F,X
        PULX                    ;Restore X register
        RTS
******************************************************************
*       Subroutine to transmit(TRS) character from SCI to serial connected terminal
*
*       Expects character to be displayed to be in ACCA upon entry.
*       No other registers modified.
*
*       CEN     9/23/93
*	
*       Modified for AS11 TER_2.09 by RBL
******************************************************************
output
TRS     PSHX                    ;Save X register
        LDX     #$1000          ;Point to register bank
TBNMT   BRCLR   $2E,X $80 TBNMT ;Loop til transmitter output buffer empty
                                ;Empty if TDRE flag in SCSR register = 1
				;Changed commas to spaces for last 2 arguments
				;for AS11 TER_2.09
        STAB    $2F,X
        PULX                    ;Restore incoming X register
        RTS
*******************************************************************
        END			;Added END statement for AS11 TER_2.09
