;PROMTEST
;February, 1999, Troy Berg, Bob Lineberry
;December, 2000, Dr. William Baumann fixed 3 bugs
;February, 2003, Changed ORG to $4000 for Dr. Armstrong
;September, 2003, Changed ORG to $2000 for Dr. Armstrong
;Bradley Department of Electrical & Computer Engineering 
*******************************************************************
* Use CALL B600 to execute from DevHC11 Terminal view
*******************************************************************
* code was rearranged and tricks used to fit it into $1ff
*******************************************************************
;Tests 68HC11 EVBU expanded mode EEPROM from $2000 to $3fff [mark]
;First pass: 4 data patterns to adjacent addresses
;address incremented by $7f
;Second pass: 1 data pattern to adjacent addresses
;all addresses tested
;Write:Read compare errors printed to terminal
*******************************************************************
;[mark]
;Successful test looks like:
;>call b600
;CALL B600
;INC AD $7F
;AD WR RD: 2000 FF FF, 2001 00 00  AD WR RD: 3FC0 FF FF, 3FC1 00 00
;AD WR RD: 2000 00 00, 2001 FF FF  AD WR RD: 3FC0 00 00, 3FC1 FF FF
;AD WR RD: 2000 55 55, 2001 AA AA  AD WR RD: 3FC0 55 55, 3FC1 AA AA
;AD WR RD: 2000 AA AA, 2001 55 55  AD WR RD: 3FC0 AA AA, 3FC1 55 55
;INC AD $01
;AD WR RD: 2000 AA AA, 2001 55 55  WAIT 35s
;AD WR RD: 3FFE AA AA, 3FFF 55 55
;OK!
;P-B600 Y-4001 X-B7E2 A-2E B-FF C-D4 S-0041 
;>
*******************************************************************
* Error looks like:
* BAD WIRING OR CHIP
*******************************************************************
*******************************************************************
* SET UP VARIABLES IN HC11 RAM
*******************************************************************
        org     $100
ADDR1   rmb     2       ;x test address
WRT1    rmb     1       ;written to x address
RD1     rmb     1       ;read from x address
ADDR2   rmb     2       ;y test address
WRT2    rmb     1       ;written to y address
RD2     rmb     1       ;read from y address

ADRCNTR rmb     1       ;tracks # of address increments
DATA    rmb     2       ;holds test data for x and y addresses
DATACNT rmb     1       ;number of test data patterns
OFFSET  rmb     1       ;value to increment address
DISPFLG rmb     1       ;number of test data patterns left

*******************************************************************
* CODE RESIDES IN HC11 EEPROM
*******************************************************************
        org     $b600
*        lds     #$01ff  ;Use CALL B600. Let Buffalo handle the stack.
        bra     GO

	include <scisubs.inc>    ;for terminal writes

TOPMEM  equ     $3fff   ;top of external memory [mark]
BOTMEM  equ     $2000   ;bottom of external memory [mark]

CRLF    ldaa    #$0d       ; cr-lf to terminal
        bsr     TRS
        ldaa    #$0a
        bsr     TRS
        rts

ERROR   ldx     #MSG4      ;Bad wiring or EEPROM
        bsr     WRTMSG     ;writes string to terminal
        bsr     CRLF
        jsr     DISPRAM    ;writes current address & data to terminal
        jmp     STOP	   ;changed to jump so we get back to buffalo  WTB
*******************************************************************
* WRTMSG uses TRS to send string in x to terminal
*******************************************************************
WRTMSG  ldaa    $00,x   ;read char into a
        cmpa    #'.'    ;is it the end of string (.)?
        beq     eomsg
        bsr     TRS     ;output character to terminal
        inx
        bra     WRTMSG
eomsg  rts

HOWMANY ldaa    #$41       ;number of $7fs in $1fff
        staa    ADRCNTR    ;how many address increments now
        rts

*******************************************************************
* MAIN TEST ROUTINE
*******************************************************************

GO      jsr     INIT     ;setup terminal writes
        jsr     DELPT15  ;DELAY while terminal starts

STARTVALS:

        bsr     HOWMANY         ;loads # of $7f increments in $1fff

        ldd     #$ff00          ;initial data values
        std     DATA            ;for data pattern change
        staa    WRT1            ;for write:read compare
        stab    WRT2            ;for write:read compare

        ldaa    #$03            ;for 4 data patterns
        staa    DATACNT         ;tracks data to test
        staa    DISPFLG         ; so they match for initial display
*******************************************************************
* this writes INC AD BY $7F to terminal
*******************************************************************
        ldx     #CNTMSG         ;message format
        bsr     WRTMSG          ;to terminal
        ldaa    #$7F            ;initial address increment
        staa    OFFSET
        jsr     ASCII           ;convert hex in acca and write terminal
        bsr     CRLF
*******************************************************************
        jsr     ST2K            ;x to $2000, y to $2001 [mark]

TOP     ldaa    WRT1            ;first datum value to a
        ldab    WRT2            ;first datum value to b
        stx     ADDR1           ;starting test address
        sty     ADDR2           ;starting test address +1

*******************************************************************
* Next 2 lines write to external memory
* Process:  Write byte in ACCA to external memory location pointed to
*               by index register X
*           Perform 4 ms DELAY
*******************************************************************
        staa    $00,x
        jsr     DELAY           ;4 ms DELAY for EEPROM
*******************************************************************
        stab    $00,y           ;x's memory location +1
        jsr     DELAY           ;4 ms DELAY for EEPROM

        ldaa    $00,x           ;read x's memory location
        staa    RD1             ;variable for compare
        ldab    $00,y
        stab    RD2

        cmpa    WRT1            ;compare data written:read
        beq     CMP1            ;if x's memory:write=read value, test y's
        bra     ERROR           ;display mismatch and halt -- changed to branch
				;to avoid return here WTB
CMP1    cmpb    WRT2            ;compare y's memory location
        beq     CMP2
        bra     ERROR           ;display mismatch and return -- changed to branch
				;to avoid return here WTB
CMP2    ldaa    DATACNT         ;which of 4 test patterns are we on
        cmpa    DISPFLG         ;more data test patterns?
        bne     LPNEXT          ;if so, increment test address
        dec     DISPFLG         ;one less data test pattern
        jsr     DISPRAM         ;display address, written data, read data
                                ;from variables in HC11 RAM

LPNEXT  ldaa    #$02            ;offset for contiguous address test
        cmpa    OFFSET          ;are we in contiguous test?
        beq     SKIP            ;if so, skip next two lines
*******************************************************************
        dec     ADRCNTR         ;number of address increments left
        beq     CHGDATA         ;finished testing last data pattern
*******************************************************************
SKIP    ldab    OFFSET          ;offset to increment address for next test
	aby                     ;increment y by offset
	abx			;increment x by offset
	       
EXIT    cpx     #$4000          ;are we at the top of memory? [mark]
        beq     FINISHED        ;end of contiguous test
        jmp     TOP             ;more addresses to test


CHGDATA ldaa    #$00
        cmpa    DATACNT         ;have 4 data patterns been tested?
        beq     MORE            ;if so, start contiguous test

        jsr     DISPRAM         ;if not, display ad wr rd now

        ldd     DATA            ;last data values
        cmpa    #$ff            ;off by one problem
        bne     chg1
        inca                    ;ff=>00
        bra     chg2
chg1    adda    #$55            ;00=>55 or 55=>ff

chg2    cmpb    #$00
        bne     chg3
        decb                    ;00=>ff
        bra     chg4
chg3    subb    #$55            ;55=>00 or ff=>55

chg4    std     DATA            ;store next data pattern
        staa    WRT1            ;prepare for compare
        stab    WRT2

        jsr     ST2K            ;start at memory location $2000 [mark]

        jsr     HOWMANY         ;number of $7fs in $1fff

        dec     DATACNT         ;one less data pattern to test

        jmp     TOP             ;more addresses to test
*******************************************************************
MORE    jsr     DISPRAM         ;start contiguous memory location test
                                ;display ad wr rd at end of previous test
*******************************************************************
* display: INC AD BY $01
*******************************************************************
        ldx     #CNTMSG
        jsr     WRTMSG

        ldaa    #$01
        jsr     ASCII
        jsr     CRLF
*******************************************************************
        ldaa    #$02            ;ADDR1 will now increment by 2
        staa    OFFSET

        jsr     ST2K            ;start ADDR1 at $2000 [mark]

        clra                    ;ldaa #$00
        staa    DATACNT         ;for 1 data pattern
        staa    DISPFLG         ;so they match for initial display

        jmp     TOP             ;more addresses to test

FINISHED:
        jsr     CRLF

        bsr     DISPRAM         ;display last address tested
        ldx     #MSG2           ;Successfully completed all tests
        jsr     WRTMSG
*STOP   bsr     DELPT15         ;allow message before cpu halts in standalone mode
STOP    rts			;Return to Buffalo
*******************************************************************
* END OF MAIN TEST ROUTINE
*******************************************************************
ST2K    ldx     #BOTMEM    ;start test address at $2000 [mark]
        ldy     #BOTMEM+1
        rts
*******************************************************************
DELPT15 pshx
        ldx     #$ffff  ;0.15 s at 2MHz clock
dlylp15 dex
        bne     dlylp15
        pulx
        rts
*******************************************************************
* display current address, data written, data read
*******************************************************************
DISPRAM pshx
        ldx     #MSG1      ;display AD WR RD:
        jsr     WRTMSG
        bsr     SP

        ldx     #ADDR1     ;lesser current address
        bsr     DISPADDR   ;2000 FF FF or similar [mark]

        ldaa    #','
        jsr     TRS
        bsr     SP

        ldx     #ADDR2     ;greater current address
        bsr     DISPADDR   ;2001 00 00 or similar [mark]
        bsr     SP
        bsr     SP

        ldx     #BOTMEM    ;$2000 [mark]
        cpx     ADDR1      ;starting address?
        bne     dr1        ;if not, start new line
        ldaa    #$02
        cmpa    OFFSET     ;are we in contiguous test?
        bne     dr2        ;if not, don't display wait message
        ldx     #MSG3      ;wait, contiguous test takes a while
        jsr     WRTMSG
        bra     dr2

dr1     jsr     CRLF       ;
dr2     pulx               ;
        rts
*******************************************************************
* display 2000 FF FF or similar [mark]
* takes advantage of variable order:
* ADDR1, WRT1, RD1 or ADDR2, WRT2, RD2
*******************************************************************
DISPADDR:
        ldd     $00,X      ;first address of pair
        bsr     ASCII      ;display high byte
        tba
        bsr     ASCII      ;display low byte
        bsr     SP
        ldaa    $02,X      ;datum value written
        bsr     ASCII
        bsr     SP
        ldaa    $03,X      ;datum value read
        bsr     ASCII
        rts

*******************************************************************
* ASCII: display contents of acca to terminal
*******************************************************************

ASCII   psha
outlhlf lsra            ;shift a to right
        lsra
        lsra
        lsra
        bsr     CVTASCII
        pula
        bsr     CVTASCII
        rts

CVTASCII
        anda    #$0f    ;mask top half
        adda    #$30    ;convert to ascii
        cmpa    #$39
        ble     outb    ;jump if 0-9
        adda    #$07    ;convert to a-f
outb    jsr     TRS     ;write terminal
        rts
*******************************************************************
SP      ldaa    #$20    ;write space to terminal
        jsr     TRS
        rts
*******************************************************************
CNTMSG  fcc      'INC AD $.'

MSG1    fcc      'AD WR RD:.'

MSG2    fcc      'OK!.'

MSG3    fcc      'Wait 35s.'

MSG4    fcc      'BAD WIRING OR CHIP.'

DELAY   pshx
        ldx     #$535      ;4 ms at E=2Mhz
dlylp   dex
        bne     dlylp
        pulx
        rts
*******************************************************************
	END

