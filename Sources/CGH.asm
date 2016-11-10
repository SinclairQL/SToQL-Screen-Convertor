* 68000 Machine Code Routines for SToQL Screen Convertor
* Original Routines (c) Alan Pemberton & Rich Mellor
* Last Updated 11/12/98
* UPGRADE VERSION 1.22
* QLIB : $$asmb=win1_RWAP_UTILS_CGH_EXT,14,118
*
       MOVE.W    $110,A0
       LEA.L     PROC,A1
       JSR       (A0)
       TST.L     D0
       BNE.S     WHAT
       MOVEQ     #0,D0
       TRAP      #1
       lea       SCR_LLEN,a1
       move.w    #128,(a1)            Assume screen length is 128 bytes
       lea       JM_ROM,a1
       move.w    #1,(a1)
       andi.l    #$FF00FFFF,D2        Ignore the country letter code
       cmpi.l    #$31003034,d2        JM ROM or earlier??
       bcs.s     START_PROG           Yes - signify code
       MOVE.W    #0,(A1)
       MOVE.L    #0,a0
       lea       EXTE,a2              Find out screen length
       moveq     #-1,d3
       moveq     #9,d0
       trap      #3                   ; IOW.XTOP
       lea       SCR_LLEN,a2
       move.w    d1,(a2)
       lea       SCR_BASE,a2
       move.l    a1,(a2)
START_PROG
       LEA.L     CK_MINERVA,A2
       MOVEQ     #0,D3
       LSR.L     #8,D2
       CMP.B     #'8',D2
       BLT.S     NO_MIN
       LSR.L     #8,D2
       LSR.L     #8,D2
       CMP.B   #'1',D2
       BNE.S   NO_MIN
       MOVEQ   #1,D3
NO_MIN
       MOVE.B  D3,(A2)
       move.l  $30(a6),d5
       cmp.l   $34(a6),d5
       bge.s   what
       move.l  0(a6,d5.l),a0
       lea.l   id_mess,a1
       move.w  $D0,a2
       jsr     (a2)
       MOVEQ   #0,D0
WHAT   RTS

ID_MESS
       dc.w    36
       dc.b    'CGH Toolkit v1.22',10
       dc.b    '1992 Rich Mellor',10

SCR_BASE
       dc.l    131072
SCR_LLEN
       dc.w    128
JM_ROM DC.W    0

NEW_WIN
       DC.W    3
       DC.B    'SCR',0
PROC
       DC.W    22           19 Procedures - but space for long names
       DC.W    HIRES-*
       DC.B    7,'CONV_HI'
       DC.W    MEDRES-*
       DC.B    8,'CONV_MED',0
       DC.W    LORES-*
       DC.B    8,'CONV_LOW',0
       DC.W    INVERT-*
       DC.B    6,'INVERT',0
       DC.W    S_SAVE-*
       DC.B    6,'SCR_SA',0
       DC.W    S_LOAD-*
       DC.B    6,'SCR_LO',0
       DC.W    COPY_SCR-*
       DC.B    6,'SCR_CO',0
       DC.W    COMPL50-*
       DC.B    7,'COMPL50'
       DC.W    COMPL66-*
       DC.B    7,'COMPL66'
       DC.W    COMPR50-*
       DC.B    7,'COMPR50'
       DC.W    COMPR66-*
       DC.B    7,'COMPR66'
       DC.W    COMPUP-*
       DC.B    7,'COMP_UP'
       DC.W    COMPDN-*
       DC.B    9,'COMP_DOWN'
       DC.W    EXPNDR100-*
       DC.B    9,'EXPNDR100'
       DC.W    EXPNDR33-*
       DC.B    8,'EXPNDR33',0
       DC.W    EXPNDL100-*
       DC.B    9,'EXPNDL100'
       DC.W    EXPNDL33-*
       DC.B    8,'EXPNDL33',0
       DC.W    FLIP-*
       DC.B    8,'SCR_FLIP',0
       DC.W    MODE8-*
       DC.B    7,'UNFLASH'
       dc.w    blanker-*
       dc.b    10,'BLANK_FILL',0
       DC.W    0
       DC.W    1
       DC.W    CK_DEV-*
       DC.B    6,'DEV_OK',0
       DC.W    0

EXTE
       MOVE.L  $32(A0),A1          ; Find out start of screen
       MOVEQ   #0,D1
       MOVE.W  $64(A0),D1          ; Find out screen length
       RTS

GET_CH    MOVEQ   #1,D6
          CMPA.L  A3,A5
          BEQ.S   GET_CH1
          BTST    #7,1(A6,A3.L)
          BEQ.S   GET_CH1
          MOVE.L  A5,-(A7)
          MOVE.L  A3,A5
          ADDQ.L  #8,A5
          MOVE.L  A5,-(A7)
          MOVEA.W $112,A2
          JSR     (A2)
          MOVEM.L (A7)+,A3/A5
          BNE.S   SEXIT
          MOVE.W  0(A6,A1.L),D6
          ADDQ.L  #2,$58(A6)

GET_CH1   MULU    #$28,D6
          MOVEQ   #-6,D0
          ADD.L   $30(A6),D6
          CMP.L   $34(A6),D6
          BCC.S   SEXIT
          MOVE.L  0(A6,D6.L),D1
          BTST    #$1F,D1
          BNE.S   SEXIT
          MOVE.L  D1,A0
          MOVEQ   #0,D0
          RTS

CK_MINERVA
       DS.W    1
GET_INT
       MOVE.L  D1,-(A7)
       MOVE.L  $58(A6),A4
       MOVE.W  $118,A2
       JSR     (A2)
       MOVE.L  (A7)+,D1
       TST.W   D0
       BNE.S   SEXIT
       MOVEQ   #-15,D0
       CMP.W   D1,D3
       BNE.S   SEXIT
       RTS
SEXIT
       ADDQ.L  #4,A7
       RTS
HIRES
       BSR.S   GET_CH
       MOVEQ   #1,D1
       BSR.S   GET_INT
       MOVE.L  0(A6,A1.L),A1
       MOVE.L  A4,$58(A6)
       LEA.L   HIRES_CONV,A2
EXTOP  MOVEQ   #9,D0
       MOVEQ   #-1,D3
       TRAP    #3
       RTS

HIRES_CONV
       MOVE.L  $32(A0),A3
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       MOVE.L  #255,D1

LOP0
       MOVE.L  #208,D3                  ;Number of bytes in ST screen width
       MOVE.L  A3,A4
       MOVE.L  A1,A2
       MOVE.L  #128,D5                  ;Work out width of QL screen in 2xlongwords
       SUB.L   D5,D3
       ROR.L   #3,D5
       SUBQ.L  #1,D5                    ;Work out max pixels on QL screen line
LOP1
       MOVE.L  (A1)+,D4                 ;Draw a line of the picture
       MOVEP.L D4,0(A3)
       ADDQ.L  #8,A3
       DBF     D5,LOP1

       MOVE.L  A2,A1                                ;Move to next line
       ADDA.L  D3,A1
       MOVE.L  A4,A3
       ADDA.L  D2,A3
       DBF     D1,LOP0
       MOVEQ   #0,D0
       RTS

MEDRES
       BSR     GET_CH
       MOVEQ   #2,D1
       BSR     GET_INT
       MOVE.L  4(A6,A1.L),D1
       MOVE.L  0(A6,A1.L),A1
       MOVE.L  A4,$58(A6)
       LEA.L   MEDRES_CONV,A2
       BRA     EXTOP
MEDRES_CONV
       MOVE.L  $32(A0),A3
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       MOVE.L  D1,A4
       ADDA.L  A3,A4
       MOVE.L  A4,A2
       MOVE.L  D2,D6
       MULU    #5,D6
       SUBA.L  D6,A4
       CMPA.L  A3,A4
       BGE.S   SCROK2
       MOVE.L  A3,A4

SCROK2
       MOVEQ   #0,D6
       MOVEQ   #31,D1
       MOVE.L  A4,A5
EMPT_SCR2
       CMPA.L  A4,A2
       BEQ.S   PRT_SCR2
       MOVE.L  D6,(A4)+
       DBF     D1,EMPT_SCR2
       MOVE.L  A5,A4
       ADDA.L  D2,A4
       BRA.S   SCROK2

PRT_SCR2
       MOVE.L  #199,D1
       MOVE.L  #160,D3
LOP3
       MOVE.L  A1,A2                    ;Store start of ST Screen Line
       MOVE.L  A4,A5                    ;Store start of QL Screen Line
       MOVE.L  #256,D5
       ROR.L   #2,D5
       SUBQ.L  #1,D5                    ;Calculate how many longwords can be copied
LOP4
       MOVE.B  2(A1),(A4)+
       MOVE.B  (A1),(A4)+
       MOVE.B  3(A1),(A4)+
       MOVE.B  1(A1),(A4)+
       ADDQ.L  #4,A1
       DBF     D5,LOP4

       MOVE.L  A2,A1                    ;Move to Next QL Screen Line
       ADDA.L  D3,A1
       MOVE.L  A5,A4
       ADDA.L  D2,A4
       DBF     D1,LOP3

       MOVE.L  A3,A2
       MOVE.L  D2,D0
       MULU    #128,D0
       SUBQ.L  #4,D0
       ADDA.L  D0,A2                    ;Work out end address of screen
       MOVE.L  A4,A1
       MOVE.L  D2,D0
       MULU    #5,D0
       ADDA.L  D0,A1
       MOVEQ   #0,D0
       CMPA.L  A2,A1
       BLE.S   CLR_BOT2
       MOVE.L  A2,A1

CLR_BOT2
       MOVEQ   #31,D1
       MOVE.L  A4,A5
CLR_SCR2
       CMPA.L  A1,A4
       BGT.S   FEXIT
       MOVE.L  D0,(A4)+
       DBF     D1,CLR_SCR2
       MOVE.L  A5,A4
       ADDA.L  D2,A4
       BRA.S   CLR_BOT2
FEXIT  RTS

LORES  bsr     get_ch
       MOVEQ   #2,D1
       BSR     GET_INT
       MOVE.L  4(A6,A1.L),D1
       move.l  0(a6,a1.l),a1
       move.l  a4,$58(a6)
       lea.l   lores_conv,a2
       BRA     EXTOP
lores_conv
       move.l  $32(a0),a3
       move.l  d1,a5
       ADDA.L  a3,a5
       MOVE.L  A5,A2
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       MOVE.L  D2,D6
       MULU    #5,D6
       SUBA.L  D6,a5
       MOVEQ   #0,D6
       CMPA.L  A3,a5
       BGE.S   SCROK
       MOVE.L  A3,a5
SCROK
       MOVEQ   #31,D1
       MOVE.L  A5,A4
EMPT_SCR
       CMPA.L  a5,A2
       BEQ.S   PRT_SCR
       MOVE.L  D6,(a5)+
       DBF     D1,EMPT_SCR
       MOVE.L  A4,A5
       ADDA.L  D2,A5
       BRA.S   SCROK

PRT_SCR
       MOVEQ   #1,D6
       MOVE.L  #199,D5
LOP8
       MOVE.L  A1,A4
       MOVEQ   #15,D7
       MOVE.L  A5,A2
LOP6
       MOVE.L  (A1)+,D1
       MOVE.W  (A1)+,D2
       MOVE.W  (A1)+,D3
       MOVEQ   #3,D0
LOP7
       clr.w   d4
       LSL.W   D6,D3
       ROXL.W  #2,D4
       LSL.W   D6,D3
       ROXL.W  #2,D4
       LSL.W   D6,D3
       ROXL.W  #2,D4
       LSL.W   D6,D3
       ROXL.W  #2,D4
       LSL.W   D6,D1
       ROXL.W  D6,D4
       LSL.W   D6,D2
       ROXL.W  D6,D4
       LSL.W   D6,D1
       ROXL.W  D6,D4
       LSL.W   D6,D2
       ROXL.W  D6,D4
       LSL.W   D6,D1
       ROXL.W  D6,D4
       LSL.W   D6,D2
       ROXL.W  D6,D4
       LSL.W   D6,D1
       ROXL.W  D6,D4
       LSL.W   D6,D2
       ROXL.W  D6,D4
       MOVE.W  D4,(a5)+
       DBF     D0,LOP7
       DBF     D7,LOP6
       MOVE.L  A4,A1
       ADDA.L  #160,A1
       MOVE.L  A2,A5
       MOVEQ   #0,D7
       MOVE.W  SCR_LLEN,D7
       ADDA.L  D7,A5
       DBF     D5,LOP8

       MOVE.L  A3,A2
       MOVE.L  D7,D2
       MULU    #256,D7
       SUBQ.L  #4,D7
       ADDA.L  D7,A2
       MOVE.L  a5,A1
       MOVE.L  D2,D0
       MULU    #5,D0
       ADDA.L  D0,A1
       MOVEQ   #0,D0
       CMPA.L  A2,A1
       BLE.S   END_LO
       MOVE.L  A2,A1
END_LO
       MOVE.L  A5,A4
       MOVEQ   #31,D1
CLR_BOT
       CMPA.L  A1,a5
       BGT.S   EEXIT
       MOVE.L  D0,(a5)+
       DBF     D1,CLR_BOT
       MOVE.L  A4,A5
       ADDA.L  D2,A5
       BRA.S   END_LO
EEXIT  RTS

INVERT
       BSR     GET_CH
       LEA.L   DO_INVERT,A2
       BRA     EXTOP
DO_INVERT
       MOVE.L  $32(A0),A5
       MOVE.L  A5,A1
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       MOVE.L  D2,D6
       MULU    #255,D2
       ADDA.L  D2,A5
       MOVE.L  #128,D0                  ;Number of lines on screen/2
       MOVEQ   #16,D7
LOPA
       MOVEQ   #7,D1
       MOVE.L  A1,A3
       MOVE.L  A5,A4
LOPB
       MOVEM.L (A5),D2-D5
       MOVE.L  (A1),(A5)+
       MOVE.L  4(A1),(A5)+
       MOVE.L  8(A1),(A5)+
       MOVE.L  12(A1),(A5)+
       MOVEM.L D2-D5,(A1)
       ADDA.L  D7,A1
       DBF     D1,LOPB
       MOVE.L  A3,A1
       ADDA.L  D6,A1
       MOVE.L  A4,A5
       SUBA.L  D6,A5
       DBF     D0,LOPA
       MOVEQ   #0,D0
       RTS

CALC_ADD
       MOVE.L  0(A6,A1.L),D1
       LSR.L   #2,D1
       MOVEQ   #0,D3
       MOVE.W  SCR_LLEN,D3
       SUBQ.W  #2,D3
       AND.L   D3,D1                        ;(0-126)
       MOVE.L  4(A6,A1.L),D2
       MOVE.W  SCR_LLEN,D3
       MULU    D3,D2                        ;Number of bytes down
       ADDA.L  D2,A0
       ADDA.L  D1,A0
       RTS

S_SAVE
       MOVEQ   #6,D1
       BSR     GET_INT
       MOVE.L  20(A6,A1.L),A0
       BSR.S   CALC_ADD
       MOVE.L  8(A6,A1.L),D1
       MOVE.L  16(A6,A1.L),A2
       SUBQ.L  #1,D1
       LSR.L   #3,D1
       MOVE.L  D1,(A2)+
       MOVE.L  12(A6,A1.L),D0
       MOVE.L  A4,$58(A6)
       SUBQ.L  #1,D0
       MOVE.L  D0,(A2)+
       MOVE.L  D1,D3
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2

       TRAP    #0
       ORI.W   #$0700,SR
LOPE
       MOVE.L  A0,A3
LOPF
       MOVE.W  (A0)+,(A2)+
       DBF     D1,LOPF
       MOVE.L  A3,A0
       ADDA.L  D2,A0
       MOVE.L  D3,D1
       DBF     D0,LOPE
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

S_LOAD
       MOVEQ   #4,D1
       BSR     GET_INT
       MOVE.L  12(A6,A1.L),A0
       BSR     CALC_ADD
       MOVE.L  8(A6,A1.L),A1
       MOVE.L  A4,$58(A6)
       MOVE.L  (A1)+,D1
       MOVE.L  (A1)+,D0
       MOVE.L  D1,D3
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       TRAP    #0
       ORI.W   #$0700,SR
LOPG
       MOVE.L  A0,A2
LOPH
       MOVE.W  (A1)+,(A0)+
       DBF     D1,LOPH
       MOVE.L  A2,A0
       ADDA.L  D2,A0
       MOVE.L  D3,D1
       DBF     D0,LOPG
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

COPY_SCR
       MOVEQ   #2,D1
       BSR     GET_INT
       MOVE.L  4(A6,A1.L),A0
       MOVE.L  0(A6,A1.L),A1
       MOVE.L  A4,$58(A6)
       MOVE.W  SCR_LLEN,D3
       CMP.W   #128,D3
       BNE.S   BIG_SCREEN
       MOVE.B  CK_MINERVA,D3
       TST.B   D3
       BNE.S   MIN_COPY
BIG_SCREEN
       MOVEQ   #0,D3
       MOVE.W  SCR_LLEN,D3
       MOVEQ   #31,D4              Number of long words to copy-1
       MOVE.L  #255,D1             Number of lines to copy-1
       MOVE.L  SCR_BASE,A3         Are we copying from the screen?
       CMPA.L  A0,A3
       BNE.S   FROM_SCREEN
TO_SCREEN
       TRAP    #0
       ORI.W   #$0700,SR
NX_COPY
       MOVE.L  A0,A3
       MOVE.L  D4,D2
DO_COPY
       MOVE.L  (A1)+,(A0)+         Copy a line
       DBF     D2,DO_COPY
       MOVE.L  A3,A0
       ADDA.L  D3,A0               Move down a line
       DBF     D1,NX_COPY          Copy next line
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

FROM_SCREEN
       CMPA.L  A1,A3
       BNE.S   TO_BUFFER           Copying between buffers!
       TRAP    #0
       ORI.W   #$0700,SR
NX2_COPY
       MOVE.L  A1,A3
       MOVE.L  D4,D2
DO2_COPY
       MOVE.L  (A1)+,(A0)+         Copy a line
       DBF     D2,DO2_COPY
       MOVE.L  A3,A1
       ADDA.L  D3,A1               Move down a line
       DBF     D1,NX2_COPY         Copy next line
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

TO_BUFFER
       TRAP    #0
       ORI.W   #$0700,SR

       MOVE.L  #32768,D1           Copy Whole Buffer (max size=32768 at moment)
       ROR.L   #2,D1
       SUBQ.L  #1,D1
DO3_COPY
       MOVE.L  (A1)+,(A0)+         Copy a line
       DBF     D1,DO3_COPY
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

* If Minerva, and screen is 128 bytes long at present
MIN_COPY
       MOVE.L  #32768,D1
       MOVEQ   #0,D0
       MOVE.W  $158,A2
       TRAP    #0
       ORI.W   #$0700,SR
       JSR     $4000(A2)
       ANDI.W  #$D8FF,SR
       MOVEQ   #0,D0
       RTS

COMPL50
       BSR     GET_CH
       LEA.L   COMP_L50,A2
       BRA     EXTOP
COMP_L50
       MOVE.L  $32(A0),A1
       MOVE.L  A1,A2
       MOVE.L  #255,D3
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
COM1A
       MOVE.L  A1,A3
       MOVE.L  A2,A4
       MOVEQ   #31,D4
COM1
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       MOVE.W  D2,D0
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000110000001100,D2
       LSL.W   #2,D2
       OR.W    D2,D0                         ;REM D0=1111000011110000
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       LSR.W   #4,D2
       OR.W    D2,D0
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000110000001100,D2
       LSR.W   #2,D2
       OR.W    D2,D0                         ;REM D0=1111111111111111
       MOVE.W  D0,(A2)+
       DBF     D4,COM1
       MOVEQ   #0,D2
       MOVEQ   #15,D4
END_LIN
       MOVE.L  D2,(A2)+
       DBF     D4,END_LIN
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       ADDA.L  D5,A1
       ADDA.L  D5,A2
       DBF     D3,COM1A
       MOVEQ   #0,D0
       RTS

COMPL66
       BSR     GET_CH
       LEA.L   COMP_L66,A2
       BRA     EXTOP

COMP_L66
       MOVE.L  $32(A0),A1
       MOVE.L  A1,A2
       MOVE.L  #255,D3
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
COM2A
       MOVE.L  A1,A3
       MOVE.L  A2,A4
       MOVEQ   #20,D4
COM2
       MOVE.W  (A1),D2
       ANDI.W  #%1111000011110000,D2
       MOVE.W  D2,D0
       MOVE.W  (A1)+,D2                      ;pixel 0-7
       ANDI.W  #%0000001100000011,D2
       LSL.W   #2,D2
       OR.W    D2,D0                         ;REM D0=1111110011111100
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       LSR.W   #6,D2
       OR.W    D2,D0
       MOVE.W  D0,(A2)+
       MOVE.W  (A1)+,D2                      ;pixel 8-15
       ANDI.W  #%0000111100001111,D2
       LSL.W   #4,D2
       MOVE.W  D2,D0                         ;REM D0=1111000011110000
       MOVE.W  (A1)+,D2                      ;pixel 16-23
       ANDI.W  #%0011110000111100,D2
       LSR.W   #2,D2
       OR.W    D2,D0
       MOVE.W  D0,(A2)+
       DBF     D4,COM2                       ; to pixel 503
       MOVE.W  (A1)+,D2                      ; pixel 511
       ANDI.W  #%1111000011110000,D2
       MOVE.W  D2,(A2)+
       MOVEQ   #0,D2
       MOVEQ   #20,D4
END_LIN2
       MOVE.W  D2,(A2)+
       DBF     D4,END_LIN2
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       ADDA.L  D5,A1
       ADDA.L  D5,A2
       DBF     D3,COM2A
       MOVEQ   #0,D0
       RTS

COMPR50
       BSR     GET_CH
       LEA.L   COMP_R50,A2
       BRA     EXTOP
COMP_R50
       MOVE.L  $32(A0),A1
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  D5,D3
       ROL.L   #8,D3
       SUB.L   D5,D3
       ADD.L   #128,D3
       ADDA.L  D3,A1
       MOVE.L  A1,A2
       MOVE.L  #255,D3
COM3A
       MOVE.L  A1,A3
       MOVE.L  A2,A4
       MOVEQ   #31,D4
COMA
       MOVE.W  -(A1),D2
       ANDI.W  #%0000001100000011,D2
       MOVE.W  D2,D0
       MOVE.W  (A1),D2
       ANDI.W  #%0011000000110000,D2
       LSR.W   #2,D2
       OR.W    D2,D0
       MOVE.W  -(A1),D2
       ANDI.W  #%0000001100000011,D2
       LSL.W   #4,D2
       OR.W    D2,D0
       MOVE.W  (A1),D2
       ANDI.W  #%0011000000110000,D2
       LSL.W   #2,D2
       OR.W    D2,D0
       MOVE.W  D0,-(A2)
       DBF     D4,COMA
       MOVEQ   #0,D2
       MOVEQ   #15,D4
ENDA_LIN
       MOVE.L  D2,-(A2)
       DBF     D4,ENDA_LIN
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       SUBA.L  D5,A1
       SUBA.L  D5,A2
       DBF     D3,COM3A
       MOVEQ   #0,D0
       RTS

COMPR66
       BSR     GET_CH
       LEA.L   COMP_R66,A2
       BRA     EXTOP
COMP_R66
       MOVE.L  $32(A0),A1
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  D5,D3
       ROL.L   #8,D3
       SUB.L   D5,D3
       ADD.L   #128,D3
       ADDA.L  D3,A1
       MOVE.L  A1,A2
       MOVE.L  #255,D3
COM4A
       MOVE.L  A1,A3
       MOVE.L  A2,A4
       MOVEQ   #20,D4
COMB
       MOVE.W  -(A1),D2                     ;pixels 0-3
       ANDI.W  #%0000111100001111,D2
       MOVE.W  D2,D0
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       LSR.W   #2,D2
       OR.W    D2,D0
       MOVE.W  -(A1),D2                     ;pixels 4-7
       ANDI.W  #%0000001100000011,D2
       LSL.W   #6,D2
       OR.W    D2,D0
       MOVE.W  D0,-(A2)
       MOVE.W  (A1),D2
       ANDI.W  #%1111000011110000,D2
       LSR.W   #4,D2
       MOVE.W  D2,D0
       MOVE.W  -(A1),D2                      ;pixels 8-11
       ANDI.W  #%0011110000111100,D2
       LSL.W   #2,D2
       OR.W    D2,D0
       MOVE.W  D0,-(A2)
       DBF     D4,COMB
       MOVE.W  -(A1),D2
       ANDI.W  #%0000111100001111,D2
       MOVE.W  D2,-(A2)
       MOVEQ   #0,D2
       MOVEQ   #20,D4
ENDB_LIN
       MOVE.W  D2,-(A2)
       DBF     D4,ENDB_LIN
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       SUBA.L  D5,A1
       SUBA.L  D5,A2
       DBF     D3,COM4A
       MOVEQ   #0,D0
       RTS


COMPUP
       BSR     GET_CH
       LEA.L   COMP_UP,A2
       BRA     EXTOP
COMP_UP
       MOVE.L  $32(A0),A1
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  A1,A2
       MOVEQ   #126,D4
M_UP1
       MOVE.L  A1,A3
       ADDA.L  D5,A2
       MOVE.L  A2,A4
       MOVEQ   #31,D3
M_UP2
       MOVE.L  (A2)+,(A1)+
       DBF     D3,M_UP2
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       ADDA.L  D5,A1
       ADDA.L  D5,A2
       DBF     D4,M_UP1
       MOVE.L  #127,D4
       MOVEQ   #0,D0
M_UP3A
       MOVE.L  A1,A3
       MOVEQ   #31,D3
M_UP3
       MOVE.L  D0,(A1)+
       DBF     D3,M_UP3
       MOVE.L  A3,A1
       ADDA.L  D5,A1
       DBF     D4,M_UP3A
       RTS

COMPDN
       BSR     GET_CH
       LEA.L   COMP_DN,A2
       BRA     EXTOP
COMP_DN
       MOVE.L  $32(A0),A1
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  D5,D1
       MULU    #256,D1
       SUB.L   D5,D1
       ADD.L   #128,D1
       ADDA.L  D1,A1
       MOVE.L  A1,A2
       MOVEQ   #126,D4
M_DN1
       MOVE.L  A1,A3
       SUBA.L  D5,A2
       MOVE.L  A2,A4
       MOVEQ   #31,D3
M_DN2
       MOVE.L  -(A2),-(A1)
       DBF     D3,M_DN2
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       SUBA.L  D5,A1
       SUBA.L  D5,A2
       DBF     D4,M_DN1
       MOVE.L  #127,D4
       MOVEQ   #0,D0
M_DN3A
       MOVE.L  A1,A3
       MOVEQ   #31,D3
M_DN3
       MOVE.L  D0,-(A1)
       DBF     D3,M_DN3
       MOVE.L  A3,A1
       SUBA.L  D5,A1
       DBF     D4,M_DN3A
       RTS

EXPNDR100
       BSR     GET_CH
       LEA.L   EXPND_R100,A2
       BRA     EXTOP
EXPND_R100
       MOVE.L  $32(A0),A2
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  D5,D3
       MULU    #256,D3
       SUB.L   D5,D3
       ADD.L   #128,D3
       ADDA.L  D3,A2
       MOVE.L  #255,D3
EXP1
       MOVE.L  A2,A3
       MOVE.L  A2,A1
       SUBA.L  #64,A1
       MOVEQ   #31,D4
EXP2
       MOVE.W  -(A1),D2
       ANDI.W  #%0000001100000011,D2
       MOVE.W  D2,D0                        D0=------XX------XX
       LSL.W   #2,D2
       OR.W    D2,D0                        D0=----XXXX----XXXX
       MOVE.W  (A1),D2
       ANDI.W  #%0000110000001100,D2
       LSL.W   #2,D2
       OR.W    D2,D0                        D0=--XXXXXX--XXXXXX
       LSL.W   #2,D2
       OR.W    D2,D0                        D0=XXXXXXXXXXXXXXXX
       MOVE.W  D0,-(A2)
       MOVE.W  (A1),D2
       ANDI.W  #%0011000000110000,D2
       LSR.W   #2,D2
       MOVE.W  D2,D0                        D0=----XX------XX--
       LSR.W   #2,D2
       OR.W    D2,D0                        D0=----XXXX----XXXX
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       OR.W    D2,D0                        D0=XX--XXXXXX--XXXX
       LSR.W   #2,D2
       OR.W    D2,D0                        D0=XXXXXXXXXXXXXXXX
       MOVE.W  D0,-(A2)
       DBF     D4,EXP2
       MOVE.L  A3,A2
       SUBA.L  D5,A2
       DBF     D3,EXP1
       MOVEQ   #0,D0
       RTS

EXPNDR33
       BSR     GET_CH
       LEA.L   EXPND_R33,A2
       BRA     EXTOP
EXPND_R33
       MOVE.L  $32(A0),A2
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  D5,D3
       MULU    #256,D3
       SUB.L   D5,D3
       ADD.L   #128,D3
       ADDA.L  D3,A2
       MOVE.L  #255,D3
EXP5
       MOVE.L  A2,A3
       MOVE.L  A2,A1
       SUBA.L  #42,A1
       MOVEQ   #20,D4
       MOVE.W  -(A1),D2
       ANDI.W  #%0000110000001100,D2
       MOVE.W  D2,D0
       LSR.W   #2,D2
       OR.W    D2,D0
       MOVE.W  (A1),D2
       ANDI.W  #%1111000011110000,D2
       OR.W    D2,D0
       MOVE.W  D0,-(A2)
EXP6
       MOVE.W  -(A1),D2
       ANDI.W  #%0000001100000011,D2
       MOVE.W  D2,D0                       D0=---X
       MOVE.W  (A1),D2
       ANDI.W  #%0011111100111111,D2
       LSL.W   #2,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,-(A2)
       MOVE.W  (A1),D2
       ANDI.W  #%1111000011110000,D2
       LSR.W   #4,D2
       MOVE.W  D2,D0                       D0=--XX
       MOVE.W  -(A1),D2
       ANDI.W  #%0000001100000011,D2
       LSL.W   #4,D2
       OR.W    D2,D0                       D0=-XXX
       LSL.W   #2,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,-(A2)
       MOVE.W  (A1),D2
       ANDI.W  #%0011110000111100,D2
       LSR.W   #2,D2
       MOVE.W  D2,D0                       D0=--XX
       MOVE.W  (A1),D2
       ANDI.W  #%1111000011110000,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,-(A2)
       DBF     D4,EXP6
       MOVE.L  A3,A2
       SUBA.L  D5,A2
       DBF     D3,EXP5
       MOVEQ   #0,D0
       RTS

EXPNDL100
       BSR     GET_CH
       LEA.L   EXPND_L100,A2
       BRA     EXTOP
EXPND_L100
       MOVE.L  $32(A0),A2
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  #255,D3
EXPA
       MOVE.L  A2,A3
       MOVE.L  A2,A1
       ADDA.L  #64,A1
       MOVEQ   #31,D4
EXPB
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       MOVE.W  D2,D0                        D0=XX------XX------
       LSR.W   #2,D2
       OR.W    D2,D0                        D0=XXXX----XXXX----
       MOVE.W  (A1),D2
       ANDI.W  #%0011000000110000,D2
       LSR.W   #2,D2
       OR.W    D2,D0                        D0=XXXXXX--XXXXXX--
       LSR.W   #2,D2
       OR.W    D2,D0                        D0=XXXXXXXXXXXXXXXX
       MOVE.W  D0,(A2)+
       MOVE.W  (A1),D2
       ANDI.W  #%0000110000001100,D2
       LSL.W   #2,D2
       MOVE.W  D2,D0                        D0=--XX------XX----
       LSL.W   #2,D2
       OR.W    D2,D0                        D0=XXXX----XXXX----
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000001100000011,D2
       OR.W    D2,D0                        D0=XXXX--XXXXXX--XX
       LSL.W   #2,D2
       OR.W    D2,D0                        D0=XXXXXXXXXXXXXXXX
       MOVE.W  D0,(A2)+
       DBF     D4,EXPB
       MOVE.L  A3,A2
       ADDA.L  D5,A2
       DBF     D3,EXPA
       MOVEQ   #0,D0
       RTS

EXPNDL33
       BSR     GET_CH
       LEA.L   EXPND_L33,A2
       BRA     EXTOP
EXPND_L33
       MOVE.L  $32(A0),A2
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  #255,D3
EXPD
       MOVE.L  A2,A3
       MOVE.L  A2,A1
       ADDA.L  #42,A1
       MOVEQ   #20,D4
       MOVE.W  (A1),D2
       ANDI.W  #%0011000000110000,D2
       MOVE.W  D2,D0
       LSL.W   #2,D2
       OR.W    D2,D0
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000111100001111,D2
       OR.W    D2,D0
       MOVE.W  D0,(A2)+
EXPE
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       MOVE.W  D2,D0                       D0=---X
       MOVE.W  (A1),D2
       ANDI.W  #%1111110011111100,D2
       LSR.W   #2,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,(A2)+
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000111100001111,D2
       LSL.W   #4,D2
       MOVE.W  D2,D0                       D0=--XX
       MOVE.W  (A1),D2
       ANDI.W  #%1100000011000000,D2
       LSR.W   #4,D2
       OR.W    D2,D0                       D0=-XXX
       LSR.W   #2,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,(A2)+
       MOVE.W  (A1),D2
       ANDI.W  #%0011110000111100,D2
       LSL.W   #2,D2
       MOVE.W  D2,D0                       D0=--XX
       MOVE.W  (A1)+,D2
       ANDI.W  #%0000111100001111,D2
       OR.W    D2,D0                       D0=XXXX
       MOVE.W  D0,(A2)+
       DBF     D4,EXPE
       MOVE.L  A3,A2
       ADDA.L  D5,A2
       DBF     D3,EXPD
       MOVEQ   #0,D0
       RTS

FLIP
       BSR     GET_CH
       LEA.L   DO_FLIP,A2
       BRA     EXTOP
DO_FLIP
       MOVE.L  $32(A0),A1
       MOVE.L  A1,A2
       MOVE.W  #%1100000011000000,D3
       MOVE.W  #%0000001100000011,D4
       MOVE.L  #255,D5
       ADDA.L  #128,A2
FLOP1
       MOVE.L  D5,-(A7)
       MOVE.L  A1,A3
       MOVE.L  A2,A4
       MOVEQ   #31,D5
FLIPPER
       MOVE.W  (A1),D2
       MOVE.W  -(A2),D1
       AND.W   D3,D2
       LSR.W   #6,D2
       MOVE.W  D2,D7
       AND.W   D4,D1
       LSL.W   #6,D1
       MOVE.W  D1,D0
       MOVE.W  (A1),D2
       MOVE.W  (A2),D1
       LSR.W   #2,D3
       AND.W   D3,D2
       LSR.W   #2,D2
       OR.W    D2,D7
       LSL.W   #2,D4
       AND.W   D4,D1
       LSL.W   #2,D1
       OR.W    D1,D0
       MOVE.W  (A1),D2
       MOVE.W  (A2),D1
       AND.W   D4,D2
       LSL.W   #2,D2
       OR.W    D2,D7
       AND.W   D3,D1
       LSR.W   #2,D1
       OR.W    D1,D0
       MOVE.W  (A1),D2
       MOVE.W  (A2),D1
       LSR.W   #2,D4
       AND.W   D4,D2
       LSL.W   #6,D2
       OR.W    D2,D7
       LSL.W   #2,D3
       AND.W   D3,D1
       LSR.W   #6,D1
       OR.W    D1,D0
       MOVE.W  D0,(A1)+
       MOVE.W  D7,(A2)
       DBF     D5,FLIPPER
       MOVE.L  (A7)+,D5
       MOVE.L  A3,A1
       MOVE.L  A4,A2
       MOVEQ   #0,D2
       MOVE.W  SCR_LLEN,D2
       ADDA.L  D2,A1
       ADDA.L  D2,A2
       DBF     D5,FLOP1
       MOVEQ   #0,D0
       RTS

MODE8
       BSR     GET_CH
       LEA.L   DO_MODE8,A2
       BRA     EXTOP
DO_MODE8
       MOVE.L  $32(A0),A1
       MOVEQ   #0,D5
       MOVE.W  SCR_LLEN,D5
       MOVE.L  #%10101010111111111010101011111111,D2
       MOVE.L  #255,D3
FL_1
       MOVE.L  A1,A3
       MOVE.L  #31,D1
FL_2
       AND.L   D2,(A1)+
       DBF     D1,FL_2
       MOVE.L  A3,A1
       ADDA.L  D5,A1
       DBF     D3,FL_1
       MOVEQ   #0,D0
       RTS

BP_RET MOVEQ  #-15,D0
OUCH   RTS

CK_DEV
       MOVE.L $58(A6),A4
       MOVE.W $116,A2
       JSR    (A2)
       TST.L  D0
       BNE.S  OUCH
       CMPI.W #1,D3             One string parameter MUST be given
       BNE.S  BP_RET
       MOVE.L A4,$58(A6)
       MOVE.W 0(A6,A1.L),D1     Get length of string parameter
       TST.W  D1
       BEQ    LEN0_STRG
       ADDQ.L #2,A1
       MOVE.L A1,A4
       SUBQ.W #1,D1
COPY_STRG
       MOVE.B 0(A6,A1.L),D2
       CMP.B  #'a',D2
       BLT.S  NO_ALPH
       CMP.B  #'z',D2
       BGT.S  NO_ALPH
       BCLR   #5,D2             Force string into upper case
NO_ALPH
       MOVE.B D2,0(A6,A1.L)
       ADDQ.L #1,A1
       DBF    D1,COPY_STRG
       MOVEQ  #0,D0
       TRAP   #1
       MOVE.L A4,A1
       MOVE.L A4,A3
       MOVE.L $48(A0),A0        Start of directory device list
CMP_DEV
       MOVE.L A0,A4
       MOVE.W $24(A0),D1        Get length of first device
       TST.W  D1
       BEQ.S  NEXT_DEV
       LEA.L  $26(A0),A0
       SUBQ.W #1,D1
CMP_CHR
       MOVE.B 0(A1,A6.L),D2
       CMP.B  (A0)+,D2
       ADDQ.L #1,A1
       BNE.S  NEXT_DEV
       DBF    D1,CMP_CHR
       BRA.S  FOUND_DEV
NEXT_DEV
       MOVE.L (A4),D1
       TST.L  D1
       BEQ.S  NOT_FOUND
       MOVE.L D1,A0
       MOVE.L A3,A1
       BRA.S  CMP_DEV
FOUND_DEV
       MOVEQ  #0,D7
       MOVE.W $24(A4),D7
       CMPI.W #1,D7
       BNE.S  NOT_NET
       MOVE.B $26(A4),D5
       CMP.B  #'N',D5
       BNE.S  NOT_NET
       MOVEQ  #1,D7
       MOVEQ  #1,D1
       BRA.S  YES_NET
NOT_NET
       MOVE.L D7,D1
       ADDQ.L #2,D7
YES_NET
       MOVE.W 0(A3,A6.L),D2
       SUB.W  D1,D2
       CMPI.W #2,D2
       BLT.S  NOT_FOUND
       MOVE.B 0(A1,A6.L),D1
       CMP.B  #'1',D1
       BLT.S  NOT_FOUND
       CMP.B  #'8',D1
       BGT.S  NOT_FOUND
       MOVE.B 1(A1,A6.L),D0
       CMP.B  #'_',D0
       BEQ.S  RESET_MEM
NOT_FOUND
       MOVEQ  #0,D7
RESET_MEM
       BRA.S  FLOAT_RET

FLOAT3
       MOVEQ   #0,D2
       MOVE.W  #$0820,D0
FLOAT8 SUBQ.W  #1,D0
       ASL.L   #1,D1
       BVS.S   FLOAT6
       BEQ.S   FLOAT7
       BRA.S   FLOAT8

FLOAT6 ROXR.L  #1,D1
       MOVE.W  D0,D2

FLOAT7 RTS

FLOAT_RET
       MOVE.L  D7,D1
       BSR.S   FLOAT3
       MOVE.L  D1,D3
       MOVEQ   #6,D1
       BSR.S   FLOAT4
       MOVEA.L $58(A6),A1
       MOVE.W  D2,0(A6,A1.L)
       MOVE.L  D3,2(A6,A1.L)
       MOVEQ   #0,D0
       MOVEQ   #2,D4
       RTS

FLOAT5 MOVEM.L D1-D3/A0/A2,-(A7)
       MOVEA.W $11A,A2
       JSR     (A2)
       MOVEM.L (A7)+,D1-D3/A0/A2
       RTS

FLOAT4 BSR.S   FLOAT5
       SUB.L   D1,$58(A6)
       RTS

LEN0_STRG
       MOVEQ  #-15,D7
       MOVE.L A4,$58(A6)
       BRA.S  FLOAT_RET

blanker
       moveq   #-15,d0         Bad parameter
       adda.w  #2*8,a3
       cmpa.l  a3,a5
       bne.s   rts0            Ensure exactly two parameters

* Get the default buffer

       suba.l  #8,a3           Retrieve just the last parameter
       move.l  a3,-(a7)
       move.l  $58(a6),a4
       move.w  $112,a2         get operation code
       jsr     (a2)
       bne.s   rts0
       move.w  0(a1,a6.l),d7
       move.l  (a7)+,a3
       suba.l  #8,a3
       move.w  $118,a2
       jsr     (a2)
       bne.s   rts0

* Read the first parameter: the buffer start address

       move.l  0(a1,a6.l),a0   Get buff_start
       move.l  a4,$58(a6)      Tidy stack
       tst.w   d7
       bne.s   move_buffer
       move.l  #23,d0
       lea.l   store,a1
       movem.l (a1),d1-d6/a2-a3
clear_loop
       movem.l d1-d6/a2-a3,(a0)
       adda.l  #32,a0
       movem.l d1-d6/a2-a3,(a0)
       adda.l  #32,a0
       dbf     d0,clear_loop
       moveq   #0,d0
rts0   rts

move_buffer
       adda.l  #512,a0
       move.l  a0,a1
       adda.l  #512,a1
       move.l  #127,d0
       moveq   #-1,d1
move_along
       move.l  (a1),(a0)+
       move.l  d1,(a1)+
       dbf     d0,move_along
       moveq   #0,d0
       rts

store   dc.l    -1,-1,-1,-1,-1,-1,-1,-1

FIN    END
