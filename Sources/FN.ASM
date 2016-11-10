* Miscellaneous Public Domain Functions
* Improved by Rich Mellor 26/5/92
* v1.05
* QLIB : $$asmb=WIN1_RWAP_UTILS_FN_EXT,0,12
*
START     MOVE.W  $110,A0
          LEA.L   DEFINE,A1
          JSR     (A0)
          RTS

DEFINE    DC.W    0,0,14
          DC.W    QDOS-*
          DC.B    5,'QDOS$'
          DC.W    RMODE-*
          DC.B    5,'RMODE'
          DC.W    ATARI-*
          DC.B    7,'QuATARI'
          DC.W    PINFO-*
          DC.B    5,'PINF$'
          DC.W    WINFO-*
          DC.B    5,'WINF$'
          DC.W    QFLIM-*
          DC.B    5,'QFLIM'
          DC.W    THING-*
          DC.B    5,'THING'
          DC.W    TH_VER-*
          DC.B    7,'TH_VER$'
          DC.W    KEYWORD-*
          DC.B    4,'KEYW',0
          DC.W    SCREEN-*
          DC.B    6,'SCREEN',0
          DC.W    SCRINC-*
          DC.B    6,'SCRINC',0
          DC.W    SYSBASE-*
          DC.B    8,'SYS_BASE',0
          DC.W    WINBASE-*
          DC.B    8,'WIN_BASE',0
          DC.W    DEFSCR-*
          DC.B    11,'DEFAULT_SCR'
          DC.W    0

*         BASIC - FUNCTION    'SCREEN'

SCREEN    MOVEQ   #0,D4
          BSR     GET_CH
          MOVE.L  D0,D1
          BNE.S   SCREXIT
          LEA     WN_B03,A2
          MOVEQ   #-1,D3
          MOVEQ   #9,D0
          TRAP    #3
          TST.L   D0
          BNE.S   SCR004
          BRA.S   SCR002

SCR004    MOVE.L  D0,D1
          BRA.S   SCREXIT     * FLOAT_RET D1

SCR002    MOVE.L  D1,A0
          MOVE.L  $32(A0),D1

SCREXIT   MOVE.L  D1,D5
          BRA     FLOAT_RET

MINERVA   MOVEQ   #0,D0
          TRAP    #1
          MOVEQ   #1,D1
          LSR.L   #8,D2
          CMP.B   #'6',D2
          BLT.S   NOT_MIN
          LSR.L   #8,D2
          LSR.L   #8,D2
          CMP.B   #'1',D2
          BNE.S   NOT_MIN
          MOVEQ   #0,D1
NOT_MIN   TST.L   D1                 D1=0 if it is Minerva!
          RTS

*         BASIC - FUNCTION    'SCRINC'

SCRINC    MOVEQ   #0,D4
          BSR     GET_CH
          MOVE.L  D0,D1
          BNE.S   SCRINCEX
          LEA     WN_B03,A2
          MOVEQ   #-1,D3
          MOVEQ   #9,D0
          TRAP    #3
          TST.L   D0
          BNE.S   SCRINC4
          BRA.S   SCRINC2

SCRINC4   MOVE.L  D0,D1
          BRA.S   SCRINCEX    * FLOAT_RET D1

SCRINC2   MOVE.L  D1,A0
          CLR.L   D1
          MOVE.W  $64(A0),D1

SCRINCEX  MOVE.L  D1,D5
          BRA     FLOAT_RET



*         BASIC - FUNCTION    'QDOS$'

QDOS      MOVEQ   #0,D0
          TRAP    #1
          MOVE.L  #6,D6
          MOVE.L  D2,D7
          BRA.S   STRING

*         BASIC - FUNCTION    'PINF$'

PINFO     BSR     GET_CH
          TST.L   D0
          BNE.S   ERR_BP
          BSR.S   IOP_PINF
          BNE.S   PTR_NF
          MOVE.L  D1,D7
          MOVEQ   #6,D6
          BRA.S   STRING

*         BASIC - FUNCTION    'WINF$'

WINFO     BSR     GET_CH
          TST.L   D0
          BNE.S   ERR_BP
          BSR.S   IOP_PINF
          BNE.S   PTR_NF
          CMPA.L  #0,A1
          BEQ.S   PTR_NF
          MOVE.L  0(A1),D7
          MOVEQ   #6,D6
          BRA.S   STRING

IOP_PINF  MOVEQ   #$70,D0
          TRAP    #3
          TST.L   D0
          RTS

PTR_NF    MOVEQ   #-7,D5
          BRA     FLOAT_RET

STRING    MOVE.L  D6,D1
          MOVE.L  D6,-(A7)
          MOVEA.W $11A,A2
          JSR     (A2)
          MOVEA.L $58(A6),A1
          MOVE.L  (A7)+,D6
          SUBA.W  D6,A1
          MOVE.W  #0,0(A6,A1.L)
          TST.L   D7
          BEQ.S   L00009
          MOVE.W  #4,0(A6,A1.L)
          MOVE.L  D7,2(A6,A1.L)

L00009    MOVE.L  A1,$58(A6)
          MOVEQ   #0,D0
          MOVEQ   #1,D4
          RTS

ERR_BP    MOVE.L  D0,D5
          BRA     FLOAT_RET

OOPS1     MOVEQ   #-15,D0
BAS_RET   MOVE.L  D0,D5
          BRA     FLOAT_RET

GETSCR    MOVE.L  $58(A6),A4
          MOVE.W  $112,A2
          JSR     (A2)
          TST.L   D0
          BEQ.S   OK_PAR
          MOVE.L  A4,$58(A6)
          BRA.S   BAS_RET
OK_PAR    MOVEQ   #0,D1
          CMPI.W  #1,D3            One parameter only
          BNE.S   DEFAULT
          MOVE.W  0(A1,A6.L),D1    Number of screen in D1 please
          MOVE.L  A4,$58(A6)
DEFAULT   RTS

*         BASIC - FUNCTION    'RMODE'

MT_DMODE1 BSR.S   MT_DMODE
          BRA.S   SHOW_RET

MT_DMODE  MOVEQ   #-1,D1
MT_DMODE2 MOVEQ   #-1,D2
          MOVEQ   #$10,D0
          TRAP    #1
          RTS

NO_2SCREEN
          MOVEQ   #-19,D5
          BRA.S   FLOAT_RET

RMODE     MOVE.L  A5,D1
          SUB.L   A3,D1
          CMPI.L  #8,D1
          BGT.S   OOPS1
          BSR.S   GETSCR
          TST.L   D0
          BNE.S   BAS_RET
          TST.W   D1
          BEQ.S   MT_DMODE1      Mode of Default Screen needed
          CMPI.W  #1,D1
          BNE.S   OOPS1
          BSR     MINERVA
          BNE.S   NO_2SCREEN     Not Minerva - one screen only
          MOVEQ   #-128,D1       Get extra information
          BSR.S   MT_DMODE2
          BTST    #7,D1
          BEQ.S   NO_2SCREEN     Only single mode enabled
          MOVEQ   #4,D5
          BTST    #2,D1          Is Other Screen mode 4 or mode 8?
          BEQ.S   FLOAT_RET       NB. Mode 2 not currently supported
          MOVEQ   #8,D1
SHOW_RET
          MOVEQ   #0,D5
          MOVE.W  D1,D5
          BNE.S   FLOAT_RET
          MOVEQ   #4,D5
          BRA.S   FLOAT_RET

*         BEGIN - BASIC FLOAT_RET

FLOAT3    MOVEQ   #0,D2
          MOVE.W  #$0820,D0

FLOAT8    SUBQ.W  #1,D0
          ASL.L   #1,D1
          BVS.S   FLOAT6
          BEQ.S   FLOAT7
          BRA.S   FLOAT8

FLOAT6    ROXR.L  #1,D1
          MOVE.W  D0,D2

FLOAT7    RTS

FLOAT_RET MOVE.L  D5,D1
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

FLOAT5    MOVEM.L D1-D3/A0/A2,-(A7)
          MOVEA.W $11A,A2
          JSR     (A2)
          MOVEM.L (A7)+,D1-D3/A0/A2
          RTS

FLOAT4    BSR.S   FLOAT5
          SUB.L   D1,$58(A6)
          RTS

*         ENDE -  BASIC FLOAT_RET

KEW001    MOVEQ   #8,D0
          ADD.L   A3,D0
          SUB.L   A5,D0
          BNE.S   KEW009
          BSR     KEW010
          BNE.S   KEW011
          MOVE.W  0(A6,A1.L),D1
          ADDQ.L  #2,A1
          BSR.S   KEW012
          RTS

KEW009    MOVEQ   #-15,D0

KEW011    ADDQ.L  #4,A7
          RTS

*         BASIC - FUNCTION    'KEYW'

KEYWORD   MOVEQ   #0,D5
          BSR.S   KEW001
          MOVE.L  D0,D5
          BRA.S   FLOAT_RET           * FLOAT_RET D5

KEW012    MOVEM.L D2/D4/A0-A2,-(A7)
          TRAP    #0
          ADDA.L  A6,A1
          MOVE.L  A6,-(A7)
          MOVE.L  D1,D4
          MOVEQ   #0,D0
          TRAP    #1
          MOVE.L  D4,D1
          MOVEA.L $68(A0),A0
          MOVEA.L (A0),A0
          ADDA.W  #$68,A0
          MOVEA.L A0,A6
          SUBA.L  A6,A1
          MOVEA.L $18(A6),A3

KEW016    CMPA.L  $1C(A6),A3
          BCC.S   KEW013
          MOVE.W  2(A6,A3.L),D2
          BMI.S   KEW014
          MOVEA.W D2,A2
          ADDA.L  $20(A6),A2
          MOVEQ   #0,D2
          MOVE.B  0(A6,A2.L),D2
          CMP.W   D1,D2
          BEQ.S   KEW015

KEW014    ADDQ.L  #8,A3
          BRA.S   KEW016

KEW013    MOVEQ   #-7,D0
          BRA.S   KEW020

KEW015    MOVEA.L A1,A0
          BRA.S   KEW017

KEW018    MOVE.B  0(A6,A0.L),D4
          CMP.B   1(A6,A2.L),D4
          BEQ.S   KEW021
          BCHG    #5,D4
          CMP.B   1(A6,A2.L),D4
          BNE.S   KEW014

KEW021    ADDQ.L  #1,A0
          ADDQ.L  #1,A2

KEW017    DBRA    D2,KEW018
          CMPI.W  #$0800,0(A6,A3.L)
          BEQ.S   KEW019
          CMPI.W  #$0900,0(A6,A3.L)
          BNE.S   KEW013

KEW019    MOVEQ   #0,D0

KEW020    MOVEA.L (A7)+,A6
          ANDI.W  #$D8FF,SR
          MOVEM.L (A7)+,D2/D4/A0-A2
          RTS

KEW010    MOVEM.L D1-D4/D6/A0/A2/A5,-(A7)
          LEA     8(A3),A5
          MOVEA.W $116,A2
          JSR     (A2)
          BNE.S   KEW022
          MOVEQ   #0,D1
          MOVE.W  0(A6,A1.L),D1
          ADDQ.L  #3,D1
          BCLR    #0,D1
          ADD.L   A1,D1
          MOVE.L  D1,$58(A6)

KEW022    MOVEA.L A5,A3
          MOVEM.L (A7)+,D1-D4/D6/A0/A2/A5
          TST.L   D0
          RTS

*         ANFANG  GET_STRING

GETS01    LSL.L   #3,D1
          MOVE.L  A5,D0
          SUB.L   A3,D0
          SUB.L   D1,D0
          BEQ.S   GETS22
          MOVEQ   #-15,D0

GETS22    RTS

GETS03    MOVE.L  $58(A6),D5
          MOVEA.W $116,A2
          JSR     (A2)
          BNE.S   GETS20
          MOVEQ   #0,D0
          BRA.S   GETS21

GETS20    MOVEQ   #-1,D0

GETS21    MOVE.L  D5,$58(A6)
          TST.L   D0
          RTS

*         BASIC - FUNCTION    'TH_VER$'

TH_VER    MOVEQ   #1,D1
          BSR.S   GETS01
          BNE.S   GETS22
          BSR.S   GETS03
          BNE.S   GETS22
          LEA     0(A6,A1.L),A0
          MOVE.L  A0,D6
          BSR.S   FIND_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVEA.L 8(A3),A4
          MOVE.L  D6,A0
          BSR.S   USE_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVE.L  D3,D7
          MOVE.L  A1,A3
          MOVE.L  D6,A0
          BSR.S   FREE_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVE.L  #6,D6
          BRA     STRING

*         BASIC - FUNCTION    'THING'

THING     MOVEQ   #1,D1
          BSR.S   GETS01
          BNE.S   GETS22
          BSR.S   GETS03
          BNE.S   GETS22
          LEA     0(A6,A1.L),A0
          MOVE.L  A0,D6
          BSR.S   FIND_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVEA.L 8(A3),A4
          MOVE.L  D6,A0
          BSR.S   USE_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVE.L  A1,A3
          MOVE.L  D6,A0
          BSR.S   FREE_THG
          TST.L   D0
          BNE.S   ERREXIT
          MOVEQ   #0,D5
          BRA     FLOAT_RET                * FLOAT_RET D5

ERREXIT   MOVE.L  D0,D5
          BRA     FLOAT_RET

FIND_THG  MOVEQ   #0,D0
          TRAP    #1
          MOVEQ   #-7,D0
          MOVE.L  $B8(A0),D5
          BEQ.S   FIND_EXIT

F_LOOP    MOVE.L  D5,A0
          MOVE.L  (A0),D5
          BNE.S   F_LOOP
          MOVE.L  $10(A0),A3
          CMPI.L  #'THG%',(A3)
          BNE.S   FIND_EXIT
          CMPI.L  #-1,4(A3)
          BNE.S   FIND_EXIT
          MOVEQ   #0,D0
FIND_EXIT RTS

USE_THG   MOVEQ   #$28,D0
          MOVEQ   #-1,D1
          MOVEQ   #0,D2
          MOVEQ   #-1,D3
          JSR     (A4)
          RTS

FREE_THG  MOVEQ   #$29,D0
          MOVEQ   #-1,D1
          JSR     (A4)
          CMP.L   #0,D0
          BCS.S   FIND_EXIT
          MOVEQ   #0,D0         Note - This routine may return a positive
          RTS                          value in D0 even if no error

*         BASIC - FUNCTION    'SYS_BASE'

SYSBASE   MOVEQ   #0,D0
          TRAP    #1
          MOVE.L  A0,D5
          BRA     FLOAT_RET

*         BASIC - FUNCTION    'WIN_BASE'

GET_CH    MOVEQ   #1,D6
          CMPA.L  A3,A5
          BEQ.S   GET_CH1
          BTST    #7,1(A6,A3.L)
          BEQ.S   GET_CH1
          MOVE.L  A5,-(A7)
          MOVEA.L A3,A5
          ADDQ.L  #8,A5
          MOVE.L  A5,-(A7)
          MOVEA.W $112,A2
          JSR     (A2)
          MOVEA.L (A7)+,A3
          MOVEA.L (A7)+,A5
          SUBQ.W  #1,D3
          BNE.S   CH_RETURN
          MOVE.W  0(A6,A1.L),D6
          ADDQ.L  #2,$58(A6)

GET_CH1   MULU    #$28,D6
          MOVEQ   #-6,D0
          ADD.L   $30(A6),D6
          CMP.L   $34(A6),D6
          BCC.S   CH_RETURN
          MOVE.L  0(A6,D6.L),D1
          BTST    #$1F,D1
          BNE.S   CH_RETURN
          MOVEA.L D1,A0
          MOVEQ   #0,D0

CH_RETURN TST.L   D0
          RTS

WINBASE   MOVEQ   #0,D4
          BSR.S   GET_CH
          MOVE.L  D0,D1
          BNE.S   WN_B02
          LEA     WN_B03,A2
          MOVEQ   #-1,D3
          MOVEQ   #9,D0
          TRAP    #3
          TST.L   D0
          BNE.S   WN_B04
          BRA.S   WN_B02

WN_B04    MOVE.L  D0,D1
          BRA.S   WN_B02           * FLOAT_RET D1

WN_B03    MOVE.L  A0,D1
          MOVEQ   #0,D0
          RTS

WN_B02    MOVE.L  D1,D5
          BRA     FLOAT_RET

*         BASIC - FUNCTION    'ATARI'

ATARI2    MOVEM.L A0/A1,-(A7)
          SUBA.L  A0,A0
          LEA     $FC0000,A1
          CMPM.L  (A0)+,(A1)+
          BNE.S   ATARI5
          CMPM.L  (A0)+,(A1)+

ATARI5    MOVEM.L (A7)+,A0/A1
          RTS

ATARI     MOVEQ   #1,D5
          BSR.S   ATARI2
          BEQ.S   ATARI3
          MOVEQ   #0,D5

ATARI3    BRA     FLOAT_RET

*         BASIC   FUNKTION  'QFLIM'

QFLIM     BSR.S   PARA01
          BNE.S   QFEXIT
          MOVE.L  A0,A4
          MOVE.W  $112,A2
          JSR     (A2)
          BNE.S   QFEXIT
          MOVEQ   #-15,D0
          SUBQ.W  #1,D3
          BNE.S   QFEXIT
          CMPI.W  #3,0(A6,A1.L)
          BGT.S   QFEXIT
          MOVEQ   #$6C,D0
          MOVEQ   #0,D2
          MOVEQ   #-1,D3
          MOVE.L  (A6),A1
          TRAP    #4
          TRAP    #3
          MOVE.L  $58(A6),A2
          MOVE.W  0(A6,A2.L),D1
          LSL.W   #1,D1
          ADDA.W  D1,A1
          MOVE.W  0(A6,A1.L),0(A6,A2.L)
          MOVEQ   #3,D4
          MOVEQ   #0,D0
QFEXIT    RTS

PARA01    MOVEQ   #1,D6
          CMPA.L  A3,A5
          BEQ.S   PARA32
          BTST    #7,1(A6,A3.L)
          BEQ.S   PARA32
          MOVE.L  A5,-(A7)
          MOVEA.L A3,A5
          ADDQ.L  #8,A5
          MOVE.L  A5,-(A7)
          MOVEA.W $0112,A2
          JSR     (A2)
          MOVEM.L (A7)+,A3/A5
          BNE.S   PARA33
          MOVE.W  0(A6,A1.L),D6
          ADDQ.L  #2,A1
          MOVE.L  A1,$0058(A6)

PARA32    MULU    #$28,D6
          MOVEQ   #-6,D0
          ADD.L   $30(A6),D6
          CMP.L   $34(A6),D6
          BGE.S   PARA33
          MOVEA.L 0(A6,D6.L),A0
          MOVE.W  A0,D1
          BMI.S   PARA33
          MOVEQ   #0,D0

PARA33    RTS

DEFSCR
       BSR     MINERVA
       BEQ.S   YES_MINERVA
ONE_SCREEN
       MOVEQ   #0,D5
       BRA     FLOAT_RET
YES_MINERVA
       MOVEQ   #-128,D1       Read extra information
       MOVEQ   #-1,D2
       MOVEQ   #$10,D0
       TRAP    #1
       BTST    #7,D1
       BEQ.S   ONE_SCREEN     Only single mode enabled
       MOVEQ   #0,D5
       BTST    #5,D1
       BEQ     FLOAT_RET
       MOVEQ   #1,D5
       BRA     FLOAT_RET
       END
