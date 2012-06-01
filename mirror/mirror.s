;***            CubeInAMirror Part
;***            (C) Copyright 1993 by Torsten Curdt/TEK
;***            routine finished in August/December

;               size unpacked:  10092
;               size packed:    5284
;               script:         $340d0
;               Duration:       1565 frames / 31,3 s


WORK            equ     1               ;SCRIPT: ma_myskript
SHOW            equ     0
RECORD          equ     0
MIRROR          equ     1

f               =$111

SHIFT           =$100000

                include "rampage.i"

;««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««

mr_bpr          equ     40
mr_height       equ     256

BACKCOL         equ     $423-$111
BACKCOL2        equ     $453

NORMALCOL1      equ     $c52
NORMALCOL2      equ     $b31
NORMALCOL3      equ     $a21

MIRRORCOL1      equ     $c52+$002-$210
MIRRORCOL2      equ     $b31+$002-$210
MIRRORCOL3      equ     $a21+$002-$210

MIRRORPLATECOL  equ     $006

;               \1 - Long
;               \2 - Word
;               \3 - Dest
mulsLW:         macro
                movem.l d0-d4,-(a7)
                move.l  \1,d0
                move.w  \2,d1
                moveq   #0,d4
                tst.w   d1
                bpl.s   .\@pl1
                neg.w   d1
                addq.w  #1,d4
.\@pl1:         tst.l   d0
                bpl.s   .\@pl2
                neg.l   d0
                subq.w  #1,d4
.\@pl2:         move.w  d0,d2
                swap    d0
                move.w  d0,d3
                mulu    d1,d2
                mulu    d1,d3
                swap    d3
                add.l   d3,d2
                tst.w   d4
                beq.s   .\@nomin
                neg.l   d2
.\@nomin:       move.l  d2,.\@dest+2
                movem.l (a7)+,d0-d4
.\@dest:        move.l  #$00000000,\3
                endm

                org     CODE_AREA
                load    CODE_AREA
                jumpptr CODE_AREA


                IF      WORK=1

su_main:        lea     ma_skript(pc),a0
                bsr.w   manager_init

                bsr     setup

                


;               lea     $dff000,a6
;               move.w  #$7fff,$0096(a6)
;               move.w  #$4000,$009a(a6)
;               move.w  #$0020,$01dc(a6)

                lea     $dff000,a6
                move.l  $64.w,mongo
                move.l  #su_inter,$64.w
                move.l  #su_copper,$0080(a6)
                move.w  #$82c0,$0096(a6)
                move.w  d0,$0088(a6)

                move.w  #$c004,$009a(a6)


su_loop:        jsr     ma_queue
                btst    #6,$bfe001
                bne.s   su_loop


                move.l  mongo,$64.w
                bsr     closedown

;               clr.w   $dff1dc
                rts

su_inter:       movem.l d0-d7/a0-a6,-(a7)
                lea     $dff01e,a6
                move.w  (a6),$9c-$1e(a6)
                bsr.w   manager
                movem.l (a7)+,d0-d7/a0-a6
                rte

mongo           dc.l    0

SETUP_VBR0      =1
SETUP_CACHEOFF  =0

                include "setup.i"


;=========================================================================
;
;               MANAGER 2.0     1993, (C)aptain Bifat /TEK
;                               created 2.8.1993

ma_maxcalls     =6
ma_maxqueue     =10
ma_maxgosub     =4

                rsreset
man_struc       rs.b    0               ; *** MANAGER2.0 - Struktur ***
man_aktframe    rs.l    1               ; aktuelles Frame
man_nextframe   rs.l    1               ; nächstes Frame
man_label       rs.l    1               ; aktuelles Label
man_labelstack  rs.l    ma_maxgosub     ; Stack für Unterprogramm-Labels
man_framestack  rs.l    ma_maxgosub     ; Stack für Unterprogramm-Frames
man_stackptr    rs.w    1               ; Stackpointer für Unterprogramm-Stack
man_calltable   rs.l    ma_maxcalls     ; Call-Tabelle
man_pcallflags  rs.w    ma_maxcalls     ; 0=call, 1=pcall
man_anzcall     rs.l    ma_maxcalls     ; Call-Counter-Tabelle
man_pcallpara   rs.l    ma_maxcalls*4   ; Parameterlisten für PCALL
man_queuetable  rs.l    ma_maxqueue     ; Queue-Calltable
man_pqueuepara  rs.l    ma_maxqueue*4   ; Parameterlisten für Queue
man_queuepos    rs.w    1               ; Position in QueueTable
man_SIZEOF      rs.b    0

managerstruc    blk.b   man_SIZEOF,0


manager_init    ;       a0      Pointer auf Skript
                lea     managerstruc+man_SIZEOF(pc),a1
                move.w  #man_SIZEOF/2-1,d0
ma_clear        clr.w   -(a1)
                dbf     d0,ma_clear
                move.l  (a0),man_nextframe(a1)
                addq.l  #4,a0
                move.l  a0,man_label(a1)
                rts

manager         lea     managerstruc(pc),a0
                move.l  man_aktframe(a0),d7
                cmp.l   man_nextframe(a0),d7
                bne.w   ma_nonew
                move.l  man_label(a0),a1
ma_getnew       move.l  (a1)+,d0                        ; Opcode
                move.l  ma_labeltab(pc,d0.w),a2
                jmp     (a2)
ma_labeltab     dc.l    ma_com_call,ma_com_pcall
                dc.l    ma_com_queue,ma_com_pqueue
                dc.l    ma_com_switch,ma_com_goto
                dc.l    ma_com_gosub,ma_com_return
                dc.l    ma_com_nop,ma_com_move,ma_com_add
                dc.l    ma_com_beq,ma_com_bne,ma_com_kill
ma_com_move     move.l  (a1)+,a2
                move.l  (a1)+,d0
                move.w  d0,(a2)
                bra.w   ma_checkmore
ma_com_add      move.l  (a1)+,a2
                move.l  (a1)+,d0
                add.w   d0,(a2)
                bra.w   ma_checkmore
ma_com_beq      move.l  (a1)+,d1                ; Label
                move.l  (a1)+,a2                ; Adresse
                move.l  (a1)+,d0                ; Wert
                cmp.w   (a2),d0
                bne.w   ma_checkmore
                move.l  d1,a1
                move.l  (a1)+,d7
                move.l  d7,man_aktframe(a0)
                bra.b   ma_getnew
ma_com_bne      move.l  (a1)+,d1                ; Label
                move.l  (a1)+,a2                ; Adresse
                move.l  (a1)+,d0                ; Wert
                cmp.w   (a2),d0
                beq.w   ma_checkmore
                move.l  d1,a1
                move.l  (a1)+,d7
                move.l  d7,man_aktframe(a0)
                bra.b   ma_getnew
ma_com_gosub    move.w  man_stackptr(a0),d0
                lea     (a0,d0.w),a2
                move.l  d7,man_framestack(a2)   ; aktuelles Frame auf Stack
                move.l  (a1)+,a3                ; neues Label
                move.l  (a3)+,d7                ; neues aktuelles Frame
                move.l  d7,man_aktframe(a0)     ; merken
                move.l  a1,man_labelstack(a2)   ; Label nach GOSUB auf Stack
                move.l  a3,a1                   ; neues Label
                addq.w  #4,man_stackptr(a0)     ; Stack erhöhen
                bra.w   ma_getnew
ma_com_return   subq.w  #4,man_stackptr(a0)     ; Stack erniedrigen
                move.w  man_stackptr(a0),d0
                lea     (a0,d0.w),a2
                add.l   man_framestack(a2),d7   ; gemerktes Frame + akt. Frame
                move.l  d7,man_aktframe(a0)             
                move.l  man_labelstack(a2),a1
                bra.w   ma_checkmore
ma_com_switch   move.l  (a1)+,$dff080           ; Copperlist
                bra.w   ma_checkmore
ma_com_goto     move.l  (a1)+,a1                ; neues Label
                move.l  (a1)+,d7                ; neues aktuelles Frame
                move.l  d7,man_aktframe(a0)
                bra.w   ma_getnew
ma_com_queue    move.w  man_queuepos(a0),d0
                lea     (a0,d0.w),a2
                move.l  (a1)+,man_queuetable(a2)
                addq.w  #4,d0
                cmp.w   #ma_maxqueue*4,d0
                beq.w   ma_checkmore
                move.w  d0,man_queuepos(a0)
                bra.w   ma_checkmore
ma_com_pqueue   move.w  man_queuepos(a0),d0
                lea     (a0,d0.w),a2
                move.l  (a1)+,man_queuetable(a2)
                movem.l (a1)+,d1-d4             ; Parameter
                move.w  d0,d5
                add.w   d5,d5
                add.w   d5,d5
                lea     (a0,d5.w),a2
                movem.l d1-d4,man_pqueuepara(a2)                
                addq.w  #4,d0
                cmp.w   #ma_maxqueue*4,d0
                beq.s   ma_checkmore
                move.w  d0,man_queuepos(a0)
                bra.s   ma_checkmore
ma_com_kill     move.l  (a1)+,d0
                neg.w   d0
                add.w   #ma_maxcalls-1,d0
                add.w   d0,d0
                add.w   d0,d0
                lea     (a0,d0.w),a2
                clr.l   man_anzcall(a2)
                bra.s   ma_checkmore
ma_com_call     movem.l (a1)+,d0-d2             ; Routine/Anzahl/nr
                neg.w   d2
                add.w   #ma_maxcalls-1,d2
                add.w   d2,d2
                lea     (a0,d2.w),a2
                clr.w   man_pcallflags(a2)
                add.w   d2,d2
                lea     (a0,d2.w),a2
                move.l  d0,man_calltable(a2)    ; Call eintragen
                move.l  d1,man_anzcall(a2)      ; Anzahl Aufrufe eintragen
                bra.s   ma_checkmore
ma_com_pcall    movem.l (a1)+,d0-d6             ; Routine/Anzahl/nr/d0/d1/a0/a1
                neg.w   d2
                add.w   #ma_maxcalls-1,d2
                add.w   d2,d2
                lea     (a0,d2.w),a2
                move.w  #1,man_pcallflags(a2)
                add.w   d2,d2
                lea     (a0,d2.w),a2
                move.l  d0,man_calltable(a2)    ; Call eintragen
                move.l  d1,man_anzcall(a2)      ; Anzahl Aufrufe eintragen
                add.w   d2,d2
                add.w   d2,d2           
                lea     (a0,d2.w),a2
                movem.l d3-d6,man_pcallpara(a2) ; Parameter eintragen
ma_com_nop
ma_checkmore    move.l  (a1)+,d0
                cmp.l   d0,d7
                beq.w   ma_getnew
                move.l  d0,man_nextframe(a0)
                move.l  a1,man_label(a0)
ma_nonew        addq.l  #1,man_aktframe(a0)
                lea     man_pcallpara(a0),a1    ; CALL/PCALL abarbeiten
                moveq   #ma_maxcalls-1,d0
ma_cloop        tst.l   -(a1)
                beq.s   ma_notactive
                subq.l  #1,(a1)                 ; anzcall erniedrigen
                move.w  d0,d7
                movem.l d0/a0/a1,-(a7)
                add.w   d7,d7
                lea     (a0,d7.w),a2
                add.w   d7,d7
                lea     (a0,d7.w),a3
                tst.w   man_pcallflags(a2)
                beq.s   ma_nopcall              ; kein PCALL, normaler CALL
                add.w   d7,d7
                add.w   d7,d7
                lea     (a0,d7.w),a4
                movem.l man_pcallpara(a4),d0/d1/a0/a1
ma_nopcall      move.l  man_calltable(a3),a2
                jsr     (a2)
                movem.l (a7)+,d0/a0/a1
ma_notactive    dbf     d0,ma_cloop
ma_ende         rts
ma_queue        lea     managerstruc(pc),a2
                move.l  man_queuetable(a2),d2
                beq.s   ma_queueempty
                movem.l man_pqueuepara(a2),d0/d1/a0/a1
                move.l  d2,a2
                jsr     (a2)
                lea     managerstruc(pc),a0
                lea     man_queuetable(a0),a1
                lea     man_pqueuepara(a0),a2
                move.w  man_queuepos(a0),d0
                beq.s   ma_q1
                lsr.w   #2,d0
                subq.w  #1,d0
                moveq   #16,d5
ma_movequeue    move.l  4(a1),(a1)+             
                movem.l 16(a2),d1-d4
                movem.l d1-d4,(a2)
                add.l   d5,a2
                dbf     d0,ma_movequeue
ma_q1           clr.l   (a1)
                subq.w  #4,man_queuepos(a0)
ma_queueempty   rts

su_copper:      dc.w    $009c,$8004
                dc.w    $ffff,$fffe

ma_skript:      IF      MIRROR=0
                dc.l    0,gosub,mr_trans
                ENDIF
                dc.l    00000,call,mucke,-1,0
                dc.l    00000,switch,bg_copper
                dc.l    00100,gosub,ma_myskript
                dc.l       -1,goto,ma_skript

                ENDIF

mucke           moveq   #14,d0
lop             blk.w   100,$4e71
                dbf     d0,lop
                rts

;=========================================================================
; opcode        |       Syntax
;---------------+---------------------------------------------------------
call=0*4        ;       frame,CALL,routine,anzahl,nr
pcall=1*4       ;       frame,PCALL,routine,anzahl,nr,d0,d1,a0,a1
queue=2*4       ;       frame,QUEUE,routine
pqueue=3*4      ;       frame,PQUEUE,routine,d0,d1,a0,a1
switch=4*4      ;       frame,SWITCH,copperlist
goto=5*4        ;       frame,GOTO,label
gosub=6*4       ;       frame,GOSUB,label
return=7*4      ;       frame,RETURN
nop=8*4         ;       frame,NOP
move=9*4        ;       frame,MOVE,adresse,wert
add=10*4        ;       frame,ADD,adresse,wert
beq=11*4        ;       frame,BEQ,label,adresse,wert
bne=12*4        ;       frame,BNE,label,adresse,wert
kill=13*4       ;       frame,KILL,nr
;;«««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««««
bg_time         equ     45
mr_wait1        equ     40
mr_wait2        equ     1115

ma_myskript:
        dc.l    00000,switch,bg_copper
        dc.l    00000,call,bg_stripes,-1,1
        dc.l    00000,queue,mr_clearmem                 ; !

        dc.l    00000+bg_time,kill,1
        dc.l    00000+bg_time,queue,mr_init             ; !

        dc.l    00000+bg_time,queue,vt_init             ; !


        if      WORK=0
        dc.l    bg_time,queue,jumpptr_prepare           ; Prepare next
        endif


        dc.l    00002+bg_time,move,mr_priorstatus,1
        dc.l    00002+bg_time,move,mr_mirrorstatus,0
        dc.l    00002+bg_time,move,mr_cubestatus,0
        dc.l    00002+bg_time,call,mr_create,-1,1
        dc.l    00002+bg_time,pcall,mi_motioninterpreter,-1,2,0,0,mi_motionlist,0

        dc.l    00003+bg_time,switch,mr_maincopper

        dc.l    00005+bg_time+5,move,mr_cubestatus,1

        dc.l    00005+bg_time+mr_wait1,move,mr_priorstatus,0
        dc.l    00005+bg_time+mr_wait1,move,mr_mirrorstatus,1

        dc.l    00007+bg_time+mr_wait1+mr_wait2,move,mr_priorstatus,1
        dc.l    00007+bg_time+mr_wait1+mr_wait2,move,mr_mirrorstatus,0
        dc.l    00007+bg_time+mr_wait1+mr_wait2,call,mr_clearoldmirrorcube2,1,3

mr_trans:
        dc.l    00160+bg_time+mr_wait1+mr_wait2,kill,1
        dc.l    00162+bg_time+mr_wait1+mr_wait2,call,vt_transit,-1,2
        dc.l    00163+bg_time+mr_wait1+mr_wait2,switch,vt_copper
        dc.l    00163+bg_time+mr_wait1+mr_wait2,pcall,vt_fadenormal,20,3,15,0,0,0

        dc.l    00163+bg_time+mr_wait1+mr_wait2+15,pcall,vt_nomask,vt_bpr,4,0,0,0,0

        IF      RECORD=1
        dc.l    00163+bg_time+mr_wait1+mr_wait2+15+vt_bpr+1,call,vt_sign,-1,2
        dc.l    00163+bg_time+mr_wait1+mr_wait2+13+vt_bpr+1,call,vt_clear,1,2
        ELSE
        dc.l    345+bg_time+mr_wait1+mr_wait2,pcall,vt_fadewhite,5,3,5,0,0,0
        dc.l    350+bg_time+mr_wait1+mr_wait2,pcall,vt_fadeback,11,3,10,0,0,0
        dc.l    361+bg_time+mr_wait1+mr_wait2,call,vt_clear,1,2
        ENDIF

        dc.l    1565,return

;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

mi_sinusbase    equ     512
mi_maxobj       equ     2
mi_turnline     equ     4*7
mi_moveline     equ     4*6
mi_alpha        equ     0
mi_beta         equ     2
mi_gamma        equ     4
mi_addx         equ     0
mi_addy         equ     2
mi_addz         equ     4

;               a0 - motionlist
mi_motioninterpreter:
                lea     mi_turntable-mi_turnline*3(pc),a6
                lea     mi_movetable-mi_moveline*3(pc),a5
.nextobj:       lea     mi_turnline*3(a6),a6    ;next turnframe
                lea     mi_moveline*3(a5),a5    ;next moveframe
                move.l  (a0),d0
                bge.w   .doobj

                lea     mi_turntable(pc),a6     ;execute turns
                moveq   #mi_maxobj*3-1,d7
.turn:          move.l  (a6),d0
                bmi.s   .noTurn
                movem.l 4(a6),d1/d2/d3/d6/a0/a1
                mulu    d6,d0
                mulu    d6,d3
                move.l  mi_aktframe(pc),d4
                sub.w   d1,d4                   ;delta frame
                cmp.w   d0,d4                   ;frames = delta ?
                blt.s   .noKickTurn
                move.l  #-1,(a6)                ;kick turn
.noKickTurn:    muls    d4,d3
                divs    d0,d3
                muls    #mi_sinusbase,d3        ;° to offset
                divs    #360,d3
                add.w   d3,d3
                add.w   d2,d3
                add.w   #2*mi_sinusbase,d3
                and.w   #2*(mi_sinusbase-1),d3
                move.w  d3,(a1)
.noTurn:        lea     mi_turnline(a6),a6
                dbf     d7,.turn

                lea     mi_movetable(pc),a6     ;execute moves
                moveq   #mi_maxobj*3-1,d7
.move:          move.l  (a6),d0
                bmi.s   .noMove
                movem.l 4(a6),d1/d2/d3/d4/a1
                move.l  mi_aktframe(pc),d4
                sub.w   d1,d4                   ;delta frame
                cmp.w   d0,d4                   ;frames = delta ?
                blt.s   .noKickMove
                move.l  #-1,(a6)                ;kick move
.noKickMove:    muls    d4,d3
                divs    d0,d3
                add.w   d2,d3
                move.w  d3,(a1)
.noMove:        lea     mi_moveline(a6),a6
                dbf     d7,.move

                addq.l  #1,mi_aktframe          ;inc framecounter
                rts

.doobj:         move.l  d0,a1
.nextcom:       move.l  (a1)+,d1
                cmp.l   mi_aktframe(pc),d1      ;test if right frame
                bmi.s   .tonextobj
                ble.s   .doit
.tonextobj:     lea     3*4(a0),a0              ;next object
                bra.w   .nextobj
.doit:          move.l  (a1)+,d1                ;get opcode
                move.l  .oc_tab(pc,d1.w),a2
                jmp     (a2)
.oc_tab:        dc.l    .c_turn
                dc.l    .c_setangle
                dc.l    .c_setpos
                dc.l    .c_move

.c_turn:        movem.l (a1)+,d1/d2/d3/d4/d5    ;init turn
                move.l  4(a0),a2
                lea     (a2,d1.w),a2
                mulu    #mi_turnline/2,d1
                lea     (a6,d1.w),a3
                move.l  d4,(a3)                 ;frames
                move.l  mi_aktframe(pc),4(a3)   ;startfr
                moveq   #0,d6
                move.w  (a2),d6
                move.l  d6,8(a3)                ;fromangle
                move.l  d2,12(a3)               ;angles
                move.l  d5,16(a3)               ;times
                move.l  d3,20(a3)               ;acc
                move.l  a2,24(a3)               ;dest
                move.l  a1,(a0)
                bra.s   .nextcom

.c_move:        movem.l (a1)+,d1/d2/d3/d4       ;init move
                move.l  8(a0),a2
                lea     (a2,d1.w),a2
                mulu    #mi_moveline/2,d1
                lea     (a5,d1.w),a3
                move.l  d4,(a3)                 ;frames
                move.l  mi_aktframe(pc),4(a3)   ;startfr
                move.w  (a2),d5
                ext.l   d5
                move.l  d5,8(a3)                ;frompos
                move.l  d2,12(a3)               ;dis
                move.l  d3,16(a3)               ;acc
                move.l  a2,20(a3)               ;dest
                move.l  a1,(a0)
                bra.w   .nextcom

.c_setangle:    movem.l (a1)+,d1/d2                     ;set angle in °
                mulu    #mi_sinusbase,d2
                divu    #360,d2
                add.w   d2,d2
                move.l  4(a0),a2
                and.w   #2*(mi_sinusbase-1),d2
                move.w  d2,(a2,d1.w)
                move.l  a1,(a0)
                bra.w   .nextcom

.c_setpos:      movem.l (a1)+,d1/d2                     ;set pos
                move.l  8(a0),a2
                move.w  d2,(a2,d1.w)
                move.l  a1,(a0)
                bra.w   .nextcom

mi_turntable:   dcb.b   mi_maxobj*mi_turnline*3,-1
                ;frames,startfr,fromangle,angles,times,acc,dest

mi_movetable:   dcb.b   mi_maxobj*mi_moveline*3,-1
                ;frames,startfr,frompos,dis,acc,dest

mi_aktframe:    dc.l    0
mi_motionlist:  dc.l    mr_cubemotion,mr_cubeAlpha,mr_cubeAddX
                dc.l    mr_mirrormotion,mr_mirrorAlpha,mr_mirrorAddX
                dc.l    -1

;=======================================================================
;Opcode         |       Syntax
;---------------+-------------------------------------------------------
mi_turn=0*4     ;       frame,mi_TURN,angleId,angle,acc,time,times
mi_setangle=1*4 ;       frame,mi_SETANGLE,angleId,angle
mi_setpos=2*4   ;       frame,mi_SETPOS,addId,pos
mi_move=3*4     ;       frame,mi_MOVE,addId,pos,acc,time


mr_cubemotion:  dc.l    00050,mi_turn,     mi_beta,    360,0, 1*50,5
                dc.l    00050,mi_turn,     mi_alpha,   360,0, 5*50,1
                dc.l    00050,mi_move,     mi_addx,   -800,0, 5*50
                dc.l    00050,mi_move,     mi_addy,   -400,0, 5*50
                dc.l    00301,mi_turn,     mi_beta,    360,0, 3*50,5
                dc.l    00301,mi_move,     mi_addx,  2*800,0, 5*50
                dc.l    00301,mi_move,     mi_addy,  2*400,0, 5*50
                dc.l    01000,mi_turn,     mi_alpha,   360,0, 5*50,3
                dc.l    01000,mi_turn,     mi_beta,    360,0, 4*50,3
                dc.l    01200,mi_move,     mi_addx,   -800,0, 2*50
                dc.l    01200,mi_move,     mi_addy,   -400,0, 2*50
                dc.l    01200,mi_move,     mi_addz,   -800,0, 2*50
                dc.l    -1

mr_mirrormotion:
                dc.l    00000,mi_setpos,   mi_addx,   -500
                dc.l    00000,mi_setpos,   mi_addz,   -300
                dc.l    00001,mi_move,     mi_addx,  -1000,0, 1*50
                dc.l    00001,mi_move,     mi_addz,  -3700,0, 1*50
                dc.l    00010,mi_turn,     mi_beta,     45,0, 4*50,1
                dc.l    00010,mi_turn,     mi_alpha,   -10,0, 4*50,1
                dc.l    00055,mi_move,     mi_addx,   2000,0, 50
                dc.l    00250,mi_turn,     mi_beta,    -60,0,10*50,1
                dc.l    00250,mi_turn,     mi_alpha,    20,0, 4*50,1
                dc.l    00800,mi_move,     mi_addx,  -1300,0, 1*50
                dc.l    00800,mi_move,     mi_addy,   -500,0, 1*50
                dc.l    00800,mi_turn,     mi_alpha,    30,0, 1*50,1
                dc.l    00800,mi_turn,     mi_beta,    -30,0, 1*50,1
                dc.l    00900,mi_turn,     mi_beta,    360,0, 6*50,1
                dc.l    01200,mi_turn,     mi_alpha,   -40,0, 1*50,1
                dc.l    01200,mi_turn,     mi_beta,     45,0, 1*50,1
                dc.l    01200,mi_move,     mi_addx,    800,0, 2*50
                dc.l    01200,mi_move,     mi_addy,    500,0, 2*50
                dc.l    01200,mi_move,     mi_addz,   1200,0, 2*50
                dc.l    01300,mi_setangle, mi_beta,      0
                dc.l    -1

;//////////////////////////////////////////////////////////////////////////////
;//////////////////////////////////////////////////////////////////////////////
;//////////////////////////////////////////////////////////////////////////////

bg_stripes:     lea     bg_stripesplane(pc),a0
                lea     bg_statuslist(pc),a2
                moveq   #22-1,d6
.setstripe:     move.w  (a2)+,d0
                cmp.w   #2*5,(a2)
                blt.s   .noAdd
                cmp.w   #2*90*512/360,-2(a2)
                bgt.s   .max
                add.w   #2*5,-2(a2)
                bra.s   .noAdd
.max:           move.w  #2*90*512/360,-2(a2)
.noAdd:         bsr.s   bg_makestripe
                dbf     d6,.setstripe
                rts

;               a0 - stripedest
;               d0 - angle
bg_makestripe:  lea     mr_sinus(pc),a1
                and.w   #2*(mi_sinusbase-1),d0
                move.w  (a1,d0.w),d0
                muls    #9,d0
                add.l   d0,d0
                swap    d0
                tst.w   d0
                bge.w   .ok
                neg.w   d0
.ok:            move.w  #$00ff,d1
                lsl.w   d0,d1
                and.w   #$ff00,d1
                move.w  d1,d7
                move.w  #$ff00,d1
                lsr.w   d0,d1
                and.w   #$00ff,d1
                or.w    d1,d7
                move.w  d7,(a0)+
                rts

bg_statuslist:  dcb.w   22,0
                dc.w    $7fff

FREESPRITE:     dc.l    0,0

bg_copper:      dc.w    $009c,$8004

                dc.w    $0096,$0160
                dc.w    $0096,$83e0
                dc.w    $00e0,bg_stripesplane>>16
                dc.w    $00e2,bg_stripesplane&$ffff
                dc.w    $008e,$2471
                dc.w    $0090,$33d1
                dc.w    $0092,$0030
                dc.w    $0094,$00d8
                dc.w    $0100,$1200
                dc.w    $0102,$0000
                dc.w    $0104,$0040
                dc.w    $0108,-44
                dc.w    $0180,$0312
;               dc.w    $0182,$0007
                dc.w    $182,$45a

        dc.w    $96,$8020                               ; Sprite-DMA ein
        dc.w    $120,SPRITE_EMBLEM_1>>16,$122,SPRITE_EMBLEM_1&$ffff
        dc.w    $124,SPRITE_EMBLEM_2>>16,$126,SPRITE_EMBLEM_2&$ffff
        dc.w    $128,SPRITE_EMBLEM_3>>16,$12a,SPRITE_EMBLEM_3&$ffff
        dc.w    $12c,SPRITE_EMBLEM_4>>16,$12e,SPRITE_EMBLEM_4&$ffff
        dc.w    $130,SPRITE_EMBLEM_5>>16,$132,SPRITE_EMBLEM_5&$ffff
        dc.w    $134,SPRITE_EMBLEM_6>>16,$136,SPRITE_EMBLEM_6&$ffff
        dc.w    $138,FREESPRITE>>16,$13a,FREESPRITE&$ffff
        dc.w    $13c,FREESPRITE>>16,$13e,FREESPRITE&$ffff
;       dc.w    $104,0                                  ; Sprites hinten
        dc.w    $1a2,$423,$1a4,$412,$1a6,$201           ; Spritefarben
        dc.w    $1aa,$423,$1ac,$412,$1ae,$201
        dc.w    $1b2,$423,$1b4,$412,$1b6,$201
        dc.w    $1ba,$423,$1bc,$412,$1be,$201

        dc.w    $8759,-2,$180,$201,$8779,-2,$180,$312
        dc.w    $87a3,-2,$180,$201,$87c5,-2,$180,$312
        dc.w    $8859,-2,$180,$201,$8879,-2,$180,$312
        dc.w    $88a3,-2,$180,$201,$88c5,-2,$180,$312

                dc.w    $ffff,$fffe

;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
;((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((

mr_clearmem:    lea     mr_frame1,a1
                move.w  #(mr_raw-mr_frame1)/4-1,d7
.clr:           clr.l   (a1)+
                dbf     d7,.clr
                rts

mr_init:        lea     mr_mulu,a0
                moveq   #0,d0
                move.w  #mr_height-1,d7
.add:           move.w  d0,(a0)+
                add.w   #mr_bpr,d0
                dbf     d7,.add
                rts

mr_changebuffer:movem.l mr_cubefront,d0-d5
                exg.l   d0,d3
                exg.l   d1,d4
                exg.l   d2,d5
                movem.l d0-d5,mr_cubefront
                movem.l mr_mirrorfront,d0-d3
                exg.l   d0,d2
                exg.l   d1,d3
                movem.l d0-d3,mr_mirrorfront
                IF      SHOW=1
                bchg.b  #1,$bfe001
                ENDIF
                rts

mr_create:      bsr.s   mr_changebuffer
                bsr.w   mr_clearoldmirror               ;mirror
                bsr.w   mr_rotatemirror
                bsr.w   mr_drawmirror
                tst.w   mr_cubestatus
                beq.s   .no_cube
                bsr.w   mr_clearoldcube                 ;cube
                bsr.w   mr_rotatecube
                bsr.w   mr_cubebacks
                bsr.w   mr_drawcube
.no_cube:       tst.w   mr_mirrorstatus
                beq.s   .no_mirror
                bsr.w   mr_mirror                       ;get mirror-koord
                bsr.w   mr_clearoldmirrorcube           ;mirrorcube
                bsr.w   mr_mirrorcubebacks
                bsr.w   mr_drawmirrorcube
.no_mirror:     bsr.w   mr_mirrorlightsource            ;lightsource mirror
                bsr.w   mr_prior
                bsr.s   mr_setplanes
                IF      SHOW=1
                move.w  #$500,$dff180
                ENDIF
                rts
mr_cubestatus:  dc.w    0
mr_mirrorstatus:dc.w    0

mr_setplanes:   lea     mr_planeptr+2(pc),a0
                lea     mr_planeptrO+2(pc),a1
                move.l  mr_cubeback(pc),d0      ;set cube
                move.w  d0,4+2*8(a0)
                move.w  d0,4+2*8(a1)
                swap    d0
                move.w  d0,0+2*8(a0)
                move.w  d0,0+2*8(a1)
                swap    d0
                add.l   #mr_bpr*mr_height,d0
                move.w  d0,4+3*8(a0)
                move.w  d0,4+3*8(a1)
                swap    d0
                move.w  d0,0+3*8(a0)
                move.w  d0,0+3*8(a1)
                swap    d0
                add.l   #mr_bpr*mr_height,d0
                move.w  d0,4+0*8(a0)
                move.w  d0,4+0*8(a1)
                swap    d0
                move.w  d0,0+0*8(a0)
                move.w  d0,0+0*8(a1)
                swap    d0
                add.l   #mr_bpr*mr_height,d0
                move.w  d0,4+1*8(a0)
                move.w  d0,4+1*8(a1)
                swap    d0
                move.w  d0,0+1*8(a0)
                move.w  d0,0+1*8(a1)
                move.l  mr_mirrorback(pc),d0    ;set mirror
                move.w  d0,4+4*8(a0)
                move.w  d0,4+4*8(a1)
                swap    d0
                move.w  d0,0+4*8(a0)
                move.w  d0,0+4*8(a1)
                rts


mr_clearoldmirror:
                lea     $dff000,a6
                movem.l mr_mirrorback(pc),a0-a1
                add.l   (a1),a0
                move.l  #$01000002,d0
                moveq   #-1,d1
                move.w  6(a1),d2
                beq.s   .noClear
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
.noClear:       rts

mr_rotatemirror:lea     mr_raw,a1               ;raw-koor
                lea     mr_mirrorAlpha(pc),a2   ;angles and adds
                lea     mr_mirrorNewPoints(pc),a3 ;new cornerlist
                lea     mr_mirrorPoints(pc),a4  ;cornerlist
                move.w  #6-1,a5                 ;number of points to rotate

mr_rotate:      lea     mr_sinus(pc),a0
                move.w  (a2),d5                 ;alpha
                move.w  2(a2),d6                ;beta
                move.w  4(a2),d7                ;gamma
                move.w  (a0,d5.w),d0
                move.w  (a0,d6.w),d1
                move.w  (a0,d7.w),d2
                lea     (mr_cosinus-mr_sinus)(a0),a0
                move.w  (a0,d5.w),d3
                move.w  (a0,d6.w),d4
                move.w  (a0,d7.w),d5
                move.w  d4,d6                   ; A
                muls    d5,d6
                add.l   d6,d6
                swap    d6
                move.w  d6,.a+2
                move.w  d4,d6                   ; B
                muls    d2,d6
                add.l   d6,d6
                swap    d6
                move.w  d6,.b+2
                move.w  d1,d6                   ; C
                neg.w   d6
                move.w  d6,.c+2
                move.w  d0,d6                   ; D
                muls    d1,d6
                add.l   d6,d6
                swap    d6
                muls    d5,d6
                add.l   d6,d6
                swap    d6
                move.w  d3,d7
                muls    d2,d7
                add.l   d7,d7
                swap    d7
                sub.w   d7,d6
                move.w  d6,.d+2
                move.w  d0,d6                   ; E
                muls    d1,d6
                add.l   d6,d6
                swap    d6
                muls    d2,d6
                add.l   d6,d6
                swap    d6
                move.w  d3,d7
                muls    d5,d7
                add.l   d7,d7
                swap    d7
                add.l   d7,d6
                move.w  d6,.e+2
                move.w  d0,d6                   ; F
                muls    d4,d6
                add.l   d6,d6
                swap    d6
                move.w  d6,.f+2
                move.w  d3,d6                   ; G
                muls    d1,d6
                add.l   d6,d6
                swap    d6
                muls    d5,d6
                add.l   d6,d6
                swap    d6
                move.w  d0,d7
                muls    d2,d7
                add.l   d7,d7
                swap    d7
                add.l   d7,d6
                move.w  d6,.g+2
                move.w  d3,d6                   ; H
                muls    d1,d6
                add.l   d6,d6
                swap    d6
                muls    d2,d6
                add.l   d6,d6
                swap    d6
                move.w  d0,d7
                muls    d5,d7
                add.l   d7,d7
                swap    d7
                sub.l   d7,d6
                move.w  d6,.h+2
                move.w  d3,d6                   ; I
                muls    d4,d6
                add.l   d6,d6
                swap    d6
                move.w  d6,.i+2
                moveq   #8,d5                   ;persfac.
                move.w  a5,d6
.nextp:         movem.w (a4),d0-d2              ;get new x
.a:             muls    #$0000,d0               ;x`=ax+by+cz
.b:             muls    #$0000,d1
.c:             muls    #$0000,d2
                add.l   d1,d2
                add.l   d2,d0
                add.l   d0,d0
                swap    d0
                add.w   6(a2),d0                ;addx
                move.w  d0,(a3)
                movem.w (a4),d0-d2              ;get new y
.d:             muls    #$0000,d0               ;y`=dx+ey+fz
.e:             muls    #$0000,d1
.f:             muls    #$0000,d2
                add.l   d1,d2
                add.l   d2,d0
                add.l   d0,d0
                swap    d0
                add.w   8(a2),d0                ;addy
                move.w  d0,2(a3)
                movem.w (a4)+,d0-d2             ;get new z
.g:             muls    #$0000,d0               ;z`=gx+hy+iz
.h:             muls    #$0000,d1
.i:             muls    #$0000,d2
                add.l   d1,d2
                add.l   d2,d0
                add.l   d0,d0
                swap    d0
                move.w  d0,4(a3)
                movem.w (a3),d1-d2              ;perspective
                add.w   10(a2),d0               ;addz
                beq.s   .noPers
                move.w  d1,(a1)+
                move.w  d2,(a1)+
                move.w  d0,(a1)+
                ext.l   d1
                ext.l   d2
                asl.l   d5,d1
                asl.l   d5,d2
                divs    d0,d1
                divs    d0,d2
                bra.s   .persRdy
.noPers:        move.w  d1,(a1)+
                move.w  d2,(a1)+
                move.w  d0,(a1)+
.persRdy:       add.w   #mr_bpr*8/2,d1
                add.w   #mr_height/2,d2
                move.w  d1,(a3)+
                move.w  d2,(a3)+
                move.w  d0,(a3)+
                dbf     d6,.nextp
                rts

mr_drawmirror:  lea     mr_mirrorNewPoints(pc),a2
                lea     mr_mirrorConnections(pc),a3
                lea     mr_mulu,a0
                moveq   #4-1,d5
                move.l  #(mr_bpr*8)<<16+mr_height,mr_minmaxwindow
                clr.l   mr_minmaxwindow+4
                moveq   #1-1,d0                 ;number of planes
                bsr.w   mr_initline
                move.w  #$0001,mr_linecol+2
.connect:       movem.w (a3)+,d2-d3
                movem.w (a2,d2.w),d0-d1
                movem.w (a2,d3.w),d2-d3
                move.l  mr_mirrorback(pc),a5
                bsr.w   mr_windowline
                dbf     d5,.connect

                lea     $dff000,a6
                move.l  mr_mirrorBack(pc),a5
                move.l  mr_mirrorBack+4(pc),a4
                movem.w mr_minmaxwindow(pc),d0-d3
                move.w  d1,d4
                add.w   d4,d4
                move.w  (a0,d4.w),d4
                sub.w   d1,d3                   ;height
                bge.s   .given
                clr.w   6(a4)
                rts
.given:         lsr.w   #4,d0                   ;x1.w
                lsr.w   #4,d2                   ;x2.w
                move.w  d2,d5
                sub.w   d0,d2                   ;delta x.w
                addq.w  #1,d2                   ;width.w
                add.w   d0,d0                   ;x1.b
                add.w   d4,d0                   ;x1+y1 start
                add.w   d5,d5                   ;x2.b
                add.w   d4,d5
                lea     (a5,d5.w),a5            ;x2+y1 start
                ext.l   d5
                move.l  d5,(a4)
                addq.w  #1,d3
                lsl.w   #6,d3
                or.b    d2,d3                   ;bltseize
                add.w   d2,d2                   ;width.b
                move.w  #-mr_bpr,d1
                sub.w   d2,d1                   ;mod.b
                move.l  #$09f00012,d0
                move.w  d1,4(a4)
                move.w  d3,6(a4)
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  d0,$0040(a6)
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.w  d3,$0058(a6)
                rts

mr_rotatecube:  lea     mr_raw2,a1              ;raw-koor
                lea     mr_cubeAlpha(pc),a2     ;angles and adds
                lea     mr_cubeNewPoints(pc),a3 ;new cornerlist
                lea     mr_cubePoints(pc),a4    ;cornerlist
                move.w  #8-1,a5                 ;number of points to rotate
                bra.w   mr_rotate

mr_clearoldcube:lea     $dff000,a6
                movem.l mr_cubeback(pc),a0-a1
                add.l   (a1),a0
                move.l  #$01000002,d0
                moveq   #-1,d1
                move.w  6(a1),d2
                beq.s   .noClear
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
                lea     mr_bpr*mr_height(a0),a0
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
.noClear:       rts

mr_clearoldmirrorcube:
                lea     $dff000,a6
                move.l  mr_cubeback(pc),a0
                lea     mr_bpr*mr_height*2(a0),a0
                move.l  mr_cubeback+8(pc),a1
                add.l   (a1),a0
                move.l  #$01000002,d0
                moveq   #-1,d1
                move.w  6(a1),d2
                beq.s   .noClear
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
                lea     mr_bpr*mr_height(a0),a0
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
.noClear:       rts

mr_clearoldmirrorcube2:
                lea     $dff000,a6
                move.l  mr_cubeback(pc),a0
                lea     mr_bpr*mr_height*2(a0),a0
                move.l  mr_cubeback+8(pc),a1
                add.l   (a1),a0
                move.l  #$01000002,d0
                moveq   #-1,d1
                move.w  6(a1),d2
                beq.s   .noClear
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
                lea     mr_bpr*mr_height(a0),a0
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
.noClear:       
                move.l  mr_cubefront(pc),a0
                lea     mr_bpr*mr_height*2(a0),a0
                move.l  mr_cubeback+8(pc),a1
                add.l   (a1),a0
                move.l  #$01000002,d0
                moveq   #-1,d1
                move.w  6(a1),d2
                beq.s   .noClear2
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
                lea     mr_bpr*mr_height(a0),a0
                btst.b  #6,$0002(a6)
                bne.s   *-6
                movem.l d0-d1,$0040(a6)
                move.l  a0,$0054(a6)
                move.w  4(a1),$0066(a6)
                move.w  d2,$0058(a6)
.noClear2:      rts


mr_mirrorcubebacks:
                lea     mr_cubeStruct(pc),a0    ;vector structure
                lea     mr_mirrorcubeNewPoints(pc),a1 ;coor
                lea     mr_mirrorcubeShowlist(pc),a3 ;drawlist
                lea     mr_mirrorcubeSurfaceNum(pc),a4
                moveq   #6-1,d7                 ;WORK 6 surfaces
                move.b  #$6e,mr_cond            ;BGT
                bra.s   mr_backs

mr_cubebacks:   lea     mr_cubeStruct(pc),a0    ;vector structure
                lea     mr_cubeNewPoints(pc),a1 ;coor
                lea     mr_cubeShowlist(pc),a3  ;drawlist
                lea     mr_cubeSurfaceNum(pc),a4
                moveq   #6-1,d7                 ;WORK 6 surfaces
                move.b  #$6f,mr_cond            ;BLE

mr_backs:       clr.w   (a4)
mr_next:        movem.w (a0),d4-d6
                movem.w (a1,d4.w),d0-d1
                movem.w (a1,d5.w),d2-d3
                movem.w (a1,d6.w),d4-d5
                sub.w   d0,d2
                sub.w   d1,d3
                sub.w   d0,d4
                sub.w   d1,d5
                muls    d3,d4
                muls    d2,d5
                sub.l   d4,d5
mr_cond:        ble.s   mr_notVisible
                move.l  a0,(a3)+                ;save in showtable
                addq.w  #1,(a4)
mr_notVisible:  lea     (mr_cubeSurface-mr_cubeStruct)(a0),a0
                dbf     d7,mr_next
                rts

mr_drawcube:    move.w  mr_cubeSurfaceNum(pc),d7
                beq.w   .no
                subq.w  #1,d7
                lea     mr_mulu,a0
                lea     mr_cubeNewPoints(pc),a2
                lea     mr_cubeStruct(pc),a3
                move.l  mr_cubeBack(pc),a5
                lea     mr_cubeShowlist(pc),a6
                move.l  #(mr_bpr*8)<<16+mr_height,mr_minmaxwindow
                clr.l   mr_minmaxwindow+4
                moveq   #2-1,d0                 ;number of planes
                bsr.w   mr_initline
.next:          move.l  (a6)+,a4                ;surface ptr
                move.w  (mr_cubeSurface-mr_cubeStruct)-2(a4),mr_linecol+2
                moveq   #4-1,d6
.corner:        move.w  (a4)+,d5
                movem.w (a2,d5.w),d0-d1         ;start
                move.w  (a4),d5
                movem.w (a2,d5.w),d2-d3         ;stop
                movem.l d0-d7/a0-a6,-(a7)
                bsr.w   mr_windowline
                movem.l (a7)+,d0-d7/a0-a6
                dbf     d6,.corner
                dbf     d7,.next

                lea     $dff000,a6
                move.l  mr_cubeBack+4(pc),a4
                movem.w mr_minmaxwindow(pc),d0-d3
                move.w  d1,d4
                add.w   d4,d4
                move.w  (a0,d4.w),d4
                sub.w   d1,d3                   ;height
                lsr.w   #4,d0                   ;x1.w
                lsr.w   #4,d2                   ;x2.w
                move.w  d2,d5
                sub.w   d0,d2                   ;delta x.w
                addq.w  #1,d2                   ;width.w
                add.w   d0,d0                   ;x1.b
                add.w   d4,d0                   ;x1+y1 start
                add.w   d5,d5                   ;x2.b
                add.w   d4,d5
                lea     (a5,d5.w),a5            ;x2+y1 start
                ext.l   d5
                move.l  d5,(a4)
                addq.w  #1,d3
                lsl.w   #6,d3
                or.b    d2,d3                   ;bltseize
                add.w   d2,d2                   ;width.b
                move.w  #-mr_bpr,d1
                sub.w   d2,d1                   ;mod.b
                move.l  #$09f00012,d0
                move.w  d1,4(a4)
                move.w  d3,6(a4)
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  d0,$0040(a6)
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.w  d3,$0058(a6)
                lea     mr_bpr*mr_height(a5),a5
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  d0,$0040(a6)
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.w  d3,$0058(a6)
.no:            rts

mr_drawmirrorcube:
                move.w  mr_mirrorcubeSurfaceNum(pc),d7
                beq.w   .no
                subq.w  #1,d7
                lea     mr_mulu,a0
                lea     mr_mirrorcubeNewPoints(pc),a2
                lea     mr_cubeStruct(pc),a3
                move.l  mr_cubeBack(pc),a5
                lea     mr_bpr*mr_height*2(a5),a5
                lea     mr_mirrorcubeShowlist(pc),a6
                move.l  #(mr_bpr*8)<<16+mr_height,mr_minmaxwindow
                clr.l   mr_minmaxwindow+4
                moveq   #2-1,d0                 ;number of planes
                bsr.w   mr_initline
.next:          move.l  (a6)+,a4                ;surface ptr
                move.w  (mr_cubeSurface-mr_cubeStruct)-2(a4),mr_linecol+2
                moveq   #4-1,d6
.corner:        move.w  (a4)+,d5
                movem.w (a2,d5.w),d0-d1         ;start
                move.w  (a4),d5
                movem.w (a2,d5.w),d2-d3         ;stop
                movem.l d0-d7/a0-a6,-(a7)
                bsr.w   mr_windowline
                movem.l (a7)+,d0-d7/a0-a6
                dbf     d6,.corner
                dbf     d7,.next

.normal:
                lea     $dff000,a6
                move.l  mr_cubeBack+8(pc),a4
                movem.w mr_minmaxwindow(pc),d0-d3
                move.w  d1,d4
                add.w   d4,d4
                move.w  (a0,d4.w),d4
                sub.w   d1,d3                   ;height
                bge.s   .given
                clr.w   6(a4)
                rts
.given:         lsr.w   #4,d0                   ;x1.w
                lsr.w   #4,d2                   ;x2.w
                move.w  d2,d5
                sub.w   d0,d2                   ;delta x.w
                addq.w  #1,d2                   ;width.w
                add.w   d0,d0                   ;x1.b
                add.w   d4,d0                   ;x1+y1 start
                add.w   d5,d5                   ;x2.b
                add.w   d4,d5
                lea     (a5,d5.w),a5            ;x2+y1 start
                ext.l   d5
                move.l  d5,(a4)
                addq.w  #1,d3
                lsl.w   #6,d3
                or.b    d2,d3                   ;bltseize
                add.w   d2,d2                   ;width.b
                move.w  #-mr_bpr,d1
                sub.w   d2,d1                   ;mod.b
                move.l  #$09f00012,d0
                move.w  d1,4(a4)
                move.w  d3,6(a4)
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  d0,$0040(a6)
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.w  d3,$0058(a6)
                lea     mr_bpr*mr_height(a5),a5
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  d0,$0040(a6)
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.w  d3,$0058(a6)
.no:            rts


;Spiegeln:      p = Ortsvektor auf den Punkt
;               a = Ortsvektor der Ebene
;               n = Normalenvektor der Ebene
;
;               x = p + 2 · n·(a-p) · n
;                           -------
;                             |n|²
;                       \_________/
;                            |
;                          Skalar !
;
;               Da der Normalenvektor nur gedreht wird ist |n|² konstant.
;               Somit läßt sich die Prozedur noch wesentlich vereinfachen.
;               Man sorgt dafür, daß gilt |n|² durch eine 2er-Potenz auszu-
;               drücken ist. Der Richtungsvektor der Ebene muß wie folgt
;               aufgebaut sein:
;                        ___
;                     / V256 \
;                n = (    0   )   =>  |n|² = 256
;                     \   0  /
;
;               Die 32Bitdivision kann nun durch ein "asr.l #8" ersetzt werden.

mr_mirror:      lea     mr_eye(pc),a0
                lea     mr_mirroreye(pc),a1

;               a0 - Ptr des Koordinatentrippel des Punktes
;               a1 - Ptr des Destinationtrippels
mr_domirroreye: movem.w mr_raw+2*3*4,d0-d2/d3-d5
                sub.w   d0,d3
                sub.w   d1,d4
                sub.w   d2,d5                   ;Normalenvektor
                sub.w   (a0)+,d0
                sub.w   (a0)+,d1
                sub.w   (a0)+,d2                ;a-p
                muls    d3,d0                   ;n·(a-p)
                muls    d4,d1
                muls    d5,d2
                add.l   d2,d1
                add.l   d1,d0                   ;SkalarZähler
                mulsLW  d0,d3,d3                ;SkalarZähler·n
                mulsLW  d0,d4,d4
                mulsLW  d0,d5,d5
                moveq   #16-1,d6                ;2·SkalarZähler/SkalarNenner
                asr.l   d6,d3
                asr.l   d6,d4
                asr.l   d6,d5
                movem.w -3*2(a0),d0-d2          ;Skalar·n + p
                ext.l   d0
                ext.l   d1
                ext.l   d2
                add.l   d0,d3
                add.l   d1,d4
                add.l   d2,d5
                movem.w d3-d5,(a1)

                lea     mr_mirroreye(pc),a0
                lea     mr_trippel(pc),a1
                lea     mr_raw2,a2
                lea     mr_mirrorcubeNewPoints(pc),a3
                moveq   #8-1,d7
.intersect:     movem.w (a2)+,d0-d2
                sub.w   (a0),d0
                sub.w   2(a0),d1
                sub.w   4(a0),d2                ;mirroreye->point
                movem.w d0-d2,(a1)
                movem.w mr_raw+2*3*4,d0-d2/d3-d5
                sub.w   d0,d3
                sub.w   d1,d4
                sub.w   d2,d5                   ;Normalenvektor
                sub.w   (a1),d0
                sub.w   2(a1),d1
                sub.w   4(a1),d2                ;a-p
                muls    d3,d0                   ;n·(a-p)
                muls    d4,d1
                muls    d5,d2
                add.l   d2,d1
                add.l   d1,d0                   ;SkalarZähler
                mulsLW  d0,d3,d3                ;SkalarZähler·n
                mulsLW  d0,d4,d4
                mulsLW  d0,d5,d5
                moveq   #16,d6                  ;SkalarZähler/SkalarNenner
                asr.l   d6,d3
                asr.l   d6,d4
                asr.l   d6,d5
                movem.w (a1),d0-d2              ;Skalar·n + p
                ext.l   d0
                ext.l   d1
                ext.l   d2
                add.l   d0,d3
                add.l   d1,d4
                add.l   d2,d5
                moveq   #8,d6
                asl.l   d6,d3
                asl.l   d6,d4
                divs    d5,d3
                divs    d5,d4
                add.w   #mr_bpr*8/2,d3
                add.w   #mr_height/2,d4
                movem.w d3-d5,(a3)
                addq.l  #3*2,a3
                dbf     d7,.intersect
                rts

;                    /  0  \
;               n = (   0   )     =>  |n| = 256
;                    \ 256 /
;
;               sqrt( n1² + n2² + n3² ) = |n|
;<=>            n1² + n2² + n3² = |n|²
;
;n3 = 256:      n1² + n2² = 0     => bright
;
;n3 = 0:        n1² + n2² = 256²  => dark
mr_mirrorlightsource:
                movem.w mr_raw+2*3*4,d0-d5
                lea     mr_mirrorColConvertTable(pc),a0

mr_lightsource: sub.w   d0,d3
                sub.w   d1,d4
                sub.w   d2,d5                   ;mirror n
                muls    d3,d3                   ;n1²
                muls    d4,d4                   ;n2²
                add.l   d4,d3                   ;n1² + n2²
                moveq   #12,d6
                lsr.l   d6,d3                   ;reduce 0..256² to 0..16
                add.w   d3,d3
                move.w  (a0,d3.w),d3
                lea     mr_colorlist4+2(pc),a0
                lea     mr_colorlist4O+2(pc),a1
                move.w  d3,(a0)
                move.w  d3,(a1)
                move.w  d3,1*16(a1)
                move.w  d3,2*16(a1)
                move.w  d3,3*16(a1)
.noOther:       rts

mircol

mr_mirrorColConvertTable:

                dc.w    $45a
                dc.w    $45a
                dc.w    $359
                dc.w    $349
                dc.w    $348
                dc.w    $348
                dc.w    $247
                dc.w    $237
                dc.w    $236
                dc.w    $236
                dc.w    $135
                dc.w    $125
                dc.w    $124
                dc.w    $124
                dc.w    $023
                dc.w    $013
                dc.w    $012
                dc.w    $012

;               dc.w    $007
;               dc.w    $007
;               dc.w    $007
;               dc.w    $006
;               dc.w    $006
;               dc.w    $006
;               dc.w    $005
;               dc.w    $005
;               dc.w    $005
;               dc.w    $004
;               dc.w    $004
;               dc.w    $004
;               dc.w    $004
;               dc.w    $003
;               dc.w    $003
;               dc.w    $003
;               dc.w    $003

mr_prior:       move.l  #mr_copper,d1
                tst.w   mr_priorstatus
                beq.s   .noFront
                move.l  #mr_copperO,d1
.noFront:       move.w  d1,mr_maincopper+4+2
                swap    d1
                move.w  d1,mr_maincopper+2
                rts
mr_priorstatus: dc.w    0

;..............................................................................

mr_initline:    move.w  #$4e71,mr_skipPl0       ;NOP
                tst.w   d0
                bne.s   .ok
                addq.w  #4,mr_skipPl0           ;RTS
.ok:            btst.b  #6,$dff002
                bne.s   .ok
                move.w  #mr_bpr,$dff060         ;BLTCMOD
                move.w  #mr_bpr,$dff066         ;BLTDMOD
                move.l  #$ffff8000,$dff072      ;BLTBDAT(pattern) + BLTADAT
                move.l  #$ffffffff,$dff044      ;BLTAFWM + BLTALWM
                rts

mr_windowline:  cmp.w   d1,d3
                beq.w   mr_noline               ;same ypos => no line
                bge.s   mr_noexchg2
                exg.l   d0,d2
                exg.l   d1,d3
mr_noexchg2:    lea     mr_minmaxwindow(pc),a1
                cmp.w   (a1),d0                 ;min x1,x2
                bge.s   .noMinX1
                move.w  d0,(a1)
                bge.s   .noMinX1
                clr.w   (a1)
                moveq   #0,d0
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMinX1:       cmp.w   (a1),d2
                bge.s   .noMinX2
                move.w  d2,(a1)
                bge.s   .noMinX2
                clr.w   (a1)
                moveq   #0,d2
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMinX2:       cmp.w   4(a1),d0                ;max x1,x2
                ble.s   .noMaxX1
                move.w  d0,4(a1)
                cmp.w   #mr_bpr*8,d0
                blt.s   .noMaxX1
                move.w  #mr_bpr*8-1,4(a1)
                move.w  #mr_bpr*8-1,d0
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMaxX1:       cmp.w   4(a1),d2
                ble.s   .noMaxX2
                move.w  d2,4(a1)
                cmp.w   #mr_bpr*8,d2
                blt.s   .noMaxX2
                move.w  #mr_bpr*8-1,4(a1)
                move.w  #mr_bpr*8-1,d2
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMaxX2:
                cmp.w   2(a1),d1                ;min y1 (y1<=y3)
                bge.s   .noMinY
                move.w  d1,2(a1)
                bge.s   .noMinY
                clr.w   2(a1)
                moveq   #0,d1
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMinY:        cmp.w   6(a1),d3                ;max y2
                ble.s   .noMaxY
                move.w  d3,6(a1)
                cmp.w   #mr_height,d3
                blt.s   .noMaxY
                move.w  #mr_height-1,6(a1)
                move.w  #mr_height-1,d3
                IF      SHOW=1
                move.w  #$fff,$dff180
                ENDIF
.noMaxY:        
                bra.s   mr_noexchg              ;skip exg

;***            a0      -MuluTable
;***            d0,d1   -P1
;***            d2,d3   -P2
;***            a5      -Plane
;===            d0-d4,d6,d7 + a1,a4,a5 changed by routine
mr_arealine:    cmp.w   d1,d3
                beq.w   mr_noline               ;same ypos => no line
                bge.s   mr_noexchg
                exg.l   d0,d2
                exg.l   d1,d3
mr_noexchg:     sub.w   d0,d2           ;delta x
                sub.w   d1,d3           ;delta y
                move.w  d0,d7
                lsr.w   #3,d7           ;get xoff
                add.w   d1,d1
                add.w   (a0,d1.w),d7    ;add yoff
                lea     (a5,d7.w),a4    ;a4 = plane + off
                moveq   #$f,d6
                and.l   d6,d0           ;just the low bits $0*
                move.b  d0,d7           ;save x low bits
                not.b   d7              ;prepare for prozessorpixel
                ror.l   #4,d0           ;low bits $*0000000
                moveq   #$0004*4,d6
                move.w  d6,d0           ;get octs $*00000010 oct=4 (7)
                tst.w   d2              ;WORK delta x
                bpl.s   mr_noxvz
                neg.w   d2              ;delta x < 0 => oct=5 (4)
                addq.w  #1*4,d0         ;$*0000014
mr_noxvz:       cmp.w   d2,d3           ;delta x < delta y ?
                ble.s   mr_nodeltachg
                exg.l   d2,d3           ;exchg deltas
                sub.w   d6,d0           ;$*0000000/$*0000004
                add.w   d0,d0           ;$*0000000/$*0000008 oct=0/2 (6/5)
mr_nodeltachg:  move.w  d3,d6           ;dy
                sub.w   d2,d6           ;dy-dx
                add.w   d6,d6
                add.w   d6,d6           ;4*(dy-dx)
                add.w   d3,d3           ;2*dy
                move.w  d3,d4
                sub.w   d2,d4           ;2*dy-dx < 0 => SIGNFLAG
                bpl.s   mr_nosign
                or.b    #$10*4,d0
mr_nosign:      or.l    #$0b4a0003,d0   ;BLTCON0 +  BLTCON1
                add.w   d3,d3           ;4*dy
                swap    d3
                move.w  d6,d3           ;4*dy + 4*(dy-dx) =BLTBMOD+BLTAMOD
                addq.w  #1,d2           ;calc bltsize
                lsl.w   #6,d2
                addq.w  #2,d2           ;BLTSIZE
mr_linecol:     move.w  #$0000,d6
                lsr.w   d6
                bcc.s   mr_skipPl0
                lea     $dff052,a1
                btst.b  #6,$02-$52(a1)  ;$0002 wait for blitter
                bne.s   *-6
                bchg.b  d7,(a4)         ;prozessor pixel
                move.l  d3,$62-$52(a1)  ;$0062 BLTBMOD + BLTAMOD
                move.l  d0,$40-$52(a1)  ;$0040 BLTCON0 + BLTCON1
                move.l  a4,$48-$52(a1)  ;$0048 BLTCPT
                move.w  d4,(a1)+        ;$0052 BLTAPTL
                move.l  a4,(a1)+        ;$0054 BLTDPT
                move.w  d2,(a1)         ;$0058 BLTSIZE
mr_skipPl0:     nop                     ;NOP/RTS
                lsr.w   d6
                bcc.s   mr_noline
                lea     mr_bpr*mr_height(a4),a4
                lea     $dff052,a1
                btst.b  #6,$02-$52(a1)  ;$0002 wait for blitter
                bne.s   *-6
                bchg.b  d7,(a4)         ;prozessor pixel
                move.l  d3,$62-$52(a1)  ;$0062 BLTBMOD + BLTAMOD
                move.l  d0,$40-$52(a1)  ;$0040 BLTCON0 + BLTCON1
                move.l  a4,$48-$52(a1)  ;$0048 BLTCPT
                move.w  d4,(a1)+        ;$0052 BLTAPTL
                move.l  a4,(a1)+        ;$0054 BLTDPT
                move.w  d2,(a1)         ;$0058 BLTSIZE
mr_noline:      rts

;------------------------------------------------------------------------------

mr_window1:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2
mr_window2:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2
mr_window3:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2
mr_window4:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2
mr_window5:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2
mr_window6:     dc.l    mr_bpr-2
                dc.w    -2*mr_bpr
                dc.w    1<<6+mr_bpr/2

mr_minmaxwindow:dc.w    0,0,0,0

mr_cubefront:   dc.l    mr_frame1
                dc.l    mr_window1
                dc.l    mr_window5
mr_cubeback:    dc.l    mr_frame2
                dc.l    mr_window2
                dc.l    mr_window6

mr_mirrorfront: dc.l    mr_frame3
                dc.l    mr_window3
mr_mirrorback:  dc.l    mr_frame4
                dc.l    mr_window4

mr_mirrorPoints:dc.w    -700,-700,0             ; -700,-500,0
                dc.w     700,-700,0             ;  700,-500,0
                dc.w     700, 700,0             ;  700, 400,0
                dc.w    -700, 700,0             ; -700, 500,0
                dc.w    0,0,0                   ;       _____    ____
                dc.w    0,0,256+1               ;256 = V65536 = V2^16
mr_mirrorNewPoints:
                dcb.w   6*3,0
mr_mirrorConnections:
                dc.w    0*6,1*6
                dc.w    1*6,2*6
                dc.w    2*6,3*6
                dc.w    3*6,0*6
mr_mirrorAlpha: dc.w    0
mr_mirrorBeta:  dc.w    0
mr_mirrorGamma: dc.w    0
mr_mirrorAddX:  dc.w    0
mr_mirrorAddY:  dc.w    0
mr_mirrorAddZ:  dc.w    -4000

mr_eye:         dc.w    0,0,0
mr_mirroreye:   dc.w    0,0,0
mr_trippel:     dc.w    0,0,0

cubebase        equ     400
mr_cubePoints:  dc.w    -cubebase/2,-cubebase/2, cubebase
                dc.w     cubebase/2,-cubebase/2, cubebase
                dc.w     cubebase/2, cubebase/2, cubebase
                dc.w    -cubebase/2, cubebase/2, cubebase
                dc.w    -cubebase,-cubebase,-cubebase
                dc.w     cubebase,-cubebase,-cubebase
                dc.w     cubebase, cubebase,-cubebase
                dc.w    -cubebase, cubebase,-cubebase
mr_cubeNewPoints:
                dcb.w   3*8,0
mr_cubeShowlist:dcb.l   6,0
mr_cubeStruct:  dc.w    0*6,1*6,2*6,3*6,0*6     ;front
                dc.w    1
mr_cubeSurface: dc.w    7*6,6*6,5*6,4*6,7*6     ;back
                dc.w    1
                dc.w    0*6,4*6,5*6,1*6,0*6     ;top
                dc.w    2
                dc.w    2*6,6*6,7*6,3*6,2*6     ;bottom
                dc.w    2
                dc.w    7*6,4*6,0*6,3*6,7*6     ;left
                dc.w    3
                dc.w    2*6,1*6,5*6,6*6,2*6     ;right
                dc.w    3
mr_cubeAlpha:   dc.w    0
mr_cubeBeta:    dc.w    0
mr_cubeGamma:   dc.w    0
mr_cubeAddX:    dc.w    0
mr_cubeAddY:    dc.w    0
mr_cubeAddZ:    dc.w    -2500
mr_cubeSurfaceNum:
                dc.w    0

mr_mirrorcubeNewPoints:
                dcb.w   3*8,0
mr_mirrorcubeShowlist:
                dcb.l   6,0
mr_mirrorcubeAlpha:
                dc.w    0
mr_mirrorcubeBeta:
                dc.w    0
mr_mirrorcubeGamma:
                dc.w    0
mr_mirrorcubeAddX:
                dc.w    -500
mr_mirrorcubeAddY:
                dc.w    0
mr_mirrorcubeAddZ:
                dc.w    -300
mr_mirrorcubeSurfaceNum:
                dc.w    0

bg_stripesplane: dcb.b  44,0

mr_maincopper:  dc.w    $0084,mr_copperO>>16
                dc.w    $0086,mr_copperO&$ffff
                dc.w    $008a,$0000
                dc.w    $ffff,$fffe

mr_copper:      dc.w    $0096,$0160
                dc.w    $0096,$87c0

                dc.w    $009c,$8004

                dc.w    $008e,$2c81
                dc.w    $0090,$2cc1
                dc.w    $0092,$0038
                dc.w    $0094,$00d0
                dc.w    $0102,$0000
                dc.w    $0104,$0000
                dc.w    $0108,$0000
                dc.w    $010a,$0000
mr_colors:      dc.w    $0180,BACKCOL2
                dc.w    $0182,BACKCOL2                  ;mirror
                dc.w    $0184,BACKCOL2
                dc.w    $0186,BACKCOL2
mr_colorlist1:  dc.w    $0188,NORMALCOL1                ;normal
                dc.w    $018a,NORMALCOL1
                dc.w    $018c,NORMALCOL1
                dc.w    $018e,NORMALCOL1
                dc.w    $0190,NORMALCOL2
                dc.w    $0192,NORMALCOL2
                dc.w    $0194,NORMALCOL2
                dc.w    $0196,NORMALCOL2
                dc.w    $0198,NORMALCOL3
                dc.w    $019a,NORMALCOL3
                dc.w    $019c,NORMALCOL3
                dc.w    $019e,NORMALCOL3
mr_colorlist4:  dc.w    $01a0,MIRRORPLATECOL
mr_colorlist3:  dc.w    $01a2,MIRRORCOL1                ;mirror
                dc.w    $01a4,MIRRORCOL2
                dc.w    $01a6,MIRRORCOL3
mr_colorlist2:  dc.w    $01a8,NORMALCOL1                ;normal
                dc.w    $01aa,NORMALCOL1
                dc.w    $01ac,NORMALCOL1
                dc.w    $01ae,NORMALCOL1
                dc.w    $01b0,NORMALCOL2
                dc.w    $01b2,NORMALCOL2
                dc.w    $01b4,NORMALCOL2
                dc.w    $01b6,NORMALCOL2
                dc.w    $01b8,NORMALCOL3
                dc.w    $01ba,NORMALCOL3
                dc.w    $01bc,NORMALCOL3
                dc.w    $01be,NORMALCOL3

                dc.w    $2c0f,-2
mr_planeptr:    dc.w    $00e0,mr_frame1>>16
                dc.w    $00e2,mr_frame1&$ffff
                dc.w    $00e4,mr_frame1>>16
                dc.w    $00e6,mr_frame1&$ffff
                dc.w    $00e8,mr_frame1>>16
                dc.w    $00ea,mr_frame1&$ffff
                dc.w    $00ec,mr_frame1>>16
                dc.w    $00ee,mr_frame1&$ffff
                dc.w    $00f0,mr_frame1>>16
                dc.w    $00f2,mr_frame1&$ffff
                dc.w    $0100,$5200

                dc.w    $ffff,$fffe


mr_copperO:     dc.w    $0096,$0160
                dc.w    $0096,$83c0

                dc.w    $009c,$8004

                dc.w    $008e,$2c81
                dc.w    $0090,$2cc1
                dc.w    $0092,$0038
                dc.w    $0094,$00d0
mr_planeptrO:   dc.w    $00e0,mr_frame1>>16
                dc.w    $00e2,mr_frame1&$ffff
                dc.w    $00e4,mr_frame1>>16
                dc.w    $00e6,mr_frame1&$ffff
                dc.w    $00e8,mr_frame1>>16
                dc.w    $00ea,mr_frame1&$ffff
                dc.w    $00ec,mr_frame1>>16
                dc.w    $00ee,mr_frame1&$ffff
                dc.w    $00f0,mr_frame1>>16
                dc.w    $00f2,mr_frame1&$ffff
                dc.w    $0100,$5200
                dc.w    $0102,$0000
                dc.w    $0104,$0000
                dc.w    $0108,$0000
                dc.w    $010a,$0000
mr_colorsO:     dc.w    $0180,BACKCOL2
                dc.w    $0182,BACKCOL2                  ;mirror
                dc.w    $0184,BACKCOL2
                dc.w    $0186,BACKCOL2
mr_colorlist1O: dc.w    $0188,NORMALCOL1                ;normal
                dc.w    $018a,NORMALCOL1
                dc.w    $018c,NORMALCOL1
                dc.w    $018e,NORMALCOL1
                dc.w    $0190,NORMALCOL2
                dc.w    $0192,NORMALCOL2
                dc.w    $0194,NORMALCOL2
                dc.w    $0196,NORMALCOL2
                dc.w    $0198,NORMALCOL3
                dc.w    $019a,NORMALCOL3
                dc.w    $019c,NORMALCOL3
                dc.w    $019e,NORMALCOL3
mr_colorlist4O: dc.w    $01a0,MIRRORPLATECOL
mr_colorlist3O: dc.w    $01a2,MIRRORCOL1                ;mirror
                dc.w    $01a4,MIRRORCOL2
                dc.w    $01a6,MIRRORCOL3
mr_colorlist2O: dc.w    $01a8,MIRRORPLATECOL
                dc.w    $01aa,MIRRORCOL1
                dc.w    $01ac,MIRRORCOL2
                dc.w    $01ae,MIRRORCOL3
                dc.w    $01b0,MIRRORPLATECOL
                dc.w    $01b2,MIRRORCOL1
                dc.w    $01b4,MIRRORCOL2
                dc.w    $01b6,MIRRORCOL3
                dc.w    $01b8,MIRRORPLATECOL
                dc.w    $01ba,MIRRORCOL1
                dc.w    $01bc,MIRRORCOL2
                dc.w    $01be,MIRRORCOL3
                dc.w    $ffff,$fffe

mr_sinus:
        dc.w      0,402,804,1206,1608,2009,2410,2811
        dc.w      3212,3612,4011,4410,4808,5205,5602,5998
        dc.w      6393,6786,7179,7571,7962,8351,8739,9126
        dc.w      9512,9896,10278,10659,11039,11417,11793,12167
        dc.w      12539,12910,13279,13645,14010,14372,14732,15090
        dc.w      15446,15800,16151,16499,16846,17189,17530,17869
        dc.w      18204,18537,18868,19195,19519,19841,20159,20475
        dc.w      20787,21096,21403,21705,22005,22301,22594,22884
        dc.w      23170,23452,23731,24007,24279,24547,24811,25072
        dc.w      25329,25582,25832,26077,26319,26556,26790,27019
        dc.w      27245,27466,27683,27896,28105,28310,28510,28706
        dc.w      28898,29085,29268,29447,29621,29791,29956,30117
        dc.w      30273,30424,30571,30714,30852,30985,31113,31237
        dc.w      31356,31470,31580,31685,31785,31880,31971,32057
        dc.w      32137,32213,32285,32351,32412,32469,32521,32567
        dc.w      32609,32646,32678,32705,32728,32745,32757,32765
mr_cosinus:
        dc.w      32767,32765,32757,32745,32728,32705,32678,32646
        dc.w      32609,32567,32521,32469,32412,32351,32285,32213
        dc.w      32137,32057,31971,31880,31785,31685,31580,31470
        dc.w      31356,31237,31113,30985,30852,30714,30571,30424
        dc.w      30273,30117,29956,29791,29621,29447,29268,29085
        dc.w      28898,28706,28510,28310,28105,27896,27683,27466
        dc.w      27245,27019,26790,26556,26319,26077,25832,25582
        dc.w      25329,25072,24811,24547,24279,24007,23731,23452
        dc.w      23170,22884,22594,22301,22005,21705,21402,21096
        dc.w      20787,20475,20159,19841,19519,19195,18867,18537
        dc.w      18204,17869,17530,17189,16846,16499,16151,15800
        dc.w      15446,15090,14732,14372,14010,13645,13278,12910
        dc.w      12539,12167,11793,11417,11039,10659,10278,9896
        dc.w      9512,9126,8739,8351,7962,7571,7179,6786
        dc.w      6392,5998,5602,5205,4808,4410,4011,3611
        dc.w      3212,2811,2410,2009,1608,1206,804,402
        dc.w      0,-402,-804,-1206,-1607,-2009,-2410,-2811
        dc.w      -3211,-3611,-4011,-4409,-4808,-5205,-5602,-5997
        dc.w      -6392,-6786,-7179,-7571,-7961,-8351,-8739,-9126
        dc.w      -9511,-9896,-10278,-10659,-11039,-11416,-11792,-12167
        dc.w      -12539,-12910,-13278,-13645,-14009,-14372,-14732,-15090
        dc.w      -15446,-15799,-16150,-16499,-16845,-17189,-17530,-17868
        dc.w      -18204,-18537,-18867,-19195,-19519,-19840,-20159,-20474
        dc.w      -20787,-21096,-21402,-21705,-22005,-22301,-22594,-22883
        dc.w      -23169,-23452,-23731,-24007,-24278,-24547,-24811,-25072
        dc.w      -25329,-25582,-25831,-26077,-26318,-26556,-26789,-27019
        dc.w      -27244,-27466,-27683,-27896,-28105,-28309,-28510,-28706
        dc.w      -28898,-29085,-29268,-29446,-29621,-29790,-29956,-30116
        dc.w      -30272,-30424,-30571,-30713,-30851,-30984,-31113,-31237
        dc.w      -31356,-31470,-31580,-31684,-31785,-31880,-31970,-32056
        dc.w      -32137,-32213,-32284,-32350,-32412,-32468,-32520,-32567
        dc.w      -32609,-32646,-32678,-32705,-32727,-32744,-32757,-32764
        dc.w      -32767,-32764,-32757,-32744,-32727,-32705,-32678,-32646
        dc.w      -32609,-32567,-32520,-32468,-32412,-32350,-32284,-32213
        dc.w      -32137,-32056,-31970,-31880,-31784,-31684,-31579,-31470
        dc.w      -31355,-31236,-31113,-30984,-30851,-30713,-30571,-30424
        dc.w      -30272,-30116,-29955,-29790,-29620,-29446,-29268,-29085
        dc.w      -28897,-28706,-28509,-28309,-28105,-27896,-27683,-27465
        dc.w      -27244,-27019,-26789,-26556,-26318,-26076,-25831,-25582
        dc.w      -25328,-25071,-24811,-24546,-24278,-24006,-23731,-23452
        dc.w      -23169,-22883,-22593,-22300,-22004,-21705,-21402,-21096
        dc.w      -20786,-20474,-20158,-19840,-19518,-19194,-18867,-18537
        dc.w      -18204,-17868,-17529,-17188,-16845,-16499,-16150,-15799
        dc.w      -15445,-15090,-14732,-14371,-14009,-13644,-13278,-12909
        dc.w      -12538,-12166,-11792,-11416,-11038,-10659,-10278,-9895
        dc.w      -9511,-9125,-8738,-8350,-7961,-7570,-7178,-6785
        dc.w      -6392,-5997,-5601,-5204,-4807,-4409,-4010,-3611
        dc.w      -3211,-2810,-2410,-2008,-1607,-1205,-803,-401
        dc.w      0,402,804,1206,1608,2009,2410,2811
        dc.w      3212,3612,4011,4410,4808,5205,5602,5998
        dc.w      6393,6786,7179,7571,7962,8351,8739,9126
        dc.w      9512,9896,10278,10659,11039,11417,11793,12167
        dc.w      12539,12910,13279,13645,14010,14372,14732,15090
        dc.w      15446,15800,16151,16499,16846,17189,17530,17869
        dc.w      18204,18537,18868,19195,19519,19841,20159,20475
        dc.w      20787,21096,21403,21705,22005,22301,22594,22884
        dc.w      23170,23452,23731,24007,24279,24547,24811,25072
        dc.w      25329,25582,25832,26077,26319,26556,26790,27019
        dc.w      27245,27466,27683,27896,28105,28310,28510,28706
        dc.w      28898,29085,29268,29447,29621,29791,29956,30117
        dc.w      30273,30424,30571,30714,30852,30985,31113,31237
        dc.w      31356,31470,31580,31685,31785,31880,31971,32057
        dc.w      32137,32213,32285,32351,32412,32469,32521,32567
        dc.w      32609,32646,32678,32705,32728,32745,32757,32765
mr_codeende:

;==============================================================================

vt_bpr          equ     44
vt_height       equ     272

vt_init:        lea     vt_mulu,a0
                move.w  #vt_height-1,d7
                moveq   #0,d0
.mulu:          add.w   #vt_bpr,d0
                move.w  d0,(a0)+
                dbf     d7,.mulu
                lea     vt_mask,a0
                moveq   #-1,d0
                move.w  #vt_height*vt_bpr/4-1,d7
.fill:          move.l  d0,(a0)+
                dbf     d7,.fill
                rts

vt_fadenormal:  move.w  d0,d1
                move.w  #BACKCOL2,d6
                move.w  #BACKCOL,d0
                move.w  vt_fadepos(pc),d4
                cmp.w   d1,d4
                bgt.s   .noFade
                bsr.s   SimpleFade
                move.w  d3,vt_backcol+2
                move.w  d3,vt_backcol2+4+2
                move.w  d3,vt_backcol3+4+2
                move.w  d3,vt_backcol4+4+2
                move.w  d3,vt_backcol5+4+2
                addq.w  #1,vt_fadepos
.noFade:        rts
vt_fadepos:     dc.w    0


vt_fadewhite:   move.w  d0,d1
                move.w  #$45a,d6
                move.w  #$fff,d0
                move.w  vt_fadepos2(pc),d4
                cmp.w   d1,d4
                bgt.s   .noFade
                bsr.s   SimpleFade
                move.w  d3,vt_col+2
                move.w  d3,vt_col2+2
                addq.w  #1,vt_fadepos2
.noFade:        rts
vt_fadepos2:    dc.w    0

vt_fadeback:    move.w  d0,d1
                move.w  #$fff,d6
                move.w  #BACKCOL,d0
                move.w  vt_fadepos3(pc),d4
                cmp.w   d1,d4
                bgt.s   .noFade
                bsr.s   SimpleFade
                move.w  d3,vt_col+2
                move.w  d3,vt_col2+2
                addq.w  #1,vt_fadepos3
.noFade:        rts
vt_fadepos3:    dc.w    0


; d0=ToColor, d1=MaxPos, d4=Pos, d6=FromColor => d3=Color

SimpleFade:     move.w  d1,.div1+2
                move.w  d1,.div2+2
                move.w  d1,.div3+2
                moveq   #$f,d5
                move.w  d0,-(a7)
                move.w  d6,d1
                ror.w   #8,d0
                ror.w   #8,d1
                and.w   d5,d0
                and.w   d5,d1
                sub.w   d1,d0
                muls    d4,d0
.div1:          divs    #$0001,d0
                add.w   d1,d0
                rol.w   #8,d0
                move.w  d0,d3
                move.w  (a7),d0
                move.w  d6,d1
                ror.w   #4,d0
                ror.w   #4,d1
                and.w   d5,d0
                and.w   d5,d1
                sub.w   d1,d0
                muls    d4,d0
.div2:          divs    #$0001,d0
                add.w   d1,d0
                rol.w   #4,d0
                or.w    d0,d3
                move.w  (a7)+,d0
                move.w  d6,d1
                and.w   d5,d0
                and.w   d5,d1
                sub.w   d1,d0
                muls    d4,d0
.div3:          divs    #$0001,d0
                add.w   d1,d0
                or.w    d0,d3
                rts

vt_clear:       lea     vt_frame1,a0
                move.w  #vt_bpr*vt_height*2/4-1,d7
.clr:           clr.l   (a0)+
                dbf     d7,.clr
                rts

                IF      RECORD=1
PSET            macro
                move.b  d0,d2
                lsr.w   #3,d0
                not.b   d2
                mulu    #vt_bpr,d1
                add.w   d0,d1
                bset.b  d2,(a0,d1.w)
                endm

PCLR            macro
                move.b  d0,d2
                lsr.w   #3,d0
                not.b   d2
                mulu    #vt_bpr,d1
                add.w   d0,d1
                bclr.b  d2,(a0,d1.w)
                endm


vt_sign:        btst.b  #7,$bfe001
                bne.s   .noSet
                lea     vt_mask,a0
                movem.w vt_koor(pc),d0/d1
                PSET    d0,d1
.noSet:         lea     vt_frame1,a0
                movem.w vt_koor(pc),d0/d1
                PCLR    d0,d1
                lea     vt_frame2,a0
                movem.w vt_koor(pc),d0/d1
                PCLR    d0,d1
                move.w  $dff00c,d0
                move.w  d0,d1
                add.w   d1,d1
                btst    #1,d0
                bne.s   .noRight
                subq.w  #1,vt_koor
.noRight:       btst    #9,d0
                bne.s   .noLeft
                addq.w  #1,vt_koor
.noLeft:        eor.w   d1,d0
                btst    #1,d0
                bne.s   .noBack
                subq.w  #1,vt_koor+2
.noBack:        btst    #9,d0
                bne.s   .noForw
                addq.w  #1,vt_koor+2
.noForw:        lea     vt_frame1,a0
                movem.w vt_koor(pc),d0/d1
                PSET    d0,d1
                lea     vt_frame2,a0
                movem.w vt_koor(pc),d0/d1
                PSET    d0,d1
                rts
vt_koor:        dc.w    0,0
                ENDIF

vt_nomask:      move.l  $dff004,d0              ; geht das ???????????
                and.l   #$0003ff00,d0
                cmp.l   #$00011e00,d0
                blt.s   vt_nomask
                

                move.l  vt_maskclear(pc),a0
                move.w  #vt_height/2-1,d7
.line1:         clr.b   (a0)
                lea     vt_bpr(a0),a0
                dbf     d7,.line1
                move.l  vt_maskclear+4(pc),a0
                move.w  #vt_height/2-1,d7
.line2:         clr.b   (a0)
                lea     vt_bpr(a0),a0
                dbf     d7,.line2
                addq.l  #1,vt_maskclear
                subq.l  #1,vt_maskclear+4
                rts
vt_maskclear:   dc.l    vt_mask
                dc.l    vt_mask+vt_bpr*vt_height/2+vt_bpr-1

vt_transit:     bsr.s   vt_clearold
                bsr.s   vt_calctransform
                bsr.w   vt_drawtransform
                bsr.s   vt_setbuffers
                IF      SHOW=1
                move.w  #$f00,$dff180
                ENDIF
                rts

vt_clearold:    lea     $dff000,a6
.wtblt:         btst.b  #6,$0002(a6)
                bne.s   .wtblt
                move.w  #$0000,$0066(a6)
                move.l  #$01000000,$0040(a6)
                move.l  vt_back(pc),$0054(a6)
                move.w  #(vt_height<<6)+vt_bpr/2,$0058(a6)
                rts

vt_setbuffers:  movem.l vt_front(pc),d0-d1
                exg.l   d0,d1
                movem.l d0-d1,vt_front
                lea     vt_plnptr+2(pc),a0
                move.w  d0,4(a0)
                swap    d0
                move.w  d0,(a0)
                rts

vt_calctransform:
                tst.w   vt_wait
                bne.w   .wait
                move.l  vt_trans(pc),a0
                cmp.l   #-1,a0
                beq.s   .noTrans
                move.w  (a0),d5
                cmp.w   vt_transpos(pc),d5
                beq.s   .next
                movem.l 2(a0),a1-a2
                move.l  (a1),vt_drawobject
                move.w  (a1),d7
                move.l  8(a1),vt_drawobject+8
                move.l  4(a1),a1                        ;from
                move.l  4(a2),a2                        ;to
                move.l  vt_drawobject+4(pc),a3          ;dest
                addq.w  #1,vt_transpos
                move.w  vt_transpos(pc),d6
.corners:       movem.w (a1)+,d0-d1                     ;x1,y1
                movem.w (a2)+,d2-d3                     ;x2,y2
                sub.w   d0,d2                           ;delta x = x2-x1
                sub.w   d1,d3                           ;delta y = y2-y1
                muls    d6,d2                           ;dx * p
                muls    d6,d3                           ;dy * p
                divs    d5,d2                           ;dx * p / pmax
                divs    d5,d3                           ;dy * p / pmax
                add.w   d0,d2                           ;dx * p / pmax + x1
                add.w   d1,d3                           ;dy * p / pmax + y1
                movem.w d2-d3,(a3)
                addq.l  #2*2,a3
                dbf     d7,.corners
.noTrans:       rts
.next:          move.w  10(a0),vt_wait
                move.l  12(a0),vt_trans
                clr.w   vt_transpos
                rts
.wait:          subq.w  #1,vt_wait
                rts
vt_wait:        dc.w    0
vt_transpos:    dc.w    0


vt_drawtransform:
                lea     vt_drawobject(pc),a0
                move.w  2(a0),d7
                movem.l 4(a0),a2-a3
                move.l  vt_back(pc),a5
                lea     vt_mulu,a0
                bsr.s   vt_initline
.connect:       movem.w (a3)+,d1-d2
                movem.w (a2,d1.w),d0-d1
                movem.w (a2,d2.w),d2-d3
                move.w  d7,-(a7)
                bsr.s   vt_arealine
                move.w  (a7)+,d7
                dbf     d7,.connect

                lea     $dff000,a6
                moveq   #0,d0
.wtblt:         btst.b  #6,$0002(a6)
                bne.s   .wtblt
                lea     vt_bpr*vt_height-2(a5),a5
                move.l  a5,$0050(a6)
                move.l  a5,$0054(a6)
                move.l  d0,$0064(a6)
                move.l  #$09f00012,$0040(a6)
                move.w  #(vt_height<<6)+vt_bpr/2,$0058(a6)
                rts

vt_initline:    btst.b  #6,$dff002
                bne.s   vt_initline
                move.w  #vt_bpr,$dff060         ;BLTCMOD
                move.w  #vt_bpr,$dff066         ;BLTDMOD
                move.l  #$ffff8000,$dff072      ;BLTBDAT(pattern) + BLTADAT
                move.l  #$ffffffff,$dff044      ;BLTAFWM + BLTALWM
                rts

;***            a0      -MuluTable
;***            d0,d1   -P1
;***            d2,d3   -P2
;***            a5      -Plane
;===            d0-d4,d6,d7 + a1,a4,a5 changed by routine
vt_arealine:    cmp.w   d1,d3
                beq.w   vt_noline               ;same ypos => no line
                bge.s   vt_noexchg
                exg.l   d0,d2
                exg.l   d1,d3
vt_noexchg:     sub.w   d0,d2           ;delta x
                sub.w   d1,d3           ;delta y
                move.w  d0,d7
                lsr.w   #3,d7           ;get xoff
                add.w   d1,d1
                add.w   (a0,d1.w),d7    ;add yoff
                lea     (a5,d7.w),a4    ;a4 = plane + off
                moveq   #$f,d6
                and.l   d6,d0           ;just the low bits $0*
                move.b  d0,d7           ;save x low bits
                not.b   d7              ;prepare for prozessorpixel
                ror.l   #4,d0           ;low bits $*0000000
                moveq   #$0004*4,d6
                move.w  d6,d0           ;get octs $*00000010 oct=4 (7)
                tst.w   d2              ;WORK delta x
                bpl.s   vt_noxvz
                neg.w   d2              ;delta x < 0 => oct=5 (4)
                addq.w  #1*4,d0         ;$*0000014
vt_noxvz:       cmp.w   d2,d3           ;delta x < delta y ?
                ble.s   vt_nodeltachg
                exg.l   d2,d3           ;exchg deltas
                sub.w   d6,d0           ;$*0000000/$*0000004
                add.w   d0,d0           ;$*0000000/$*0000008 oct=0/2 (6/5)
vt_nodeltachg:  move.w  d3,d6           ;dy
                sub.w   d2,d6           ;dy-dx
                add.w   d6,d6
                add.w   d6,d6           ;4*(dy-dx)
                add.w   d3,d3           ;2*dy
                move.w  d3,d4
                sub.w   d2,d4           ;2*dy-dx < 0 => SIGNFLAG
                bpl.s   vt_nosign
                or.b    #$10*4,d0
vt_nosign:      or.l    #$0b4a0003,d0   ;BLTCON0 +  BLTCON1
                add.w   d3,d3           ;4*dy
                swap    d3
                move.w  d6,d3           ;4*dy + 4*(dy-dx) =BLTBMOD+BLTAMOD
                addq.w  #1,d2           ;calc bltsize
                lsl.w   #6,d2
                addq.w  #2,d2           ;BLTSIZE
                lea     $dff052,a1
                btst.b  #6,$02-$52(a1)  ;$0002 wait for blitter
                bne.s   *-6
                bchg.b  d7,(a4)         ;prozessor pixel
                move.l  d3,$62-$52(a1)  ;$0062 BLTBMOD + BLTAMOD
                move.l  d0,$40-$52(a1)  ;$0040 BLTCON0 + BLTCON1
                move.l  a4,$48-$52(a1)  ;$0048 BLTCPT
                move.w  d4,(a1)+        ;$0052 BLTAPTL
                move.l  a4,(a1)+        ;$0054 BLTDPT
                move.w  d2,(a1)         ;$0058 BLTSIZE
vt_noline:      rts

vt_front:       dc.l    vt_frame1
vt_back:        dc.l    vt_frame2
vt_trans:       dc.l    vt_trans1

vt_drawobject:  dc.w    0
                dc.w    0
                dc.l    vt_freecorners  ;cornerlist
                dc.l    0               ;connectionlist
vt_freecorners: dcb.l   50,0

;******************************************************************************
vt_trans1:      dc.w    2*25            ;transform time
                dc.l    vt_object2      ;from
                dc.l    vt_object1      ;to
                dc.w    3*25            ;wait time
                dc.l    vt_trans2       ;next

vt_trans2:      dc.w    2*25            ;transform time
                dc.l    vt_object1      ;from
                dc.l    vt_object3      ;to
                dc.w    3*25            ;wait time
                dc.l    -1              ;next


vt_object1:     dc.w    41-1            ;corners-1
                dc.w    41-1            ;connects-1
                dc.l    vt_corners1     ;cornerlist
                dc.l    vt_connects1    ;connectionlist

vt_object2:     dc.w    41-1            ;corners-1
                dc.w    41-1            ;connects-1
                dc.l    vt_corners2     ;cornerlist
                dc.l    vt_connects1    ;connectionlist

vt_object3:     dc.w    41-1            ;corners-1
                dc.w    41-1            ;connects-1
                dc.l    vt_corners3     ;cornerlist
                dc.l    vt_connects1    ;connectionlist

vt_corners1:    dc.w     73,181         ;T
                dc.w    101,173
                dc.w    101,83
                dc.w    124,80
                dc.w    123,47
                dc.w     38,54
                dc.w     39,90
                dc.w     73,86
                dc.w    132,165         ;E
                dc.w    202,153
                dc.w    201,129
                dc.w    159,134
                dc.w    158,114
                dc.w    184,112
                dc.w    184,90
                dc.w    157,91
                dc.w    156,70
                dc.w    200,71
                dc.w    199,46
                dc.w    127,46
                dc.w    205,152         ;K
                dc.w    231,149
                dc.w    229,113
                dc.w    250,147
                dc.w    275,147
                dc.w    247,100
                dc.w    277,50
                dc.w    253,48
                dc.w    229,86
                dc.w    228,46
                dc.w    202,46
                dc.w     62,225         ;-
                dc.w     90,209
                dc.w    160,187
                dc.w    215,175
                dc.w    260,170
                dc.w    261,192
                dc.w    210,194
                dc.w    169,200
                dc.w    123,208
                dc.w     84,219

vt_connects1:   dc.w    0*4,1*4         ;T
                dc.w    1*4,2*4
                dc.w    2*4,3*4
                dc.w    3*4,4*4
                dc.w    4*4,5*4
                dc.w    5*4,6*4
                dc.w    6*4,7*4
                dc.w    7*4,0*4
                dc.w    8*4,9*4         ;E
                dc.w    9*4,10*4
                dc.w    10*4,11*4
                dc.w    11*4,12*4
                dc.w    12*4,13*4
                dc.w    13*4,14*4
                dc.w    14*4,15*4
                dc.w    15*4,16*4
                dc.w    16*4,17*4
                dc.w    17*4,18*4
                dc.w    18*4,19*4
                dc.w    19*4,8*4
                dc.w    20*4,21*4       ;K
                dc.w    21*4,22*4
                dc.w    22*4,23*4
                dc.w    23*4,24*4
                dc.w    24*4,25*4
                dc.w    25*4,26*4
                dc.w    26*4,27*4
                dc.w    27*4,28*4
                dc.w    28*4,29*4
                dc.w    29*4,30*4
                dc.w    30*4,20*4
                dc.w    31*4,32*4       ;-
                dc.w    32*4,33*4
                dc.w    33*4,34*4
                dc.w    34*4,35*4
                dc.w    35*4,36*4
                dc.w    36*4,37*4
                dc.w    37*4,38*4
                dc.w    38*4,39*4
                dc.w    39*4,40*4
                dc.w    40*4,31*4

vt_korrx        equ     16
vt_korry        equ     6
vt_corners2:
                dc.w     96+vt_korrx,129+vt_korry
                dc.w    129+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,106+vt_korry
                dc.w    159+vt_korrx,84+vt_korry-18
                dc.w    129+vt_korrx,84+vt_korry-18
                dc.w     96+vt_korrx,84+vt_korry-18
                dc.w     96+vt_korrx,106+vt_korry

                dc.w    159+vt_korrx,94+vt_korry
                dc.w    159+vt_korrx,114+vt_korry
                dc.w    159+vt_korrx,129+vt_korry
                dc.w    178+vt_korrx,129+vt_korry
                dc.w    201+vt_korrx,129+vt_korry
                dc.w    222+vt_korrx,129+vt_korry
                dc.w    222+vt_korrx,116+vt_korry
                dc.w    222+vt_korrx,100+vt_korry
                dc.w    222+vt_korrx,84+vt_korry-18
                dc.w    205+vt_korrx,84+vt_korry-18
                dc.w    178+vt_korrx,84+vt_korry-18
                dc.w    159+vt_korrx,84+vt_korry-18

                dc.w    159+vt_korrx,173+vt_korry+18
                dc.w    192+vt_korrx,173+vt_korry+18
                dc.w    222+vt_korrx,173+vt_korry+18
                dc.w    222+vt_korrx,162+vt_korry
                dc.w    222+vt_korrx,142+vt_korry
                dc.w    222+vt_korrx,129+vt_korry
                dc.w    201+vt_korrx,129+vt_korry
                dc.w    178+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,144+vt_korry
                dc.w    159+vt_korrx,160+vt_korry

                dc.w     96+vt_korrx,173+vt_korry+18
                dc.w     96+vt_korrx,159+vt_korry
                dc.w     96+vt_korrx,141+vt_korry
                dc.w     96+vt_korrx,129+vt_korry
                dc.w    129+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,129+vt_korry
                dc.w    159+vt_korrx,144+vt_korry
                dc.w    159+vt_korrx,160+vt_korry
                dc.w    159+vt_korrx,173+vt_korry+18
                dc.w    129+vt_korrx,173+vt_korry+18


vt_corners3:    dc.w    68,102
                dc.w    104,102
                dc.w    138,102
                dc.w    157,121
                dc.w    173,137
                dc.w    173,172
                dc.w    173,207
                dc.w    123,157

                dc.w    141,98
                dc.w    154,85
                dc.w    164,75
                dc.w    176,63
                dc.w    189,76
                dc.w    200,87
                dc.w    211,98
                dc.w    198,111
                dc.w    187,122
                dc.w    176,133
                dc.w    164,121
                dc.w    153,110

                dc.w    216,170
                dc.w    216,152
                dc.w    216,133
                dc.w    216,115
                dc.w    216,100
                dc.w    236,100
                dc.w    262,100
                dc.w    286,100
                dc.w    269,117
                dc.w    250,136
                dc.w    232,154

                dc.w    177,207
                dc.w    177,182
                dc.w    177,157
                dc.w    177,137
                dc.w    193,121
                dc.w    212,102
                dc.w    212,122
                dc.w    212,145
                dc.w    212,172
                dc.w    195,189



vt_copper:      dc.w    $009c,$8004

                dc.w    $008e,$2471
                dc.w    $0090,$33d1
                dc.w    $0092,$0030
                dc.w    $0094,$00d8

                dc.w    $0102,$0000
;               dc.w    $0104,$0000
                dc.w    $0108,$0000
                dc.w    $010a,$0000

vt_backcol:     dc.w    $0180,$312      ;BACKCOL
vt_col:         dc.w    $0182,$045a             ; 007
                dc.w    $0184,BACKCOL2
vt_col2:        dc.w    $0186,$045a             ; 007

        dc.w    $96,$83e0                               ; Sprite-DMA ein
        dc.w    $120,SPRITE_EMBLEM_1>>16,$122,SPRITE_EMBLEM_1&$ffff
        dc.w    $124,SPRITE_EMBLEM_2>>16,$126,SPRITE_EMBLEM_2&$ffff
        dc.w    $128,SPRITE_EMBLEM_3>>16,$12a,SPRITE_EMBLEM_3&$ffff
        dc.w    $12c,SPRITE_EMBLEM_4>>16,$12e,SPRITE_EMBLEM_4&$ffff
        dc.w    $130,SPRITE_EMBLEM_5>>16,$132,SPRITE_EMBLEM_5&$ffff
        dc.w    $134,SPRITE_EMBLEM_6>>16,$136,SPRITE_EMBLEM_6&$ffff
        dc.w    $138,FREESPRITE>>16,$13a,FREESPRITE&$ffff
        dc.w    $13c,FREESPRITE>>16,$13e,FREESPRITE&$ffff
        dc.w    $104,0                                  ; Sprites hinten
        dc.w    $1a2,$534-f,$1a4,$523-f,$1a6,$312-f           ; Spritefarben
        dc.w    $1aa,$534-f,$1ac,$523-f,$1ae,$312-f
        dc.w    $1b2,$534-f,$1b4,$523-f,$1b6,$312-f
        dc.w    $1ba,$534-f,$1bc,$523-f,$1be,$312-f

                dc.w    $240f,-2
vt_plnptr:      dc.w    $00e0,vt_frame1>>16
                dc.w    $00e2,vt_frame1&$ffff
                dc.w    $00e4,vt_mask>>16
                dc.w    $00e6,vt_mask&$ffff
                dc.w    $0100,$2200

                dc.w    $8759,-2,$180,$312-f
vt_backcol2:    dc.w    $8779,-2,$180,$423-f
                dc.w    $87a3,-2,$180,$312-f
vt_backcol3:    dc.w    $87c5,-2,$180,$423-f
                dc.w    $8859,-2,$180,$312-f
vt_backcol4:    dc.w    $8879,-2,$180,$423-f
                dc.w    $88a3,-2,$180,$312-f
vt_backcol5:    dc.w    $88c5,-2,$180,$423-f

                dc.w    $ffff,$fffe
                
vt_codeende:

ende

len             =ende-CODE_AREA

;==============================================================================
vt_mulu         equ     DATA_AREA
vt_frame1       equ     vt_mulu+2*vt_height
vt_frame2       equ     vt_frame1+vt_bpr*vt_height
vt_mask         equ     vt_frame2+vt_bpr*vt_height
vt_dataende     equ     vt_mask+vt_bpr*vt_height

mr_mulu         equ     vt_dataende
mr_frame1       equ     mr_mulu+2*mr_height
mr_frame2       equ     mr_frame1+mr_height*mr_bpr*4
mr_frame3       equ     mr_frame2+mr_height*mr_bpr*4
mr_frame4       equ     mr_frame3+mr_height*mr_bpr
mr_raw          equ     mr_frame4+mr_height*mr_bpr
mr_raw2         equ     mr_raw+10*3*2
mr_dataende     equ     mr_raw2+10*3*2

;==============================================================================
                IF2

                IF      DATA_AREA<=mr_codeende
                PRINTT  "ERROR: Daten und Code überschneiden sich !"
                FAIL
                ENDIF

                IF      (mr_dataende&$fffff)>$80000
                PRINTT  "ERROR: Daten verlassen Speichergrenze !"
                FAIL
                END

                ENDIF

                END

>extern         "Sprite1.raw",SPRITE_EMBLEM_1
>extern         "Sprite2.raw",SPRITE_EMBLEM_2
>extern         "Sprite3.raw",SPRITE_EMBLEM_3
>extern         "Sprite4.raw",SPRITE_EMBLEM_4
>extern         "Sprite5.raw",SPRITE_EMBLEM_5
>extern         "Sprite6.raw",SPRITE_EMBLEM_6
