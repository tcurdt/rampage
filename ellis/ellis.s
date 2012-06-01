;******************************************************************************
;****           Six-Rotating/Wobbeling-Filled-Overscan-Ellipses
;****           on Trackmobackground
;****           (C) Copyright 1993 by Torsten Curdt/TEK
;****           done in Juli
;****           ca. 25 sek
;******************************************************************************

;               duration:       1540 frames / 30,8 s
;               script:         $34114
;               size unpacked:  50888
;               size packed:    4262
;               prepare time:   800 frames / medium cpu usage


f               =$111

TEST            equ     1       ;-> SCRIPT: fc_ellipart !!!
SHOW            equ     0
MUSICTEST       equ     0
SHIFT           equ     $100000

STACK           =$400+SHIFT
MUSIC_AREA      =$400+SHIFT
SPRITE_AREA     =$32e00+SHIFT
CODE_AREA       =$34000+SHIFT
DATA_AREA       =$60000+SHIFT ;ACHTUNG ! real start ist $50000

JUMPPTR_PREPARE =$32df2+SHIFT

SPRITE_EMBLEM_1 =SPRITE_AREA
SPRITE_EMBLEM_2 =SPRITE_EMBLEM_1+808
SPRITE_EMBLEM_3 =SPRITE_EMBLEM_2+788
SPRITE_EMBLEM_4 =SPRITE_EMBLEM_3+880
SPRITE_EMBLEM_5 =SPRITE_EMBLEM_4+816
SPRITE_EMBLEM_6 =SPRITE_EMBLEM_5+544

;##############################################################################

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

fc_bpr          equ     352/8
fc_height       equ     272

fc_maxrad       equ     116
fc_disrad       equ     1024
fc_startz       equ     -31000
fc_destz        equ     3000 ;*(-1)
fc_step         equ     100

fc_trix         equ     (fc_bpr*8+fc_maxrad-64+15)>>4*16
fc_triwidth     equ     (2*((fc_maxrad+15)>>4)+1)*2
fc_framebpr     equ     (fc_trix+fc_triwidth*8/2+15)>>4*2
fc_framelen     equ     fc_framebpr*(fc_maxrad+1)
fc_korr         equ     4

fc_normbright   equ     1200

                org     CODE_AREA
                load    CODE_AREA
                jumpptr CODE_AREA

                IF      TEST=1 THEN

                bsr     setup
                move.l  $64.w,mongo

su_Main:        lea     ma_skript,a0
                bsr     manager_init

                lea     $dff000,a6
                move.l  #su_VBI,$64.w
                move.l  #su_Copper1,$0080(a6)
                move.l  #su_Copper2,$0084(a6)
                move.w  d0,$0088(a6)
                move.w  #$83c0,$0096(a6)
                move.w  #$c004,$009a(a6)

su_loop:        bsr.w   ma_queue
                tst.w   endflag
                bne.s   raus
                btst    #6,$bfe001
                bne.s   su_loop

raus            move.w  #$4000,$dff09a
                move.l  #su_copper3,d0
                bsr.w   fc_newcopper

                ;wait at least 1 frame !
                moveq   #-1,d7
.wait:          move.w  d7,$dff180
                nop
                nop
                nop
                nop
                dbf     d7,.wait

                ;now reset your copperlist & dmas

                move.l  mongo,$64.w

                bra     closedown

endflag         dc.w    0

su_VBI:         movem.l d0-d7/a0-a6,-(a7)
                bsr.w   manager
                movem.l (a7)+,d0-d7/a0-a6
                move.w  #$0004,$dff09c
                rte

su_Copper1:     dc.w    $009c,$8004
                dc.w    $ffff,$fffe
su_Copper2:     dc.w    $0088,$0000
                dc.w    $ffff,$fffe

SETUP_CACHEOFF  =1
SETUP_VBR0      =1
                include "setup.i"

mongo           dc.l    0

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
man_nextframe   rs.l    1               ; n‰chstes Frame
man_label       rs.l    1               ; aktuelles Label
man_labelstack  rs.l    ma_maxgosub     ; Stack f¸r Unterprogramm-Labels
man_framestack  rs.l    ma_maxgosub     ; Stack f¸r Unterprogramm-Frames
man_stackptr    rs.w    1               ; Stackpointer f¸r Unterprogramm-Stack
man_calltable   rs.l    ma_maxcalls     ; Call-Tabelle
man_pcallflags  rs.w    ma_maxcalls     ; 0=call, 1=pcall
man_anzcall     rs.l    ma_maxcalls     ; Call-Counter-Tabelle
man_pcallpara   rs.l    ma_maxcalls*4   ; Parameterlisten f¸r PCALL
man_queuetable  rs.l    ma_maxqueue     ; Queue-Calltable
man_pqueuepara  rs.l    ma_maxqueue*4   ; Parameterlisten f¸r Queue
man_queuepos    rs.w    1               ; Position in QueueTable
man_SIZEOF      rs.b    0

managerstruc    blk.b   man_SIZEOF,0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

manager_init    ;       a0      Pointer auf Skript

                lea     managerstruc+man_SIZEOF(pc),a1
                move.w  #man_SIZEOF/2-1,d0
ma_clear        clr.w   -(a1)
                dbf     d0,ma_clear
                move.l  (a0),man_nextframe(a1)
                addq.l  #4,a0
                move.l  a0,man_label(a1)
                rts

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
                addq.w  #4,man_stackptr(a0)     ; Stack erhˆhen
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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

ma_skript:      dc.l    00000,switch,su_copper3
                dc.l    00000,gosub,fc_ellipart
                dc.l    -1,goto,ma_skript
                        ENDIF

su_Copper3:     dc.w    $009c,$8004
                dc.w    $008e,$2471
                dc.w    $0090,$33d1
                dc.w    $0092,$0030
                dc.w    $0094,$00d8
                dc.w    $0100,$1200
                dc.w    $0102,$0000
                dc.w    $0180,$0423-f
                dc.w    $0108,-fc_bpr
                dc.w    $00e0,su_free>>16
                dc.w    $00e2,su_free&$ffff

        dc.w    $96,$8020                               ; Sprite-DMA ein
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

        dc.w    $8759,-2,$180,$312-f
        dc.w    $8779,-2,$180,$423-f
        dc.w    $87a3,-2,$180,$312-f
        dc.w    $87c5,-2,$180,$423-f
        dc.w    $8859,-2,$180,$312-f
        dc.w    $8879,-2,$180,$423-f
        dc.w    $88a3,-2,$180,$312-f
        dc.w    $88c5,-2,$180,$423-f

                dc.w    $ffff,$fffe

su_free:        dcb.b   fc_bpr,0

INITWAIT        equ     130


                IF      MUSICTEST=1 THEN
music:          IF      SHOW=1 THEN
                move.w  #$f00,$dff180
                ENDIF
                moveq   #10-1,d7
.loop:          dcb.w   100,$4e71
                dbf     d7,.loop
                IF      SHOW=1 THEN
                move.w  #$000,$dff180
                ENDIF
                rts
                ENDIF

fc_ellipart:    dc.l    00000,queue,fc_init

                IF      MUSICTEST=1 THEN
                dc.l    00000,call,music,-1,0
                ENDIF
                dc.l    00001+INITWAIT,call,fc_buildcopper,-1,1
                dc.l    00001+INITWAIT,call,fc_mover,-1,2
                dc.l    00001+INITWAIT,move,fc_direction,$4e71
                dc.l    00001+INITWAIT,call,fc_zoomIn,(-fc_startz-fc_destz)/fc_step,3
                dc.l    00001+INITWAIT,queue,fc_initwobbel
                dc.l    00001+INITWAIT,switch,fc_maincopper

                dc.l    00001+INITWAIT,call,fc_resetelli,1,5
                dc.l    00300+INITWAIT,call,fc_elli,-1,4
                dc.l    00500+INITWAIT,call,fc_waitstill,-1,4
                dc.l    00600+INITWAIT,kill,4
                dc.l    00600+INITWAIT,queue,fc_initrotate
                dc.l    00600+INITWAIT,call,fc_resetelli,1,5

                if      TEST=0
                dc.l    600+initwait,queue,jumpptr_prepare
                endif

                dc.l    00800+INITWAIT,call,fc_elli,-1,4
                dc.l    01100+INITWAIT,move,fc_direction,$4441

                dc.l    01100+INITWAIT,call,fc_zoomOut,(-fc_startz-fc_destz)/fc_step,3
                dc.l    01200+INITWAIT,call,fc_waitstill,-1,4

                dc.l    01400+INITWAIT,kill,1
                dc.l    01400+INITWAIT,kill,2
                dc.l    01400+INITWAIT,kill,3
                dc.l    01400+INITWAIT,kill,4
                dc.l    01400+INITWAIT,kill,5
                dc.l    01400+INITWAIT,pcall,fc_newcopper,1,1,intercopper,0,0,0

                if      TEST=1
                dc.l    1405+INITWAIT,move,endflag,1
                endif

                dc.l    01409+INITWAIT,return



fc_newcopper:   move.w  d0,fc_maincopper+2+4
                swap    d0
                move.w  d0,fc_maincopper+2
                rts

intercopper     dc.w    $009c,$8004
                dc.w    $008e,$2471
                dc.w    $0090,$33d1
                dc.w    $0092,$0030
                dc.w    $0094,$00d8
                dc.w    $0100,$1200
                dc.w    $0102,$0000
                dc.w    $0180,$0423-f
                dc.w    $0108,-fc_bpr
                dc.w    $00e0,su_free>>16
                dc.w    $00e2,su_free&$ffff
                dc.w    $96,$8020                               ; Sprite-DMA ein
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
                dc.w    $8759,-2,$180,$312-f
                dc.w    $8779,-2,$180,$423-f
                dc.w    $87a3,-2,$180,$312-f
                dc.w    $87c5,-2,$180,$423-f
                dc.w    $8859,-2,$180,$312-f
                dc.w    $8879,-2,$180,$423-f
                dc.w    $88a3,-2,$180,$312-f
                dc.w    $88c5,-2,$180,$423-f
                dc.w    $80,su_copper3>>16
                dc.w    $82,su_copper3&$ffff
                dc.w    $84,su_copper3>>16
                dc.w    $86,su_copper3&$ffff
                dc.l    -2

;==============================================================================

sqrt:           MACRO
                movem.l d0-d5,-(a7)
                move.l  \1,d0
                moveq   #0,d1
                moveq   #16-1,d2
                moveq   #0,d3
.\@loop:        move.l  d3,d5
                lsl.l   #2,d1
                move.l  d0,d4
                rol.l   #2,d4
                and.b   #$03,d4
                or.b    d4,d1
                lsl.l   #2,d0
                lsl.l   #2,d3
                addq.l  #1,d3
                sub.l   d3,d1
                bge.s   .\@lab
                add.l   d3,d1
                move.l  d5,d3
                lsl.l   #1,d3
                bra.s   .\@next
.\@lab:         move.l  d5,d3
                lsl.l   #1,d3
                or.b    #$01,d3
.\@next:        dbf     d2,.\@loop
                and.l   #$ffff,d3
                move.w  d3,.\@lab2+2
                movem.l (a7)+,d0-d5
.\@lab2:        move.w  #0,d2
                ENDM

fc_zoomin:      add.w   #fc_step,fc_addz        ;zoom in
                rts

fc_zoomout:     sub.w   #fc_step,fc_addz        ;zoom out
                rts

fc_waitstill:   bsr.s   fc_elli                 ;wait for ellipos 0
                movem.l d0-d7/a0-a6,-(a7)
                lea     fc_circlestruct1+8(pc),a0
                lea     fc_addtab(pc),a1
                moveq   #6-1,d7
.next:          tst.w   (a0)
                bne.s   .nostill
                clr.w   (a1)
.nostill:       addq.w  #2,a1
                lea     (fc_circlestruct2-fc_circlestruct1)(a0),a0
                dbf     d7,.next
                movem.l (a7)+,d0-d7/a0-a6
                rts

fc_elli:        movem.l d0-d7/a0-a6,-(a7)
                lea     fc_addtab(pc),a1
                lea     fc_circlestruct1+8(pc),a2
                moveq   #6-1,d7
.change:        move.w  (a2),d0                 ;get val
                add.w   (a1)+,d0                ;next
                bge.s   .nomin
                sub.w   -2(a1),d0               ;test if in range
                neg.w   -2(a1)
.nomin:         cmp.w   #19,d0
                ble.s   .notop
                sub.w   -2(a1),d0               ;test if in range
                neg.w   -2(a1)
.notop:         move.w  d0,(a2)                 ;save new val
                lea     (fc_circlestruct2-fc_circlestruct1)(a2),a2
                dbf     d7,.change
                movem.l (a7)+,d0-d7/a0-a6
                rts

fc_resetelli:   move.w  #2,fc_addtab            ;reset elli speed
                move.w  #1,fc_addtab+2
                move.w  #2,fc_addtab+4
                move.w  #1,fc_addtab+6
                move.w  #2,fc_addtab+8
                move.w  #1,fc_addtab+10
                rts

fc_mover:       movem.l d0-d7/a0-a6,-(a7)
                addq.w  #2*3,fc_alpha           ;change angles
                addq.w  #2*2,fc_beta
                addq.w  #2*1,fc_gamma
                and.w   #1022,fc_alpha
                and.w   #1022,fc_beta
                and.w   #1022,fc_gamma

                move.w  fc_addz(pc),d1
                add.w   #fc_destz,d1
                asr.w   #2,d1
                muls    d1,d1
                asr.l   #8,d1
                asr.l   #4,d1
fc_direction:   nop                             ;nop/neg.w d1 = $4e71/$4441
                move.w  d1,fc_addx

                movem.l (a7)+,d0-d7/a0-a6
                rts

fc_init:        movem.l d0-d7/a0-a6,-(a7)
                lea     $dff000,a6

                lea     fc_triangles,a0         ;clear mem
                move.w  #(fc_dataend-fc_triangles)/4-1,d7
.clear:         clr.l   (a0)+
                dbf     d7,.clear

                lea     fc_mulu,a0              ;mulu table
                moveq   #0,d0
                move.w  #fc_height-1,d7
.get:           move.w  d0,(a0)+
                add.w   #fc_bpr,d0
                dbf     d7,.get

                bsr.w   fc_initline             ;prepare blitter

                lea     fc_triangles+fc_framebpr,a5 ;skip width 0
                moveq   #0,d7
.drawtri:       move.w  #fc_trix,d0
                moveq   #0,d1
                move.w  #fc_trix-(fc_maxrad-1),d2
                move.w  #fc_maxrad-1,d3
                add.w   d7,d0
                add.w   d7,d2
                movem.l d7/a5,-(a7)
                bsr.w   fc_arealine             ;/
                movem.l (a7)+,d7/a5
                move.w  #fc_trix+1,d0
                moveq   #0,d1
                move.w  #fc_trix+1+(fc_maxrad-1),d2
                move.w  #fc_maxrad-1,d3
                add.w   d7,d0
                add.w   d7,d2
                movem.l d7/a5,-(a7)
                bsr.w   fc_arealine             ;\ => /\
                movem.l (a7)+,d7/a5
                lea     fc_framelen(a5),a5      ;next tri
                addq.w  #1,d7
                cmp.w   #15,d7
                ble.s   .drawtri

                lea     fc_triangles+fc_framebpr-2,a0
                move.l  #$09f0000a,d0
                move.w  #-fc_triwidth-fc_framebpr,d1
                move.w  #(8*(fc_maxrad+1))<<6+fc_triwidth/2,d2
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  a0,$0050(a6)
                move.l  a0,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.l  d0,$0040(a6)
                move.w  d2,$0058(a6)
                add.l   #8*fc_framelen,a0
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  a0,$0050(a6)
                move.l  a0,$0054(a6)
                move.w  d1,$0064(a6)
                move.w  d1,$0066(a6)
                move.l  d0,$0040(a6)
                move.w  d2,$0058(a6)

                lea     fc_build1(pc),a0        ;build copperlists
                lea     fc_build2(pc),a1
                lea     fc_build3(pc),a2
                lea     fc_build4,a3
                moveq   #$24,d0
                move.w  #fc_height-1,d7
.create:        move.b  d0,(a0)+
                move.b  d0,(a1)+
                move.b  d0,(a2)+
                move.b  d0,(a3)+
                move.b  #$01,(a0)+
                move.b  #$01,(a1)+
                move.b  #$01,(a2)+
                move.b  #$01,(a3)+
                move.w  #$fffe,(a0)+
                move.w  #$fffe,(a1)+
                move.w  #$fffe,(a2)+
                move.w  #$fffe,(a3)+
                move.l  #$00e20000,(a0)+
                move.l  #$00e20000,(a1)+
                move.l  #$00e20000,(a2)+
                move.l  #$00e20000,(a3)+
                move.l  #$00ea0000,(a0)+
                move.l  #$00ea0000,(a1)+
                move.l  #$00ea0000,(a2)+
                move.l  #$00ea0000,(a3)+
                move.l  #$00f20000,(a0)+
                move.l  #$00f20000,(a1)+
                move.l  #$00f20000,(a2)+
                move.l  #$00f20000,(a3)+
                move.l  #$00e60000,(a0)+
                move.l  #$00e60000,(a1)+
                move.l  #$00e60000,(a2)+
                move.l  #$00e60000,(a3)+
                move.l  #$00ee0000,(a0)+
                move.l  #$00ee0000,(a1)+
                move.l  #$00ee0000,(a2)+
                move.l  #$00ee0000,(a3)+
                move.l  #$00f60000,(a0)+
                move.l  #$00f60000,(a1)+
                move.l  #$00f60000,(a2)+
                move.l  #$00f60000,(a3)+
                cmp.b   #$ff,d0
                bne.s   .nospecial
                move.l  #$ffd1fffe,(a0)+
                move.l  #$ffd1fffe,(a1)+
                move.l  #$ffd1fffe,(a2)+
                move.l  #$ffd1fffe,(a3)+
                bra.s   .nonormal
.nospecial:     cmp.b   #$87,d0
                beq.s   .doBranch
                cmp.b   #$88,d0
                beq.s   .doBranch
                move.l  #$017e0000,(a0)+
                move.l  #$017e0000,(a1)+
                move.l  #$017e0000,(a2)+
                move.l  #$017e0000,(a3)+
.nonormal:      addq.w  #1,d0
                dbf     d7,.create
                bra.s   .skip

.doBranch:      move.l  #$008a0000,(a0)+
                move.l  #$008a0000,(a1)+
                move.l  #$008a0000,(a2)+
                move.l  #$008a0000,(a3)+
                addq.w  #1,d0
                dbf     d7,.create

.skip:          lea     fc_line0(pc),a0         ;<=> cos 0∞
                moveq   #0,d0
                moveq   #fc_maxrad-1,d7
.getcontab:     move.l  d0,(a0)+
                add.w   #fc_framebpr,d0
                dbf     d7,.getcontab

                lea     fc_radtab,a0            ;create table with dims at rad
                moveq   #0,d7
.nextrad:       move.l  a0,a1
                move.w  d7,d0                   ;r
                mulu    d0,d0                   ;r≤
                moveq   #1,d6                   ;start with x=1
.nextdim:       move.w  d6,d1                   ;x
                mulu    d1,d1                   ;x≤
                move.l  d0,d2
                sub.l   d1,d2                   ;r≤-x≤
                sqrt    d2                      ;result in d2
                add.w   d2,d2
                add.w   d2,d2
                move.w  d2,(a1)+                ;save dims
                addq.w  #1,d6
                cmp.w   d7,d6                   ;`til x=r
                blt.s   .nextdim
                lea     2*(fc_maxrad)(a0),a0
.fill:          cmp.l   a0,a1
                bge.s   .filled
                clr.w   (a1)+
                bra.s   .fill
.filled:        addq.w  #1,d7
                cmp.w   #fc_maxrad,d7           ;`til r=maxrad
                blt.w   .nextrad

                lea     DATA_AREA-$10000,a0     ;clear def. pointers
                moveq   #256/4-1,d7
.clear2:        clr.l   (a0)+
                dbf     d7,.clear2

                btst.b  #6,$0002(a6)            ;wait for blitter (?)
                bne.s   *-6

                movem.l (a7)+,d0-d7/a0-a6
                rts

fc_createline:  moveq   #0,d0                   ;a0-line d1-maxrad
                moveq   #fc_maxrad-1,d7
.getcontab:     move.w  d1,d2
                mulu    d0,d2                   ;x * maxrad / anz
                divu    #fc_maxrad-1,d2
                ext.l   d2
                mulu    #fc_framebpr,d2
                move.l  d2,(a0)+
                addq.w  #1,d0
                dbf     d7,.getcontab
                rts

fc_initwobbel:  movem.l d0-d7/a0-a6,-(a7)
                lea     fc_line1(pc),a0
                moveq   #fc_maxrad*970/1000,d1
                bsr.s   fc_createline
                lea     fc_line2(pc),a0
                moveq   #fc_maxrad*940/1000,d1
                bsr.s   fc_createline
                lea     fc_line3(pc),a0
                moveq   #fc_maxrad*910/1000,d1
                bsr.s   fc_createline
                lea     fc_line4(pc),a0
                moveq   #fc_maxrad*880/1000,d1
                bsr.s   fc_createline
                lea     fc_line5(pc),a0
                moveq   #fc_maxrad*850/1000,d1
                bsr.s   fc_createline
                lea     fc_line6(pc),a0
                moveq   #fc_maxrad*820/1000,d1
                bsr.s   fc_createline
                lea     fc_line7(pc),a0
                moveq   #fc_maxrad*790/1000,d1
                bsr.s   fc_createline
                lea     fc_line8(pc),a0
                moveq   #fc_maxrad*760/1000,d1
                bsr.s   fc_createline
                lea     fc_line9(pc),a0
                moveq   #fc_maxrad*730/1000,d1
                bsr.w   fc_createline
                lea     fc_line10(pc),a0
                moveq   #fc_maxrad*700/1000,d1
                bsr.s   fc_createline
                lea     fc_line11(pc),a0
                moveq   #fc_maxrad*670/1000,d1
                bsr.s   fc_createline
                lea     fc_line12(pc),a0
                moveq   #fc_maxrad*640/1000,d1
                bsr.w   fc_createline
                lea     fc_line13(pc),a0
                moveq   #fc_maxrad*610/1000,d1
                bsr.w   fc_createline
                lea     fc_line14(pc),a0
                moveq   #fc_maxrad*580/1000,d1
                bsr.w   fc_createline
                lea     fc_line15(pc),a0
                moveq   #fc_maxrad*550/1000,d1
                bsr.w   fc_createline
                lea     fc_line16(pc),a0
                moveq   #fc_maxrad*520/1000,d1
                bsr.w   fc_createline
                lea     fc_line17(pc),a0
                moveq   #fc_maxrad*490/1000,d1
                bsr.w   fc_createline
                lea     fc_line18(pc),a0
                moveq   #fc_maxrad*460/1000,d1
                bsr.w   fc_createline
                lea     fc_line19(pc),a0
                moveq   #fc_maxrad*430/1000,d1
                bsr.w   fc_createline
                movem.l (a7)+,d0-d7/a0-a6
                clr.b   fc_rotatelight
                rts

fc_initrotate:  movem.l d0-d7/a0-a6,-(a7)
                lea     fc_line1(pc),a0         ;cos 5∞
                moveq   #fc_maxrad*996/1000,d1
                bsr.w   fc_createline
                lea     fc_line2(pc),a0         ;cos 10∞
                moveq   #fc_maxrad*984/1000,d1
                bsr.w   fc_createline
                lea     fc_line3(pc),a0         ;cos 15∞
                moveq   #fc_maxrad*965/1000,d1
                bsr.w   fc_createline
                lea     fc_line4(pc),a0         ;cos 20∞
                moveq   #fc_maxrad*939/1000,d1
                bsr.w   fc_createline
                lea     fc_line5(pc),a0         ;cos 25∞
                moveq   #fc_maxrad*906/1000,d1
                bsr.w   fc_createline
                lea     fc_line6(pc),a0         ;cos 30∞
                moveq   #fc_maxrad*866/1000,d1
                bsr.w   fc_createline
                lea     fc_line7(pc),a0         ;cos 35∞
                moveq   #fc_maxrad*819/1000,d1
                bsr.w   fc_createline
                lea     fc_line8(pc),a0         ;cos 40∞
                moveq   #fc_maxrad*766/1000,d1
                bsr.w   fc_createline
                lea     fc_line9(pc),a0         ;cos 45∞
                moveq   #fc_maxrad*707/1000,d1
                bsr.w   fc_createline
                lea     fc_line10(pc),a0                ;cos 50∞
                moveq   #fc_maxrad*642/1000,d1
                bsr.w   fc_createline
                lea     fc_line11(pc),a0                ;cos 55∞
                moveq   #fc_maxrad*573/1000,d1
                bsr.w   fc_createline
                lea     fc_line12(pc),a0                ;cos 60∞
                moveq   #fc_maxrad*500/1000,d1
                bsr.w   fc_createline
                lea     fc_line13(pc),a0                ;cos 65∞
                moveq   #fc_maxrad*422/1000,d1
                bsr.w   fc_createline
                lea     fc_line14(pc),a0                ;cos 70∞
                moveq   #fc_maxrad*342/1000,d1
                bsr.w   fc_createline
                lea     fc_line15(pc),a0                ;cos 75∞
                moveq   #fc_maxrad*258/1000,d1
                bsr.w   fc_createline
                lea     fc_line16(pc),a0                ;cos 80∞
                moveq   #fc_maxrad*173/1000,d1
                bsr.w   fc_createline
                lea     fc_line17(pc),a0                ;cos 85∞
                moveq   #fc_maxrad*87/1000,d1
                bsr.w   fc_createline
                lea     fc_line18(pc),a0                ;cos 90∞
                moveq   #fc_maxrad*0/1000,d1
                bsr.w   fc_createline
                lea     fc_line19(pc),a0                ;cos 90∞
                moveq   #fc_maxrad*0/1000,d1
                bsr.w   fc_createline
                movem.l (a7)+,d0-d7/a0-a6
                move.b  #-1,fc_rotatelight
                rts

fc_initline:    btst.b  #6,$0002(a6)
                bne.s   *-6
                move.w  #fc_framebpr,$0060(a6)  ;bltcmod
                move.w  #fc_framebpr,$0066(a6)  ;bltdmod
                move.l  #$ffff8000,$0072(a6)    ;bltbdat(pattern) + bltadat
                move.l  #$ffffffff,$0044(a6)    ;bltafwm + bltalwm
                rts

;***            d0,d1   -p1
;***            d2,d3   -p2
;***            a5      -plane
;===            d0-d4,d6,d7 + a1,a4,a5 changed by routine
fc_arealine:    cmp.w   d1,d3
                beq.w   fc_noline       ;same ypos => no line
                bge.s   fc_noexchg
                exg.l   d0,d2
                exg.l   d1,d3
fc_noexchg:     sub.w   d0,d2           ;delta x
                sub.w   d1,d3           ;delta y
                move.w  d0,d7
                lsr.w   #3,d7           ;get xoff
                mulu    #fc_framebpr,d1
                add.w   d1,d7           ;add yoff
                lea     (a5,d7.w),a4    ;a4 = plane + off
                moveq   #$f,d6
                and.l   d6,d0           ;just the low bits $0*
                move.b  d0,d7           ;save x low bits
                ror.l   #4,d0           ;low bits $*0000000
                moveq   #$0004*4,d6
                move.w  d6,d0           ;get octs $*00000010 oct=4 (7)
                tst.w   d2              ;test delta x
                bpl.s   fc_noxvz
                neg.w   d2              ;delta x < 0 => oct=5 (4)
                addq.w  #1*4,d0         ;$*0000014
fc_noxvz:       cmp.w   d2,d3           ;delta x < delta y ?
                ble.s   fc_nodeltachg
                exg.l   d2,d3           ;exchg deltas
                sub.w   d6,d0           ;$*0000000/$*0000004
                add.w   d0,d0           ;$*0000000/$*0000008 oct=0/2 (6/5)
fc_nodeltachg:  move.w  d3,d6           ;dy
                sub.w   d2,d6           ;dy-dx
                add.w   d6,d6
                add.w   d6,d6           ;4*(dy-dx)
                add.w   d3,d3           ;2*dy
                move.w  d3,d4
                sub.w   d2,d4           ;2*dy-dx < 0 => signflag
                bpl.s   fc_nosign
                or.b    #$10*4,d0
fc_nosign:      or.l    #$0b4a0003,d0   ;bltcon0 +  bltcon1
                add.w   d3,d3           ;4*dy
                swap    d3
                move.w  d6,d3           ;4*dy + 4*(dy-dx) =bltbmod+bltamod
                addq.w  #1,d2           ;calc bltsize
                lsl.w   #6,d2
                addq.w  #2,d2           ;bltsize
                lea     $dff052,a1
                btst.b  #6,$02-$52(a1)  ;$0002 wait for blitter
                bne.s   *-6
                move.l  d3,$62-$52(a1)  ;$0062 bltbmod + bltamod
                move.l  d0,$40-$52(a1)  ;$0040 bltcon0 + bltcon1
                move.l  a4,$48-$52(a1)  ;$0048 bltcpt
                move.w  d4,(a1)+        ;$0052 bltaptl
                move.l  a4,(a1)+        ;$0054 bltdpt
                move.w  d2,(a1)         ;$0058 bltsize
fc_noline:      rts


fc_buildcopper: bsr     fc_changecopper
                movem.l d0-d7/a0-a6,-(a7)       ;main routine -> build coplist
                IF      SHOW=1 THEN
                bchg.b  #1,$bfe001
                ENDIF
                bsr.w   fc_rebuild
                bsr.w   fc_rotate
                bsr.w   fc_sort
                lea     fc_circlestruct1(pc),a0
fc_circleoff1:  move.w  #0*4,d0
                bsr.w   fc_createcircle
                lea     fc_circlestruct2(pc),a0
fc_circleoff2:  move.w  #1*4,d0
                bsr.w   fc_createcircle
                lea     fc_circlestruct3(pc),a0
                move.w  #2*4,d0
                bsr.w   fc_createcircle
                lea     fc_circlestruct4(pc),a0
                move.w  #3*4,d0
                bsr.w   fc_createcircle
                lea     fc_circlestruct5(pc),a0
                move.w  #4*4,d0
                bsr.w   fc_createcircle
                lea     fc_circlestruct6(pc),a0
                move.w  #5*4,d0
                bsr.w   fc_createcircle
                bsr.w   fc_setcolors
                IF      SHOW=1 THEN
                move.w  #$00f,$dff180
                ENDIF
                movem.l (a7)+,d0-d7/a0-a6
                rts

fc_changecopper:movem.l d0-d1/a0,-(a7)
                movem.l fc_showptr(pc),d0-d2
                exg.l   d0,d2
                exg.l   d1,d2
                movem.l d0-d2,fc_showptr
                move.l  d2,a0
                move.l  d2,d0

                cmp.l   #fc_copper1,d0
                bne.s   .no1
                move.w  #fc_branch11&$ffff,242(a0)
                bra.s   .coprdy
.no1:           cmp.l   #fc_copper2,d0
                bne.s   .no2
                move.w  #fc_branch12&$ffff,242(a0)
                bra.s   .coprdy
.no2:           move.w  #fc_branch13&$ffff,242(a0)
.coprdy:
                move.w  d0,fc_maincopper+2+4
                swap    d0
                move.w  d0,fc_maincopper+2
                movem.l (a7)+,d0-d1/a0
                rts

fc_rotate:      lea     fc_sinus,a0             ;rotate 6 points
                lea     fc_cosinus,a1
                lea     fc_koor(pc),a2
                lea     fc_destkoor(pc),a3
                moveq   #6-1,d5
.next1:         movem.w (a2)+,d0-d2
                move.w  fc_alpha(pc),d3
                move.w  d1,d4
                move.w  d2,d7
                muls    (a1,d3.w),d1
                muls    (a0,d3.w),d2
                sub.l   d2,d1
                swap    d1
                add.w   d1,d1
                muls    (a0,d3.w),d4
                muls    (a1,d3.w),d7
                add.l   d7,d4
                swap    d4
                add.w   d4,d4
                move.w  d4,d2
                move.w  fc_beta(pc),d3
                move.w  d0,d4
                move.w  d2,d7
                muls    (a1,d3.w),d0
                muls    (a0,d3.w),d2
                add.l   d2,d0
                swap    d0
                add.w   d0,d0
                neg.w   d4
                muls    (a0,d3.w),d4
                muls    (a1,d3.w),d7
                add.l   d7,d4
                swap    d4
                add.w   d4,d4
                move.w  d4,d2
                move.w  fc_gamma(pc),d3
                move.w  d0,d4
                move.w  d1,d7
                muls    (a1,d3.w),d0
                muls    (a0,d3.w),d1
                sub.l   d1,d0
                swap    d0
                add.w   d0,d0
                muls    (a0,d3.w),d4
                muls    (a1,d3.w),d7
                add.l   d7,d4
                swap    d4
                add.w   d4,d4
                move.w  d4,d1
                movem.w d0-d2,(a3)
                addq.l  #3*2,a3
                dbf     d5,.next1
                lea     fc_destkoor(pc),a0      ;pers
                lea     fc_zpos(pc),a1
                lea     fc_addx(pc),a3
                moveq   #9,d3
                move.w  #fc_bpr*8/2,d4
                move.w  #fc_height/2,d5
                moveq   #6-1,d7
.next2:         movem.w (a0)+,d0-d2             ;rotated
                add.w   (a3),d0
                add.w   2(a3),d1
                add.w   4(a3),d2
                beq.s   .nopers
                ext.l   d0
                ext.l   d1
                asl.l   d3,d0
                asl.l   d3,d1
                divs    d2,d0
                divs    d2,d1
.nopers:        add.w   d4,d0
                add.w   d5,d1
                movem.w d0-d2,-3*2(a0)
                move.w  d2,(a1)+
                dbf     d7,.next2
                lea     fc_destkoor(pc),a0      ;calc radi
                lea     fc_circlestruct1(pc),a1
                moveq   #6-1,d7
.copy:          movem.w (a0)+,d0/d1/d2
                move.l  #-512<<9,d3
                move.w  d2,d6
                tst.w   d2
                beq.s   .big
                divs    d2,d3
                move.w  d3,d2
                ble.s   .mini
                cmp.w   #fc_maxrad,d2           ;check for max/min
                blt.s   .ok
                IF      SHOW=1 THEN
                move.w  #$008,$dff180
                ENDIF
.big:           move.w  #fc_maxrad-1,d2
.ok:            movem.w d0/d1/d2,(a1)
                move.w  d6,10(a1)
                lea     (fc_circlestruct2-fc_circlestruct1)(a1),a1
                dbf     d7,.copy
                rts
.mini:          moveq   #1,d2
                movem.w d0/d1/d2,(a1)
                move.w  d6,10(a1)
                lea     (fc_circlestruct2-fc_circlestruct1)(a1),a1
                dbf     d7,.copy
                rts

fc_rebuild:     lea     $dff000,a6
                lea     fc_copper4,a0                   ;referenz-list
                move.l  fc_clearptr(pc),a1              ;clear old one
                btst.b  #6,$0002(a6)
                bne.s   *-6
                move.l  #$09f00000,$0040(a6)
                move.l  #$ffffffff,$0044(a6)
                move.l  #$00000000,$0064(a6)
                movem.l a0/a1,$0050(a6)
                move.w  #((fc_copper4end-fc_copper4)/16)<<6+8,$0058(a6)
                rts

fc_sort:        lea     fc_zpos(pc),a1                  ;seach for maximum
                lea     fc_circleoff1+2(pc),a4          ;(not very fast, but
                moveq   #5*4,d5                         ;ok for 6 values)
                moveq   #6-1,d7
.search:        move.l  a1,a5                                   
                move.w  #-32000,d0                      ;max startval
                moveq   #0,d3
                moveq   #6-1,d6
.next:          cmp.w   (a5)+,d0
                bge.s   .nomax
                lea     -2(a5),a2                       ;save pos ptr
                move.w  (a2),d0                         ;use as new max
                move.w  d3,d4                           ;save offsets
.nomax:         add.w   #(fc_circleoff2-fc_circleoff1),d3
                dbf     d6,.next
                move.w  #-32000,(a2)                    ;kill last max
                move.w  d5,(a4,d4.w)
                subq.w  #4,d5
                dbf     d7,.search
                rts

fc_zpos:        dcb.w   6,0

fc_setcolors:   move.l  fc_buildptr(pc),a0              ;set colors in right
                lea     10*4+2(a0),a0                   ;order
                lea     fc_colorlist(pc),a1             ;put colors into list
                move.w  (a1)+,0*4(a0)
                move.w  (a1),1*4(a0)
                move.w  (a1)+,2*4(a0)
                move.w  (a1),3*4(a0)
                move.w  (a1),4*4(a0)
                move.w  (a1),5*4(a0)
                move.w  (a1)+,6*4(a0)
                lea     7*4(a0),a0
                move.w  (a1)+,0*4(a0)
                move.w  (a1),1*4(a0)
                move.w  (a1)+,2*4(a0)
                move.w  (a1),3*4(a0)
                move.w  (a1),4*4(a0)
                move.w  (a1),5*4(a0)
                move.w  (a1),6*4(a0)
                rts

; a0=ColorPtr, d0=FromColor, d1=MaxPos, d4=Pos, d6=ToColor
; changes d0,d1,d3,d5

SIMPLEFADE:     move.w  d1,.div1+2
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
                or.w    d0,d3   ;<- hier wurde ein move entfernt
                rts

fc_createcircle:move.l  fc_buildptr(pc),a3              ;elli-creationroutine
                lea     24*4+2(a3,d0.w),a3              ;highptr
                move.l  a3,fc_highptr+2
                move.w  d0,d2                           ;offset x*4
                asr.w   #1,d2                           ;offset x*2
                lea     fc_colorlist(pc),a1

                movem.l d0-d7/a0/a2,-(a7)
                move.w  #-fc_startz-fc_destz,d1         ;max delta norm. fade
                move.w  10(a0),d4
                sub.w   #fc_startz,d4                   ;pos. in delta range
                move.w  10(a0),d3                       ;inc circle-delta
                sub.w   fc_addz(pc),d3                  ;colorchange
                add.w   d3,d3
                add.w   d3,d3
                add.w   d3,d3
                sub.w   #2^3*fc_disrad,d3
                add.w   d3,d4
                tst.b   fc_rotatelight
                beq.s   .rangecheck
                lea     fc_cosgrade(pc),a2              ;brightness `cause rot.
                move.w  8(a0),d3                        ;angle-offset
                add.w   d3,d3
                move.w  (a2,d3.w),d3                    ;cos*fc_coschange
                asl.w   #5,d3
                add.w   d3,d4
.rangecheck:    tst.w   d4
                bgt.s   .noMin
                moveq   #0,d4
.noMin:         cmp.w   #-fc_startz-fc_destz,d4
                ble.s   .noMax
                move.w  d1,d4
.noMax:         move.w  6(a0),d0                        ;bright color
                move.w  12(a0),d6                       ;dark color
                bsr.w   SIMPLEFADE
                move.w  d3,(a1,d2.w)
                movem.l (a7)+,d0-d7/a0/a2

                move.w  (a0),d2                         ;x-cliping (out)
                move.w  4(a0),d3
                ble.w   fc_nocircle
                add.w   d3,d2                           ;xpos+rad
                blt.w   fc_nocircle
                sub.w   d3,d2
                sub.w   d3,d2
                cmp.w   #fc_bpr*8,d2
                bgt.w   fc_nocircle
                move.w  2(a0),d1                        ;y-clipping (out)
                add.w   d3,d1                           ;ypos+rad
                ble.w   fc_nocircle
                sub.w   d3,d1
                sub.w   d3,d1
                cmp.w   #fc_height,d1
                bge.w   fc_nocircle
                move.w  2(a0),d1
                ext.l   d0
                add.l   #fc_build1-fc_copper1+4+2,d0    ;add start
                add.l   fc_buildptr(pc),d0              ;offset + addr
                move.l  d0,a6
                mulu    #(6*4+4+4),d1                   ;y-copperlist-offset
                add.l   d1,d0
                move.l  d0,a1                           ;where to put in
                lea     fc_triangles,a2                 ;where to get from
                move.w  (a0),d0
                moveq   #15,d1
                and.w   d0,d1                           ;xpos&15
                asr.w   #4,d0                           ;xpos>>3&$fffe
                add.w   d0,d0
                mulu    #fc_framelen,d1                 ;(xpos&15)*len = yoff
                move.w  #fc_trix/8,d2
                sub.w   d0,d2                           ;xoff
                ext.l   d2
                add.l   d2,a2
                add.l   d1,a2                           ;get triangleptr
                move.w  d3,d7                           ;get radius
                lea     fc_line0(pc),a3
                move.w  8(a0),d6
                mulu    #(fc_line1-fc_line0),d6
                add.l   d6,a3
                lea     fc_radtab,a4
                tst.w   2(a0)                           ;special clipping modes
                ble.w   fc_clip3
                cmp.w   #fc_height,2(a0)
                bge.w   fc_clip4
                cmp.w   2(a0),d3
                bge.s   fc_clip1
                move.w  #fc_height,d4
                sub.w   d3,d4
                cmp.w   2(a0),d4
                blt.w   fc_clip2

fc_noclip:      lea     -(6*4+4+4)(a1),a5               ;mirrorptr
                move.w  d7,d0
                mulu    #2*(fc_maxrad),d0               ;get rad list
                add.l   d0,a4
                subq.w  #1,d7
                bsr.s   fc_sethigh
                move.w  a2,d6
.copy:          move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d6,d0
                move.w  d0,(a1)                         ;set high
                move.w  d0,(a5)                         ;set high
                lea     (6*4+4+4)(a1),a1                ;nextline
                lea     -(6*4+4+4)(a5),a5
                dbf     d7,.copy
fc_nocircle:    rts

fc_sethigh:     move.w  (a4),d1                         ;set highbits of ptr
                move.l  a2,d0
                add.l   (a3,d1.w),d0
                swap    d0
fc_highptr:     move.w  d0,$00000000.l
                rts

fc_clip1:       lea     -(6*4+4+4)(a1),a5               ;mirrorptr
                move.w  d7,d0
                mulu    #2*(fc_maxrad),d0               ;get rad list
                add.l   d0,a4
                move.w  2(a0),d6
                sub.w   d6,d7
                subq.w  #1,d6
                blt.s   fc_nocircle
                bsr.s   fc_sethigh
                move.w  a2,d5
.copy:          move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a1)                         ;set high
                move.w  d0,(a5)                         ;set high
                lea     (6*4+4+4)(a1),a1                ;nextline
                lea     -(6*4+4+4)(a5),a5
                dbf     d6,.copy
                subq.w  #1,d7
                blt.s   fc_nocircle
.copy2:         move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a1)                         ;set high
                lea     (6*4+4+4)(a1),a1
                dbf     d7,.copy2
                rts

fc_clip2:       lea     -(6*4+4+4)(a1),a5               ;mirrorptr
                move.w  d7,d0
                mulu    #2*(fc_maxrad),d0               ;get rad list
                add.l   d0,a4
                move.w  #fc_height,d6
                sub.w   2(a0),d6
                sub.w   d6,d7
                subq.w  #1,d6
                blt.w   fc_nocircle
                bsr.w   fc_sethigh
                move.w  a2,d5
.copy:          move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a1)                         ;set high
                move.w  d0,(a5)                         ;set high
                lea     (6*4+4+4)(a1),a1                ;nextline
                lea     -(6*4+4+4)(a5),a5
                dbf     d6,.copy
                subq.w  #1,d7
                blt.w   fc_nocircle
.copy2:         move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a5)                         ;set high
                lea     -(6*4+4+4)(a5),a5
                dbf     d7,.copy2
                rts

fc_clip3:       move.l  a6,a1
                move.w  d7,d0
                mulu    #2*(fc_maxrad),d0               ;get rad list
                add.l   d0,a4
                subq.w  #1,d7
                move.w  2(a0),d6
                add.w   d6,d7
                blt.w   fc_nocircle
                neg.w   d6
                add.w   d6,d6
                lea     (a4,d6.w),a4
                bsr.w   fc_sethigh
                move.w  a2,d5
.copy:          move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a1)                         ;set high
                lea     (6*4+4+4)(a1),a1                ;nextline
                dbf     d7,.copy
                rts

fc_clip4:       lea     (fc_height-1)*(6*4+4+4)(a6),a1;last line
                move.w  d7,d0
                mulu    #2*(fc_maxrad),d0               ;get rad list
                add.l   d0,a4
                subq.w  #1,d7
                move.w  2(a0),d6
                sub.w   #fc_height,d6
                sub.w   d6,d7
                blt.w   fc_nocircle
                add.w   d6,d6
                lea     (a4,d6.w),a4
                bsr.w   fc_sethigh
                move.w  a2,d5
.copy:          move.w  (a4)+,d1
                move.w  2(a3,d1.w),d0
                add.w   d5,d0
                move.w  d0,(a1)                         ;set high
                lea     -(6*4+4+4)(a1),a1               ;nextline
                dbf     d7,.copy
                rts

fc_coschange    equ     512

fc_cosgrade:    dc.w    fc_coschange-fc_coschange
                dc.w    fc_coschange*970/1000-fc_coschange
                dc.w    fc_coschange*940/1000-fc_coschange
                dc.w    fc_coschange*910/1000-fc_coschange
                dc.w    fc_coschange*880/1000-fc_coschange
                dc.w    fc_coschange*850/1000-fc_coschange
                dc.w    fc_coschange*820/1000-fc_coschange
                dc.w    fc_coschange*790/1000-fc_coschange
                dc.w    fc_coschange*760/1000-fc_coschange
                dc.w    fc_coschange*730/1000-fc_coschange
                dc.w    fc_coschange*700/1000-fc_coschange
                dc.w    fc_coschange*670/1000-fc_coschange
                dc.w    fc_coschange*640/1000-fc_coschange
                dc.w    fc_coschange*610/1000-fc_coschange
                dc.w    fc_coschange*580/1000-fc_coschange
                dc.w    fc_coschange*550/1000-fc_coschange
                dc.w    fc_coschange*520/1000-fc_coschange
                dc.w    fc_coschange*490/1000-fc_coschange
                dc.w    fc_coschange*460/1000-fc_coschange
                dc.w    fc_coschange*430/1000-fc_coschange

fc_circlestruct1:       dc.w    160                     ;xpos
                        dc.w    128                     ;ypos
                        dc.w    1                       ;radius
                        dc.w    $d75                    ;color
                        dc.w    0                       ;radiusoffset (cos a)
                        dc.w    0                       ;zpos
                        dc.w    $532

fc_circlestruct2:       dc.w    160
                        dc.w    128
                        dc.w    1
                        dc.w    $8c6
                        dc.w    0
                        dc.w    0
                        dc.w    $353

fc_circlestruct3:       dc.w    160
                        dc.w    128
                        dc.w    1
                        dc.w    $db7
                        dc.w    0
                        dc.w    0
                        dc.w    $543

fc_circlestruct4:       dc.w    160
                        dc.w    128
                        dc.w    1
                        dc.w    $7ae
                        dc.w    0
                        dc.w    0
                        dc.w    $345

fc_circlestruct5:       dc.w    160
                        dc.w    128
                        dc.w    1
                        dc.w    $c7d
                        dc.w    0
                        dc.w    0
                        dc.w    $535

fc_circlestruct6:       dc.w    160
                        dc.w    128
                        dc.w    1
                        dc.w    $6cb
                        dc.w    0
                        dc.w    0
                        dc.w    $354

fc_addtab:              dcb.w   6,0             ;speedtab

fc_koor:                dc.w    -fc_disrad*866/1000, fc_disrad/2,0
                        dc.w                      0,   fc_disrad,0
                        dc.w     fc_disrad*866/1000, fc_disrad/2,0
                        dc.w     fc_disrad*866/1000,-fc_disrad/2,0
                        dc.w                      0,  -fc_disrad,0
                        dc.w    -fc_disrad*866/1000,-fc_disrad/2,0
fc_destkoor:            dcb.w   3*6,0
fc_alpha:               dc.w    0
fc_beta:                dc.w    0*2*512/360
fc_gamma:               dc.w    0
fc_addx:                dc.w    14000
fc_addy:                dc.w    0
fc_addz:                dc.w    fc_startz
fc_rotatelight:         dc.w    0

fc_showptr:             dc.l    fc_copper1
fc_clearptr:            dc.l    fc_copper2
fc_buildptr:            dc.l    fc_copper3

fc_colorlist:           dcb.w   6,0

fc_line0:               dcb.l   fc_maxrad,0     ;triangle-offset-table
fc_line1:               dcb.l   fc_maxrad,0
fc_line2:               dcb.l   fc_maxrad,0
fc_line3:               dcb.l   fc_maxrad,0
fc_line4:               dcb.l   fc_maxrad,0
fc_line5:               dcb.l   fc_maxrad,0
fc_line6:               dcb.l   fc_maxrad,0
fc_line7:               dcb.l   fc_maxrad,0
fc_line8:               dcb.l   fc_maxrad,0
fc_line9:               dcb.l   fc_maxrad,0
fc_line10:              dcb.l   fc_maxrad,0
fc_line11:              dcb.l   fc_maxrad,0
fc_line12:              dcb.l   fc_maxrad,0
fc_line13:              dcb.l   fc_maxrad,0
fc_line14:              dcb.l   fc_maxrad,0
fc_line15:              dcb.l   fc_maxrad,0
fc_line16:              dcb.l   fc_maxrad,0
fc_line17:              dcb.l   fc_maxrad,0
fc_line18:              dcb.l   fc_maxrad,0
fc_line19:              dcb.l   fc_maxrad,0

FREESPRITE:             dc.l    0,0

fc_colors:      MACRO
                dc.w    $009c,$8004
                dc.w    $008e,$2471
                dc.w    $0090,$33d1
                dc.w    $0092,$0030
                dc.w    $0094,$00d8
                dc.w    $0100,$6600
                dc.w    $0102,$0000
                dc.w    $0104,$0040
                dc.w    $0108,$0000
                dc.w    $010a,$0000
                dc.w    $0182,$000f
                dc.w    $0184,$0ff0
                dc.w    $0186,$0ff0
                dc.w    $0188,$0f80
                dc.w    $018a,$0f80
                dc.w    $018c,$0f80
                dc.w    $018e,$0f80
                dc.w    $0192,$0080
                dc.w    $0194,$0800
                dc.w    $0196,$0800
                dc.w    $0198,$0f0f
                dc.w    $019a,$0f0f
                dc.w    $019c,$0f0f
                dc.w    $019e,$0f0f
                dc.w    $00e0,fc_triangles>>16
                dc.w    $00e8,fc_triangles>>16
                dc.w    $00f0,fc_triangles>>16
                dc.w    $00e4,fc_triangles>>16
                dc.w    $00ec,fc_triangles>>16
                dc.w    $00f4,fc_triangles>>16

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
        dc.w    $1a2,$534-f,$1a4,$523-f,$1a6,$312-f           ; Spritefarben
        dc.w    $1aa,$534-f,$1ac,$523-f,$1ae,$312-f
        dc.w    $1b2,$534-f,$1b4,$523-f,$1b6,$312-f
        dc.w    $1ba,$534-f,$1bc,$523-f,$1be,$312-f
        dc.w    $0084,fc_branch11>>16
        dc.w    $0086,fc_branch11&$ffff

                IF      SHOW=0 THEN
                dc.w    $0180,$0423-f
                ENDIF
                ENDM

fc_back1:       MACRO
                dc.w    $8759-fc_korr,-2,$180,$312-f
                dc.w    $8779-fc_korr,-2,$180,$423-f
                dc.w    $87a3-fc_korr,-2,$180,$312-f
                dc.w    $87c5-fc_korr,-2,$180,$423-f
                ENDM

fc_back2:       MACRO
                dc.w    $8859-fc_korr,-2,$180,$312-f
                dc.w    $8879-fc_korr,-2,$180,$423-f
                dc.w    $88a3-fc_korr,-2,$180,$312-f
                dc.w    $88c5-fc_korr,-2,$180,$423-f
                ENDM

fc_branch11:    fc_back1
                dc.w    $0082,(fc_copper1+3448)&$ffff
                dc.w    $0086,fc_branch21&$ffff
                dc.w    $0088,$0000
fc_branch12:    fc_back1
                dc.w    $0082,(fc_copper2+3448)&$ffff
                dc.w    $0086,fc_branch22&$ffff
                dc.w    $0088,$0000
fc_branch13:    fc_back1
                dc.w    $0082,(fc_copper3+3448)&$ffff
                dc.w    $0086,fc_branch23&$ffff
                dc.w    $0088,$0000

fc_branch21:    fc_back2
                dc.w    $0082,(fc_copper1+3448+32)&$ffff
                dc.w    $0086,fc_branch11&$ffff
                dc.w    $0088,$0000
fc_branch22:    fc_back2
                dc.w    $0082,(fc_copper2+3448+32)&$ffff
                dc.w    $0086,fc_branch12&$ffff
                dc.w    $0088,$0000
fc_branch23:    fc_back2
                dc.w    $0082,(fc_copper3+3448+32)&$ffff
                dc.w    $0086,fc_branch13&$ffff
                dc.w    $0088,$0000

fc_maincopper:  dc.w    $0080,fc_copper1>>16
                dc.w    $0082,fc_copper1&$ffff
                dc.w    $0088,$0000
                dc.w    $ffff,$fffe

                cnop    0,16
fc_copper1:     fc_colors
fc_build1:      dcb.b   fc_height*(6*4+4+4),0
                dc.w    $0080,fc_maincopper>>16
                dc.w    $0082,fc_maincopper&$ffff
                dc.w    $ffff,$fffe

                cnop    0,16
fc_copper2:     fc_colors
fc_build2:      dcb.b   fc_height*(6*4+4+4),0
                dc.w    $0080,fc_maincopper>>16
                dc.w    $0082,fc_maincopper&$ffff
                dc.w    $ffff,$fffe

                cnop    0,16
fc_copper3:     fc_colors
fc_build3:      dcb.b   fc_height*(6*4+4+4),0
                dc.w    $0080,fc_maincopper>>16
                dc.w    $0082,fc_maincopper&$ffff
                dc.w    $ffff,$fffe

                cnop    0,16
fc_copper4:     fc_colors
fc_build4:      dcb.b   fc_height*(6*4+4+4),0
fc_copper4end:

fc_sinus:
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
fc_cosinus:
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
fc_ende:

ende

len     =ende-code_area

;´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´´
fc_triangles    equ     DATA_AREA-8*fc_framelen
fc_mulu         equ     fc_triangles+16*fc_framelen
fc_RADtab       equ     fc_mulu+2*fc_height
fc_dataend      equ     fc_RADtab+2*((fc_maxrad+1)^2)

                IF2

                IF      fc_framelen>$2000 THEN
                PRINTT  "ERROR: `fc_framelen` verl‰ﬂt Bereich 0..$2000 !"
                FAIL
                ENDIF

                IF      (DATA_AREA-$10000)<=fc_ende THEN
                PRINTT  "ERROR: Daten und Code ¸berschneiden sich !"
                FAIL
                ENDIF

                IF      (fc_dataend&$fffff)>$80000 THEN
                PRINTT  "ERROR: Daten verlassen Speichergrenze !"
                FAIL
                END

                ENDIF

>extern         "Sprite1.raw",SPRITE_EMBLEM_1
>extern         "Sprite2.raw",SPRITE_EMBLEM_2
>extern         "Sprite3.raw",SPRITE_EMBLEM_3
>extern         "Sprite4.raw",SPRITE_EMBLEM_4
>extern         "Sprite5.raw",SPRITE_EMBLEM_5
>extern         "Sprite6.raw",SPRITE_EMBLEM_6
