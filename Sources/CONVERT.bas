1 REMark SToQL: Main Program v1.30
100 REMark $$asmb=win2_UTILS_qlib_ext,0,12
110 REMark $$asmb=win2_SToQL_Source_convert_cfg,0,10
111 REMark $$asmb=win2_UTILS_FN_ext,0,12
112 REMark $$asmb=win2_UTILS_CGH_ext,14,118
113 REMark $$asmb=win2_SToQL_EDLINE_ext,0,10
114 REMark $$asmb=win2_UTILS_pixel_ext,14,352
115 REMark $$asmb=win2_SToQL_header_ext,0,12
120 REMark $$stak=1024
130 REMark $$heap=1024
140 DIM a$(52):vs$='1.30':REMark *****
150 minerva%=0:a$=VER$:IF a$='JSL1':minerva%=1
160 IF RMODE<>4:MODE 4
170 IF minerva%=1:MODE 16512+DEFAULT_SCR*16+(16128-256*16),-1
180 OPEN #0,'con_400x30a56x216'
190 DIM heading$(50),heading1$(50),pname$(52),pname1$(52),fkey$(7)
200 DIM fnam$(52),fname1$(52),f$(52),dev$(52),ext$(52),fdev$(52),fext$(52)
210 DIM medium$(52),dummy$(1),exp1$(7),exp2$(7),pest$(1),zx$(30)
220 DIM d$(200,37),m$(14,26),nc(8),file$(52)
230 DIM temp$(100),coord$(12),cx$(3),cy$(3),cw$(3),cd$(3)
240 DIM drive$(52),picture_file$(52),st_file$(52),defdev$(52)
250 DIM stext$(52),qldef$(52),qlext$(52),qldev$(52),defext$(52)
260 initialise
270 credits
280 convert_menu
290 :
300 DEFine PROCedure fil (a%,b%)
310 y%=b%:c%=PIXEL%(a%,y%):inc%=ql/4:rmax%=512-inc%:temp_l%=a%
320 temp_r%=a%:IF temp_r%>rmax%:temp_r%=rmax%
330 fill_start=fill_start3:BLANK_FILL fill_buff,0
340 FOR dirn%=1,-1
350  REPeat upf%
360   first_left%=0:temp_r2%=temp_r%:temp_l2%=temp_l%
370   FOR xt%=temp_l% TO temp_r% STEP inc%
380    IF INKEY$=CHR$(27):RETurn
390    IF PEEK(fill_start+xt%)<>col%:IF y%<>b%:NEXT xt%:EXIT xt%
400    IF PIXEL%(xt%,y%)<>c%:NEXT xt%:EXIT xt%
410    temp_l2%=0
420    FOR xl%=xt%-inc% TO 0 STEP -inc%
430     IF PIXEL%(xl%,y%)<>c%:temp_l2%=xl%+inc%:EXIT xl%
440     POKE fill_start2+xl%,col%:IF y%=b%:POKE fill_buff+xl%,col%
450    END FOR xl%
460    IF first_left%=0:temp_l%=temp_l2%:first_left%=1
470    FOR xr%=xt%+inc% TO rmax% STEP inc%
480     IF PIXEL%(xr%,y%)<>c%:temp_r2%=xr%-inc%:EXIT xr%
490     POKE fill_start2+xr%,col%:IF y%=b%:POKE fill_buff+xr%,col%
500    NEXT xr%
510     xr%=512:temp_r2%=rmax%
520    END FOR xr%
530    xt%=temp_r2%:DRAW_BLOK xr%-temp_l2%,1,temp_l2%,y%,col%
540    IF temp_l2%<temp_l%:temp_l%=temp_l2%
550    IF temp_r2%>temp_r%:temp_r%=temp_r2%
560   END FOR xt%
570   IF temp_r2%<temp_r%:temp_r%=temp_r2%
580   IF y%=b%:line_left%=temp_l%:line_right%=temp_r%
590   BLANK_FILL fill_buff,1
600   y%=y%-dirn%:IF first_left%=0:EXIT upf%
610   IF y%<0 OR y%>255:EXIT upf%
620   fill_start=fill_start3
630  END REPeat upf%
640  temp_l%=line_left%:temp_r%=line_right%:y%=b%+1
650  fill_start=fill_buff
660 END FOR dirn%
670 END DEFine
680 :
690 DEFine PROCedure touch_up
700  SCR_CO screen1 TO SCREEN(#1):help_screen
710  REPeat main
720  IF return_to_convert:EXIT main
730  OVER#3,0:INK#3,col%:PAPER#3,col2%
740  inc=ql*step_size/4:siz2=INT(siz*4/ql)
750  x=INT(x):y=INT(y)
760  IF x+siz>511:x=512-siz
770  IF y+siz2>255:y=256-siz2
780  xx=x:yy=y
790  OVER#3,-1:BLOCK#3,siz,siz2,x,y,col%
800  ctemp=col%+2*((col%=0) OR (col%=64) OR (col%=128) OR (col%=192))
810  REPeat loop
820   leave=0:place=0:OVER#3,-1
830   REPeat kloop
840    a=CODE(INKEY$(30))
850    IF a:EXIT kloop
860    BLOCK#3,siz,siz2,x,y,ctemp:FOR r=1 TO 300:END FOR r
870    BLOCK#3,siz,siz2,x,y,ctemp
880   END REPeat kloop
890   SELect ON a
900    =6,194,198,202,206,210,214,218,222:BLOCK#3,siz,siz2,x,y,col%
910   END SELect
920   SELect ON a
930    =216:y=y+step_size:IF y+siz2>255:y=256-siz2
940    =220:y=y+10*step_size:IF y+siz2>255:y=256-siz2
950    =200:x=x+inc:IF x+siz>511:x=512-siz
960    =204:x=x+10*inc:IF x+siz>511:x=512-siz
970    =208:y=y-step_size:IF y<0:y=0
980    =212:y=y-10*step_size:IF y<0:y=0
990    =192:x=x-inc:IF x<0:x=0
1000    =196:x=x-10*inc:IF x<0:x=0
1010    =232:help_screen
1020    =236,240:leave=1
1030    =32:place=1
1040    =6:fil x,y
1050    =194:PAN#3,-1*inc
1060    =198:PAN#3,-10
1070    =202:PAN#3,inc
1080    =206:PAN#3,10
1090    =210:SCROLL#3,-1*step_size
1100    =214:SCROLL#3,-10
1110    =218:SCROLL#3,step_size
1120    =222:SCROLL#3,10
1130    =19:leave=2
1140    =12:leave=3
1150    =179:leave=4
1160    =172:leave=5
1170    =9:leave=6
1180    =18:leave=7
1190    =2:leave=8
1200    =24:leave=12
1210    =17:leave=13
1220   END SELect
1230   SELect ON a:=6,194,198,202,206,210,214,218,222:BLOCK#3,siz,siz2,x,y,col%
1240   IF trail
1250     OVER#3,0:BLOCK#3,siz,siz2,xx,yy,col2%:OVER#3,-1
1260   ELSE
1270   BLOCK#3,siz,siz2,xx,yy,col%
1280   END IF
1290   BLOCK#3,siz,siz2,x,y,col%
1300   IF leave:EXIT loop
1310   IF place:OVER#3,0:BLOCK#3,siz,siz2,x,y,col2%:OVER#3,-1:BLOCK#3,siz,siz2,x,y,col%
1320   xx=x:yy=y
1330   FOR r=1 TO flash_rate*10:END FOR r
1340  END REPeat loop
1350  BLOCK#3,siz,siz2,x,y,col%
1360  SCR_CO SCREEN(#1) TO screen1
1370  IF a=236:menu (0):SCR_CO screen1 TO SCREEN(#1):ELSE IF a=240:screen_manip 1
1380  IF leave>1:menu (leave)
1390  END REPeat main
1400 END DEFine touch_up
1410 :
1420 DEFine PROCedure initialise
1430 OPEN#1,'con_512x256a0x0_128':PAPER 0:CLS:INK 4
1440 OPEN#3,'scr_512x256a0x0'
1450 OPEN#4,'con_512x256a0x0_128'
1460 OPEN#5,'con_512x256a0x0_128'
1470 OPEN#7,'con_376x140a72x60_128'
1480 OPEN#8,'scr_160x112a90x80'
1490 OPEN#9,'con_162x22a270x90_128'
1500 OPEN#10,'con_162x22a270x130_128'
1510 OPEN#11,'scr_60x14a280x170'
1520 OPEN#12,'scr_60x14a362x170'
1530 picture_file$="NONE":st_file$="NONE":file_size=0:offset=0
1540 col%=7:col2%=0:trail=0:step_size=1:x=200:y=100:siz=2:flash_rate=C_BYTE(1)
1550 ql=4:transfer_type=1:ic1=7:ic2=7:ic3=3:pc1=0:pc2=0:pc3=3
1560 screen1=RESPR(32768):screen2=RESPR(32768):screen3=RESPR(32800):st_screen=RESPR(32128):file_buf=RESPR(64):fill_buff=RESPR(1536)
1565 scr_wid=SCRINC(#1)
1570 fill_start2=fill_buff+1024:fill_start3=fill_buff+512
1580 SCR_CO SCREEN(#1) TO screen1:SCR_CO SCREEN(#1) TO screen2:SCR_SA 0,0,80,50,screen3,SCREEN(#1):cutx=0:cuty=0:cutw=80:cutd=50:menu_paper=C_CODE(1)
1590 IF menu_paper<0:menu_paper=menu_paper+256
1600 defdev$=C_STRG$(1):stext$=C_STRG$(3):qldef$=C_STRG$(2):qlext$=C_STRG$(4):qldev$=qldef$
1610 menu_ink=7:select_ink=4:IF menu_paper=220:menu_ink=0:select_ink=2
1620 IF defdev$(LEN(defdev$))<>'_':defdev$=defdev$&'_'
1630 IF find_dev$(defdev$)='':defdev$='flp1_'
1640 IF qldev$(LEN(qldev$))<>'_':qldev$=qldev$&'_'
1650 IF find_dev$(qldev$)='':qldev$='flp1_'
1660 IF LEN(stext$)<1:stext$='_neo'
1670 IF LEN(qlext$)<1:qlext$='_scr'
1680 IF qlext$(1)<>'_':qlext$='_'&qlext$
1690 IF stext$(1)<>'_':stext$='_'&stext$
1700 defext$=stext$
1710 END DEFine
1720 :
1730 DEFine PROCedure menu (pre_select)
1740  LOCal pos,lpos,loopa
1750  pos=1:lpos=1:defext$=qlext$
1760  ferr=0
1770  REPeat menu_loop
1780   IF pre_select=0 THEN
1790    WINDOW#4,244,240,130,10:SET_MODE 4
1800    PAPER#4,menu_paper:CLS#4:BORDER#4,1,2
1810    AT#4,0,0:STRIP#4,0:INK#4,7:CSIZE#4,1,0:PRINT#4,"       IMAGE PROCESSING       ":AT#4,16,0:PRINT#4,"        CURRENT STATUS        "
1820   ELSE
1830    SELect ON pre_select=2,3,8,12,13:SET_MODE 4
1840   END IF
1850   items=13
1860   m$(1)="RETURN TO IMAGE"
1870   m$(2)="SAVE PICTURE"
1880   m$(3)="LOAD PICTURE"
1890   m$(4)="STORE PICTURE IN BUFFER"
1900   m$(5)="RECALL PICTURE FROM BUFFER"
1910   m$(6)="INK / PAPER COLOURS"
1920   m$(7)="RECOLOUR MENU"
1930   m$(8)="CURSOR MENU"
1940   m$(9)="MANIPULATE SCREEN"
1950   m$(10)="FORMAT MEDIUM"
1960   m$(11)="DELETE FILES"
1970   m$(12)="CONVERT ST SCREEN"
1980   m$(13)="QUIT PROGRAM"
1990   IF pre_select=0 THEN
2000    PAPER#4,menu_paper:CSIZE#4,0,0:INK#4,menu_ink:AT#4,18,2:PRINT#4,"FILENAME :- ";picture_file$
2010    AT#4,20,2:PRINT#4,"INK COLOUR :- ";:PAPER#4,col%:PRINT#4;"  ";:PAPER#4,menu_paper:PRINT#4;"  PAPER COLOUR :- ";:PAPER#4,col2%:PRINT#4;"  ":PAPER#4,menu_paper
2020    AT#4,22,2:PRINT#4,"X = ";x;"  Y = ";y;"  STEP = ";step_size;"  TRAIL = ";:IF trail:PRINT#4;"Y":ELSE PRINT#4;"N"
2030  :
2040    operate_menu pos,26,1
2050   ELSE
2060    selected=pre_select
2070   END IF
2080   PAPER#4,0:BORDER#4,0
2090   SELect ON selected
2100    =1:EXIT menu_loop
2110    =2:IF picture_file$<>"NONE"
2120        temp$=SELECT_FILE$(m$(2),qldev$&defext$,picture_file$,11)
2130       ELSE
2140        temp$=SELECT_FILE$(m$(2),qldev$&defext$,"",10)
2150       END IF
2160       IF temp$<>""
2170        Q_ERR_ON "open_in":OPEN_IN#6,temp$:Q_ERR_OFF:CLOSE#6
2180        IF NOT Q_ERR:OVERWRITE:IF carry_on=1:DELETE temp$:ELSE :NEXT menu_loop
2190        Q_ERR_ON "sbytes":SBYTES temp$,screen1,32768:Q_ERR_OFF
2200        IF Q_ERR:error_message 1:ELSE error_message 0:picture_file$=temp$:qldev$=find_dev$(picture_file$):defext$=find_ext$(temp$)
2210        IF qldev$='':qldev$=qldef$
2220       END IF
2230    IF ql=8:CLS:ELSE SCR_CO screen1 TO SCREEN(#1)
2240    =3:temp$=SELECT_FILE$(m$(3),qldev$&defext$,"",8)
2250       IF temp$<>""
2260        Q_ERR_ON "open_in":OPEN_IN#6,temp$:Q_ERR_OFF
2270        IF NOT Q_ERR
2280         GetHEAD #6,file_buf
2290         ferr=(PEEK_L(file_buf)>32768)+PEEK(file_buf+5)
2300         IF NOT ferr: Q_ERR_ON "lbytes":LBYTES temp$,screen1:Q_ERR_OFF
2310        END IF
2320        IF Q_ERR OR ferr:error_message 1:ELSE error_message 0:picture_file$=temp$:qldev$=find_dev$(picture_file$):defext$=find_ext$(temp$)
2330        IF qldev$="":qldev$=qldef$
2340        CLOSE#6
2350       END IF
2360    IF ql=8:CLS:ELSE SCR_CO screen1 TO SCREEN(#1)
2370    =4:SCR_CO screen1 TO screen2:error_message 0
2380    =5:SCR_CO screen2 TO screen1:IF NOT pre_select:error_message 0
2390    =6:colour_menu:IF ql=8:CLS:ELSE SCR_CO screen1 TO SCREEN(#1)
2400    =7:recolour_menu
2410    =8:cursor_menu
2420    =9:screen_manip 0
2430    =10:format_it
2440    =11:REPeat delete_it
2450         temp$=SELECT_FILE$(m$(11),qldev$,"",8)
2460         IF temp$="":EXIT delete_it
2470         Q_ERR_ON "delete"
2480         DELETE temp$:Q_ERR_OFF
2490         qldev$=find_dev$(temp$):IF qldev$='':qldev$=qldef$
2500         IF Q_ERR:error_message 1
2510        END REPeat delete_it
2520        IF ql=8:CLS:ELSE SCR_CO screen1 TO SCREEN(#1)
2530    =12:wanna_quit:IF return_to_convert=1:qlext$=defext$:RETurn
2540    =13:stop_prog
2550   END SELect
2560  IF pre_select:EXIT menu_loop
2570  END REPeat menu_loop
2580  IF pre_select THEN
2590   SELect ON pre_select=2,3,8,12,13:IF ql=8:SET_MODE 8
2600   SCR_CO screen1 TO SCREEN(#1)
2610  ELSE
2620   IF ql=8:SET_MODE 8
2630  END IF
2640 END DEFine
2650 :
2660 DEFine PROCedure cursor_menu
2670  bw=siz:cout=0:IF bw<ql/4:bw=ql/4
2680  lbw=bw:WINDOW#5,198,158,150,40
2690  PAPER#5,menu_paper:CLS#5:BORDER#5,1,2
2700  CSIZE#5,1,0:PAPER#5,0:INK#5,7:PRINT#5,"      CURSOR  MENU      "
2710  CSIZE#5,0,0:INK#5,menu_ink:PAPER#5,menu_paper:AT#5,2,2:PRINT#5,"CURSOR ....."
2720  AT#5,4,2:PRINT#5,"DELAY  : ";flash_rate
2730  AT#5,6,2:PRINT#5,"STEP   : ";step_size
2740  AT#5,8,2:PRINT#5,"TRAIL  : ";:IF trail:PRINT#5;"YES":ELSE PRINT#5;"NO"
2750  AT#5,10,0:PRINT#5," CTRL ¾ ¿ - CHANGE CURSOR DELAY"\"    ¼ ½ - CHANGE CURSOR SIZE"\"     ¾ ¿ - CHANGE STEP SIZE"\"     <ENTER> - TOGGLE TRAIL"\"        <ESC> - EXIT  "
2760  BLOCK#5,bw,bw,136-bw/2,60-bw/2,menu_ink
2770  REPeat cloop
2780   key=CODE(INKEY$(-1))
2790   SELect ON key
2800    =210:flash_rate=flash_rate+1*(flash_rate<12):AT#5,4,11:PRINT#5,flash_rate;' '
2810    =218:flash_rate=flash_rate-1*(flash_rate>0):AT#5,4,11:PRINT#5,flash_rate;' '
2820    =192:bw=bw-ql/4:IF bw<1:bw=ql/4
2830    =200:bw=bw+ql/4:IF bw>78:bw=78
2840    =208:step_size=step_size+1*(step_size<10):AT#5,6,11:PRINT#5,step_size;" "
2850    =216:step_size=step_size-1*(step_size>1):AT#5,6,11:PRINT#5,step_size;" "
2860    =10:trail=(NOT trail):IF trail:AT#5,8,11:PRINT#5,"YES":ELSE AT#5,8,11:PRINT#5,"NO "
2870    =27,32:cout=1
2880   END SELect
2890   IF bw<>lbw
2900    BLOCK#5,lbw,lbw,136-lbw/2,60-lbw/2,menu_paper
2910    BLOCK#5,bw,bw,136-bw/2,60-bw/2,menu_ink:lbw=bw
2920   END IF
2930   IF cout:EXIT cloop
2940  END REPeat cloop
2950  siz=bw
2960  PAPER#5,0:BORDER#5,0
2970 END DEFine
2980 :
2990 DEFine PROCedure error_message (e1)
3000 IF RMODE=4:WINDOW#5,150,40,200,100:ELSE WINDOW#5,300,40,125,100
3010 PAPER#5,menu_paper:CLS#5:BORDER#5,1,2:INK#5,menu_ink:CSIZE#5,0,0
3020 IF e1=0:AT#5,1,2:PRINT#5,"OK! PRESS ANY KEY TO        CONTINUE........"
3030 IF e1=1:AT#5,1,2:PRINT#5,"ERROR! PRESS ANY KEY           TO CONTINUE.."
3040 Waits
3050 END DEFine
3060 :
3070 DEFine FuNction find_coords$
3080  LOCal cx,cy,leave
3090  leave=0:cx$="000":cy$=cx$:cd$=cx$:cw$=cx$
3100  cx=cutx:cy=cuty:cw=cutw:cd=cutd:lcx=cx:lcy=cy:lw=cw:ld=cd
3110  OVER -1:DRAW_BLOK cw,1,cx,cy,4:DRAW_BLOK cw,1,cx,cy+cd-1,4:BLOCK 2,cd-2,cx,cy+1,4:BLOCK 2,cd-2,cx+cw-2,cy+1,4
3120  REPeat fcloop
3130   key=CODE(INKEY$(-1))
3140   SELect ON key
3150    =27:leave=2
3160    =32:leave=1
3170    =200,201:cx=cx+8:IF cx+cw>512:cx=512-cw
3180    =192,193:cx=cx-8:IF cx<0:cx=0
3190    =208:cy=cy-1:IF cy<0:cy=0
3200    =209:cy=cy-step_size:IF cy<0:cy=0
3210    =216:cy=cy+1:IF cy+cd>256:cy=256-cd
3220    =217:cy=cy+step_size:IF cy+cd>256:cy=256-cd
3230    =194,198:IF NOT past:cw=cw-8:IF cw=0:cw=8
3240    =202,206:IF NOT past:cw=cw+8:IF cw+cx>512:cw=512-cx
3250    =210:IF NOT past:cd=cd-1:IF cd<4:cd=4
3260    =214:IF NOT past:cd=cd-step_size:IF cd<4:cd=4
3270    =218:IF NOT past:cd=cd+1:IF cd+cy>256:cd=256-cy
3280    =222:IF NOT past:cd=cd+step_size:IF cd+cy>256:cd=256-cy
3290   END SELect
3300   IF leave:EXIT fcloop
3310   IF lcx<>cx OR lcy<>cy OR lw<>cw OR ld<>cd
3320    DRAW_BLOK lw,1,lcx,lcy,4:DRAW_BLOK lw,1,lcx,lcy+ld-1,4:BLOCK 2,ld-2,lcx,lcy+1,4:BLOCK 2,ld-2,lcx+lw-2,lcy+1,4
3330    DRAW_BLOK cw,1,cx,cy,4:DRAW_BLOK cw,1,cx,cy+cd-1,4:BLOCK 2,cd-2,cx,cy+1,4:BLOCK 2,cd-2,cx+cw-2,cy+1,4
3340    lcx=cx:lcy=cy:ld=cd:lw=cw
3350   END IF
3360  END REPeat fcloop
3370  DRAW_BLOK cw,1,cx,cy,4:DRAW_BLOK cw,1,cx,cy+cd-1,4:BLOCK 2,cd-2,cx,cy+1,4:BLOCK 2,cd-2,cx+cw-2,cy+1,4:OVER 0
3380  IF leave=2:RETurn "esc"
3390  cx$(4-LEN(cx) TO 3)=cx:cy$(4-LEN(cy) TO 3)=cy:cw$(4-LEN(cw) TO 3)=cw:cd$(4-LEN(cd) TO 3)=cd
3400  coord$=cx$&cy$&cw$&cd$
3410  RETurn coord$
3420 END DEFine
3430 :
3440 DEFine PROCedure cut
3450  LOCal temp$(12)
3460  past=0
3470  temp$=find_coords$
3480  IF temp$=="esc":RETurn
3490  cutx=temp$(1 TO 3):cuty=temp$(4 TO 6):cutw=temp$(7 TO 9):cutd=temp$(10 TO 12)
3500  SCR_SA cutx,cuty,cutw,cutd,screen3,SCREEN(#1)
3510 END DEFine
3520 :
3530 DEFine PROCedure paste
3540  LOCal temp$(12),cx,cy
3550  past=1
3560  temp$=find_coords$
3570  IF temp$=="esc":RETurn
3580  cx=temp$(1 TO 3):cy=temp$(4 TO 6)
3590  SCR_LO cx,cy,screen3,SCREEN(#1)
3600 END DEFine
3610 :
3620 DEFine PROCedure wanna_quit
3630  WINDOW#5,150,22,180,100:PAPER#5,menu_paper:INK#5,menu_ink:CLS#5
3640  BORDER#5,1,2:AT#5,1,1:CSIZE#5,0,0:PRINT#5,"RETURN TO THE CONVERT      MENU ( Y / N ) ?"
3650  IF INKEY$(-1)=="y":return_to_convert=1
3660  BORDER#5,0:PAPER#5,0
3670 END DEFine
3680 :
3690 DEFine FuNction colour_no (c,cc,ss)
3700 LOCal col%
3710  c1=c:c2=cc:stip=ss:temp=c2^^c1
3720  col%=((c1&&1) + (c1&&2) + (c1&&4) + 8*((temp&&1)<>0) + 16*((temp&&2)<>0) + 32*((temp&&4)<>0) + 64*stip)
3730 IF stip=0 OR c1=c2:col%=col% MOD 64
3740 RETurn col%
3750 END DEFine
3760 :
3770 DEFine PROCedure colour_menu
3780  IF ql=8 AND NOT pre_select:SET_MODE 8
3790  WINDOW#5,436,172,40,30:PAPER#5,2:CLS#5:BORDER#5,1,2
3800  CSIZE#5,2,0:AT#5,0,0:PAPER#5,0:INK#5,7:PRINT#5,"             COLOUR MENU            "
3810  AT#5,16,0:PRINT#5,"  USE   ¼ ½ ¾ ¿   TO ALTER COLOURS  "
3820  AT#5,2,17:PAPER#5,2:INK#5,4:PRINT#5,"INK        PAPER"
3830  AT#5,5,2:PRINT#5,"MAIN COLOUR"
3840  AT#5,9,2:PRINT#5,"   CONTRAST"
3850  AT#5,13,2:PRINT#5,"    MIXTURE"
3860  colour_box 4,17,ic1:colour_box 4,29,pc1
3870  colour_box 8,17,ic2:colour_box 8,29,pc2
3880  colour_box 12,17,colour_no (ic1,ic2,ic3)
3890  colour_box 12,29,colour_no (pc1,pc2,pc3)
3900  column=1:row=2:get_out=0:oic1=ic1:oic2=ic2:oic3=ic3:opc1=pc1:opc2=pc2:opc3=pc3:ocolumn=column:orow=row
3910  AT#5,5,21:INK#5,7:PAPER#5,2:PRINT#5,CHR$(184)
3920  REPeat coloop
3930   key=CODE(INKEY$(-1))
3940   SELect ON key
3950    =192:IF column=1
3960          SELect ON row
3970           =2:ic1=ic1-8/ql:IF ic1<0:ic1=8-8/ql
3980           =3:ic2=ic2-8/ql:IF ic2<0:ic2=8-8/ql
3990           =4:ic3=ic3-1:IF ic3<0:ic3=3
4000          END SELect
4010         ELSE
4020          SELect ON row
4030           =1:column=1
4040           =2:pc1=pc1-8/ql:IF pc1<0:pc1=8-8/ql
4050           =3:pc2=pc2-8/ql:IF pc2<0:pc2=8-8/ql
4060           =4:pc3=pc3-1:IF pc3<0:pc3=3
4070          END SELect
4080         END IF
4090    =200:IF column=2
4100          SELect ON row
4110           =2:pc1=pc1+8/ql:IF pc1>7:pc1=0
4120           =3:pc2=pc2+8/ql:IF pc2>7:pc2=0
4130           =4:pc3=pc3+1:IF pc3=4:pc3=0
4140          END SELect
4150         ELSE
4160          SELect ON row
4170           =1:column=2
4180           =2:ic1=ic1+8/ql:IF ic1>7:ic1=0
4190           =3:ic2=ic2+8/ql:IF ic2>7:ic2=0
4200           =4:ic3=ic3+1:IF ic3=4:ic3=0
4210          END SELect
4220         END IF
4230    =208:row=row-1:IF row=0:row=1
4240    =216:row=row+1:IF row=5:row=4
4250    =27,32,10:get_out=1
4260   END SELect
4270   IF get_out:EXIT coloop
4280   IF ic1<>oic1:colour_box 4,17,ic1:colour_box 12,17,colour_no (ic1,ic2,ic3)
4290   IF ic2<>oic2:colour_box 8,17,ic2:colour_box 12,17,colour_no (ic1,ic2,ic3)
4300   IF ic3<>oic3:colour_box 12,17,colour_no (ic1,ic2,ic3)
4310   IF pc1<>opc1:colour_box 4,29,pc1:colour_box 12,29,colour_no (pc1,pc2,pc3)
4320   IF pc2<>opc2:colour_box 8,29,pc2:colour_box 12,29,colour_no (pc1,pc2,pc3)
4330   IF pc3<>opc3:colour_box 12,29,colour_no (pc1,pc2,pc3)
4340   oic1=ic1:oic2=ic2:oic3=ic3:opc1=pc1:opc2=pc2:opc3=pc3
4350   IF column<>ocolumn:AT#5,2,17:INK#5,4+3*(column=1):PRINT#5,"INK":INK#5,4+3*(column=2):AT#5,2,28:PRINT#5,"PAPER"
4360   IF row<>orow
4370    IF orow>1:AT#5,(orow-1)*4+1,21+12*(column=2):PAPER#5,2:PRINT#5," "
4380    IF row=1
4390     AT#5,2,17:INK#5,4+3*(column=1):PRINT#5,"INK":INK#5,4+3*(column=2):AT#5,2,28:PRINT#5,"PAPER"
4400    ELSE
4410     AT#5,(row-1)*4+1,21+12*(column=2):PAPER#5,2:INK#5,7:PRINT#5,CHR$(184)
4420    END IF
4430   END IF
4440   ocolumn=column:orow=row
4450  END REPeat coloop
4460  col%=colour_no(ic1,ic2,ic3):col2%=colour_no (pc1,pc2,pc3)
4470  PAPER#5,0:BORDER#5,0
4480 END DEFine
4490 :
4500 DEFine PROCedure colour_box (cby,cbx,cbc)
4510  AT#5,cby,cbx:PAPER#5,cbc:PRINT#5,"   "
4520  AT#5,cby+1,cbx:PRINT#5,"   "
4530  AT#5,cby+2,cbx:PRINT#5,"   "
4540 END DEFine
4550 :
4560 DEFine PROCedure recolour_menu
4570  LOCal r,leave
4580  xpointer=0:old_pointer=0:leave=0
4590  FOR r=0 TO 7 STEP 8/ql:nc(r)=r:nc(r+1)=r
4600  WINDOW#5,196,222,150,10
4610  IF ql=8 AND NOT pre_select:SET_MODE 8
4620  PAPER#5,menu_paper:CLS#5:BORDER#5,1,2
4630  PAPER#5,0:INK#5,7:CSIZE#5,2,0:PRINT#5," RECOLOUR  MENU "
4640  AT#5,20,0:PAPER#5,7:INK#5,2:PRINT#5,"¼¾¿½ :- TO ALTER        COLOURS "
4650  PAPER#5,menu_paper:INK#5,menu_ink:AT#5,2,0:PRINT#5," OLD   ½½   NEW"
4660  FOR r=0 TO 7
4670   recolour_box (4+2*r),2,r:recolour_box (4+2*r),13,r
4680  END FOR r
4690  AT#5,4,7:PAPER#5,menu_paper:INK#5,menu_ink:PRINT#5,CHR$(185);CHR$(185)
4700  REPeat recloop
4710   key=CODE(INKEY$(-1))
4720   SELect ON key
4730    =192:nc(xpointer)=nc(xpointer)-8/ql
4740         IF nc(xpointer)<0:nc(xpointer)=8-8/ql
4750         recolour_box (4+xpointer*2),13,nc(xpointer)
4760         IF ql=4:recolour_box (6+xpointer*2),13,nc(xpointer):nc(xpointer+1)=nc(xpointer)
4770    =200:nc(xpointer)=nc(xpointer)+8/ql
4780         IF nc(xpointer)>7:nc(xpointer)=0
4790         recolour_box (4+xpointer*2),13,nc(xpointer)
4800         IF ql=4:recolour_box (6+xpointer*2),13,nc(xpointer):nc(xpointer+1)=nc(xpointer)
4810    =208:xpointer=xpointer-8/ql:IF xpointer<0:xpointer=xpointer+8/ql
4820    =216:xpointer=xpointer+8/ql:IF xpointer>7:xpointer=xpointer-8/ql
4830    =32,10:leave=1
4840    =27:leave=2
4850   END SELect
4860   IF xpointer<>old_pointer:PAPER#5,menu_paper:INK#5,menu_ink:AT#5,(4+2*old_pointer),7:PRINT#5,"  ":AT#5,(4+2*xpointer),7:PRINT#5,CHR$(185);CHR$(185)
4870   old_pointer=xpointer
4880   IF leave:EXIT recloop
4890  END REPeat recloop
4900  PAPER#5,0:BORDER#5,0
4910  IF leave=2:RETurn
4920  SCR_CO screen1 TO SCREEN(#1)
4930  RECOL nc(0),nc(1),nc(2),nc(3),nc(4),nc(5),nc(6),nc(7)
4940  SCR_CO SCREEN(#1) TO screen1
4950 END DEFine
4960 :
4970 DEFine PROCedure expand (direction,incr)
4980  LOCal r,count
4990  IF incr=1
5000   IF direction=1:start=128:finish=255:incr2=2:count=0:ELSE start=127:finish=0:incr2=-2:count=255
5010  ELSE
5020   IF direction=1:start=85:finish=254:incr2=1.5:count=0:ELSE start=170:finish=0:incr2=-1.5:count=255
5030  END IF
5040  FOR r=start TO finish STEP direction
5050   SCR_SA 0,r,512,1,screen3,SCREEN(#1)
5060   FOR i=0 TO incr*direction STEP direction:SCR_LO 0,INT(count+i),screen3,SCREEN(#1)
5070   count=count+incr2
5080   IF CODE(INKEY$)=27:EXIT r
5090  END FOR r
5100 END DEFine
5110 :
5120 DEFine PROCedure recolour_box (rx,ry,rc)
5130  AT#5,rx,ry-1:PAPER#5,rc:PRINT#5,"   "
5140 END DEFine
5150 :
5160 DEFine PROCedure screen_manip(whre)
5170  LOCal leave
5180  items=14:leave=0
5190  WINDOW#4,172,192,180,20:INK#4,menu_ink
5200  SET_MODE 4
5210  PAPER#4,menu_paper:CLS#4:CSIZE#4,1,0:BORDER#4,1,2:PAPER#4,0:INK#4,7:PRINT#4,"  MANIPULATE SCREEN  "
5220  PAPER#4,7:INK#4,2:AT#4,17,0:PRINT#4," ¾¿ & SPACE TO SELECT"
5230  AT#4,18,0:PRINT#4,"   < ESC > TO EXIT   "
5240  m$(1)="COMPRESS TO LEFT"
5250  m$(2)="COMPRESS TO RIGHT"
5260  m$(3)="COMPRESS UPWARDS"
5270  m$(4)="COMPRESS DOWNWARDS"
5280  m$(5)="ENLARGE TO LEFT"
5290  m$(6)="ENLARGE TO RIGHT"
5300  m$(7)="ENLARGE UPWARDS"
5310  m$(8)="ENLARGE DOWNWARDS"
5320  m$(9)="MIRROR VERTICALLY"
5330  m$(10)="MIRROR HORIZONTALLY"
5340  m$(11)="CHANGE MODE"
5350  m$(12)="CUT SCREEN BLOCK"
5360  m$(13)="PASTE SCREEN BLOCK"
5370  m$(14)="WIPE SCREEN"
5380  operate_menu 1,18,1
5390  BORDER#4,0:PAPER#4,0
5400  IF leave=99:IF whre=1:IF ql=8:SET_MODE 8:END IF :SCR_CO screen1 TO SCREEN(#1):END IF :RETurn
5410  SELect ON selected=1,2:set_ratio .5:=5 TO 8:set_ratio 2
5420  IF ql=8:SET_MODE 8
5430  IF selected<>11:SCR_CO screen1 TO SCREEN(#1)
5440  SELect ON selected
5450   =1:IF comp_flag>-1:IF comp_flag=1:COMPL66#1:ELSE COMPL50#1
5460   =2:IF comp_flag>-1:IF comp_flag=1:COMPR66#1:ELSE COMPR50#1
5470   =3:COMP_UP#1
5480   =4:COMP_DOWN#1
5490   =5:IF comp_flag>-1:IF comp_flag=1:EXPNDL33#1:ELSE EXPNDL100#1
5500   =6:IF comp_flag>-1:IF comp_flag=1:EXPNDR33#1:ELSE EXPNDR100#1
5510   =7:IF comp_flag>-1:IF comp_flag=1:expand 1,2:ELSE expand 1,1
5520   =8:IF comp_flag>-1:IF comp_flag=1:expand -1,2:ELSE expand -1,1
5530   =9:INVERT#1
5540   =10:SCR_FLIP#1
5550   =11:ql=4*(ql=4)+4:SET_MODE ql:SCR_CO screen1 TO SCREEN(#1):IF ql=8:UNFLASH#1
5560   =12:cut
5570   =13:paste
5580   =14:wipe_screen
5590  END SELect
5600  SCR_CO SCREEN(#1) TO screen1
5610 END DEFine
5620 :
5630 DEFine PROCedure wipe_screen
5640  WINDOW#5,242,22,150,80:PAPER#5,menu_paper:INK#5,menu_ink:CSIZE#5,2,0
5650  CLS#5:BORDER#5,1,2:PRINT#5," OK TO CLEAR SCREEN     ( Y / N ) ?"
5660  IF INKEY$(-1)=="y":CLS#3 :ELSE SCR_CO screen1 TO SCREEN(#1)
5670  BORDER#5,0:PAPER#5,0
5680 END DEFine
5690 :
5700 DEFine PROCedure credits
5710  WINDOW#5,260,102,120,70:PAPER#5,menu_paper:CLS#5:BORDER#5,1,2
5720  PAPER#5,0:INK#5,7:CSIZE#5,1,0:PRINT#5," ST - QL SCREEN CONVERSION"&vs$
5730  PAPER#5,menu_paper:INK#5,menu_ink:AT#5,2,1:PRINT#5;"   (c) 1991, Alan Pemberton"\TO 12;"& Rich Mellor"
5740  PRINT#5\\"   Compiled with Q_Liberator"\\"Uses some DIY Toolkit extensions"\\
5750  PAPER#5,7:INK#5,2:PRINT#5,"    PRESS ANY KEY TO START...   "
5760  Waits
5770 END DEFine
5780 :
5790 DEFine PROCedure format_it
5800  WINDOW#4,164,62,150,80:PAPER#4,menu_paper:INK#4,menu_ink:CSIZE#4,1,0
5810  CLS#4:BORDER#4,1,7:PAPER#4,0:INK#4,7:PRINT#4,"   FORMAT  MEDIUM   "
5820  AT#4,4,0:PRINT#4,"ENTER DEVICE & LABEL"
5830  PRINT#4,'  OR QUIT TO ABORT  '
5840  WINDOW#4,110,22,176,95:PAPER#4,0:INK#4,4:CSIZE#4,0,0
5850  CLS#4:BORDER#4,1,4
5860  temp$=EDLINE$(4,15,"")
5870  IF temp$=="quit" OR find_dev$(temp$)="":RETurn
5880  Q_ERR_ON "format"
5890  INK#4,2
5900  FORMAT#4,temp$:Q_ERR_OFF
5910  IF Q_ERR:error_message 1
5920 END DEFine format_it
5930 :
5940 DEFine PROCedure DRAW_FILES_BOX
5950 init_winds
5960 BORDER#box,1,select_ink
5970 PRINT#9,pname$:INK#10,2:PRINT#10,fnam$
5980 END DEFine
5990 :
6000 DEFine FuNction SELECT_FILE$(heading1$,pname1$,fname1$,box1)
6010 heading$=heading1$:pname$=pname1$:fnam$=fname1$:box=box1
6020 leave=0:carry_on=0:count=0:file$=""
6030 IF fnam$="":fnam$=pname$
6040 DRAW_FILES_BOX
6050 fkey$=CHR$(192)&CHR$(200)&CHR$(208)&CHR$(216)&CHR$(10)&CHR$(32)&CHR$(27)
6060 REMark left,right,up,down,enter,space,esc
6070 IF pname$<>"":get_direc:IF oops=1:DRAW_FILES_BOX
6080 REPeat loop
6090  IF carry_on:BORDER#box,1,menu_paper:box=carry_on:carry_on=0:BORDER#box,1,select_ink
6100  IF box<>8 OR count=0:box=select_box
6110  IF box=0
6120   FOR r=12 TO 7 STEP -1:PAPER#r,0:BORDER#r,0
6130   RETurn ""
6140  END IF
6150  BORDER#box,1,7
6160  SELect ON box
6170   =9:
6180     CLS#9:pname$=EDLINE$(9,42,pname$)
6190     f$=find_dev$(pname$):IF f$="":pname$=find_dev$(pname1$)&pname$:IF pname$='':pname$=defdev$:END IF :CLS#9:PRINT#9,pname$
6200     get_direc:carry_on=8
6210     IF oops=1:DRAW_FILES_BOX:carry_on=0:ELSE fnam$=pname$:CLS#10:PRINT#10,fnam$
6220  =8:
6230     f$=scroll_select$
6240     carry_on=9:IF f$<>"":fnam$=find_dev$(pname$)&f$:IF fnam$='':fnam$=defdev$:END IF :CLS#10:PRINT#10,fnam$:carry_on=11
6250  =10:
6260     CLS#10:fnam$=EDLINE$(10,42,fnam$)
6270     carry_on=11
6280  =11,12:leave=2*(box=11)-1
6290  END SELect
6300  IF NOT leave:NEXT loop
6310  IF leave=1
6320   dev$=find_dev$(pname$):ext$=find_ext$(pname$)
6330   fdev$=find_dev$(fnam$):fext$=find_ext$(fnam$)
6340   IF fdev$<>"":dev$="":ELSE IF dev$="":dev$=find_dev$(pname1$)
6350   IF fext$<>"":ext$=""
6360   dev$=dev$&fnam$&ext$:CLS#10:INK#10,2:PRINT#10,dev$
6370  END IF
6380  FOR r=12 TO 7 STEP -1:PAPER#r,0:BORDER#r,0
6390  IF leave=-1:RETurn ""
6400  RETurn dev$
6410 END REPeat loop
6420 END DEFine
6430 :
6440 DEFine FuNction find_dev$(drive$)
6450 driveset=DEV_OK(drive$)
6460 IF driveset>1:RETurn drive$(1 TO driveset)
6470 IF driveset<>1 OR LEN(drive$)<4:RETurn ""
6480 driveset=DEV_OK(drive$(4 TO))
6490 IF driveset>1:RETurn drive$(TO 3+driveset)
6500 RETurn ""
6510 END DEFine
6520 :
6530 DEFine FuNction find_ext$ (a$)
6540  LOCal a,temp$(52),x,loop
6550  alen=LEN(a$):temp$=a$
6560  IF alen<2:RETurn ""
6570  x=0:x1=0
6580  REPeat loop
6590   x="_" INSTR temp$
6600   IF NOT x:EXIT loop
6610   IF x=LEN(temp$):EXIT loop
6620   x1=x1+x
6630   temp$=temp$(x+1 TO LEN(temp$))
6640  END REPeat loop
6650  IF x1=5:IF find_dev$(a$)<>"":RETurn ""
6660  IF x1 AND NOT x:RETurn a$(x1 TO alen):ELSE RETurn ""
6670 END DEFine
6680 :
6690 DEFine FuNction select_box
6700  REPeat boxloop
6710   key=INKEY$(#7,-1) INSTR fkey$
6720   IF key=0:NEXT boxloop
6730   IF key>4:EXIT boxloop
6740   nbox=0
6750   SELect ON box
6760    =8:nbox=10*(key=2)
6770    =9:nbox=8*(key=1)+10*(key=4)
6780    =10:nbox=8*(key=1)+9*(key=3)+11*(key=4)
6790    =11:nbox=8*(key=1)+12*(key=2)+10*(key=3)
6800    =12:nbox=11*(key=1)+10*(key=3)
6810   END SELect
6820   IF nbox:BORDER#box,1,menu_paper:box=nbox:BORDER#box,1,select_ink
6830  END REPeat boxloop
6840  RETurn box*(key<>7)
6850 END DEFine
6860 :
6870 DEFine PROCedure get_direc
6880  oops=0:file$=""
6890  Q_ERR_ON 'open','dir'
6900  OPEN#13,'pipe_8000'
6910  IF Q_ERR:oops=1:error_message 1:RETurn
6920  Q_PIPE#13 TO #6
6930  DIR#13,pname$:CLOSE#13:Q_ERR_OFF
6940  IF Q_ERR:CLOSE#6:oops=1:error_message 1:RETurn
6950  count=0
6960  INPUT#6,medium$\medium$
6970  INK#8,4:STRIP#8,0:CLS#8
6980  REPeat readback
6990   IF EOF(#6):EXIT readback
7000   count=count+1
7010   INPUT#6,d$(count)
7020   IF count<12
7030    IF LEN(d$(count))>26:PRINT#8,d$(count)(1 TO 26):ELSE PRINT#8,d$(count)
7040   END IF
7050  END REPeat readback
7060  CLOSE#6
7070  IF count>0:spos=1:dpos=1:file$=d$(1)
7080  AT#7,12,30:PRINT#7,CHR$(32+159*(count>11))
7090 END DEFine
7100 :
7110 DEFine FuNction scroll_select$
7120  IF count=0:RETurn ""
7130  INK#8,0:STRIP#8,4:AT#8,spos-1,0:sprint file$
7140  REPeat sloop
7150   key=INKEY$(#7,-1) INSTR fkey$
7160   IF key=0:NEXT sloop
7170   IF key=7 OR key=2:RETurn ""
7180   IF key=5 OR key=6:RETurn file$
7190   SELect ON key
7200    =3:
7210       IF spos<>1
7220        spos=spos-1:dpos=dpos-1
7230        file$=d$(dpos)
7240        STRIP#8,0:INK#8,4:AT#8,spos,0:sprint d$(dpos+1)
7250        STRIP#8,4:INK#8,0:AT#8,spos-1,0:sprint file$
7260       ELSE
7270        IF dpos<>1
7280         STRIP#8,0:INK#8,4:AT#8,0,0:sprint file$
7290         SCROLL#8,10:STRIP#8,4:INK#8,0:AT#8,0,0
7300         dpos=dpos-1:file$=d$(dpos):sprint file$
7310        END IF
7320       END IF
7330    =4:
7340       IF spos<11 AND dpos<>count
7350        spos=spos+1:dpos=dpos+1
7360        file$=d$(dpos)
7370        STRIP#8,0:INK#8,4:AT#8,spos-2,0:sprint d$(dpos-1)
7380        STRIP#8,4:INK#8,0:AT#8,spos-1,0:sprint file$
7390       ELSE
7400        IF dpos<count
7410         STRIP#8,0:INK#8,4:AT#8,10,0:sprint file$
7420         SCROLL#8,-10:STRIP#8,4:INK#8,0:AT#8,10,0
7430         dpos=dpos+1:file$=d$(dpos):sprint file$
7440        END IF
7450       END IF
7460   END SELect
7470   AT#7,2,30:PRINT#7,CHR$(32+158*(spos-dpos<0))
7480   AT#7,12,30:PRINT#7,CHR$(32+159*(count-dpos>11-spos))
7490  END REPeat sloop
7500 END DEFine
7510 :
7520 DEFine PROCedure sprint (zx$)
7530  LOCal temp$(30)
7540  temp$=zx$
7550  IF LEN(temp$)>26:temp$=temp$(1 TO 26)
7560  PRINT#8,temp$
7570 END DEFine
7580 :
7590 DEFine PROCedure init_winds
7600  PAPER#7,menu_paper:CLS#7:BORDER#7,1,2
7610  CSIZE#7,2,0:INK#7,7:PAPER#7,0:AT#7,0,0
7620  l1=31-LEN(heading$):l2=l1 DIV 2:l3=l1-l2:heading$=FILL$(" ";l2)&heading$&FILL$(" ";l3):PRINT#7,heading$
7630  CSIZE#7,1,0:PAPER#7,menu_paper:INK#7,menu_ink:AT#7,2,31:PRINT#7,"PATHNAME"
7640  AT#7,6,31:PRINT#7,"FILENAME"
7650  FOR r=8 TO 12:CLS#r:BORDER#r,1,menu_paper:INK#r,7
7660  CSIZE#11,1,0:CSIZE#12,1,0:CURSOR#11,5,1:CURSOR#12,5,1:PRINT#11,"ACCEPT":PRINT#12,"CANCEL"
7670  CSIZE#7,0,0
7680 END DEFine
7690 :
7700 DEFine PROCedure OVERWRITE
7710  carry_on=1
7720  WINDOW#5,110,22,200,80:PAPER#5,menu_paper:CLS#5:BORDER#5,1,2
7730  CSIZE#5,0,0:INK#5,menu_ink:PRINT#5," OK TO OVERWRITE    ( Y / N ) ?"
7740  REPeat end_loop:dummy$=INKEY$(-1):IF dummy$ INSTR 'NnYy':EXIT end_loop
7750  IF dummy$=='n':carry_on=0
7760  PAPER#5,0:BORDER#5,0
7770 END DEFine
7780 :
7790 DEFine PROCedure get_st_screen
7800  LOCal carry_on,temp$(52),x
7810  IF st_file$<>"NONE":OVERWRITE:IF carry_on=0:RETurn
7820  offset=0:defext$=stext$
7830  SELect ON transfer_type:=2,3,4:defext$="_pi"&(transfer_type-1)
7840  REPeat ST_LOAD_loop
7850   temp$=SELECT_FILE$("LOAD ST SCREEN IMAGE",defdev$&defext$,"",8)
7860   IF temp$="":RETurn
7870   Q_ERR_ON "open_in","close","gethead","lbytes"
7880   OPEN_IN#6,temp$
7890   Q_ERR_OFF "open_in","close"
7900   IF Q_ERR:CLOSE#6:error_message 1:NEXT ST_LOAD_loop
7910   GetHEAD #6,file_buf:x=PEEK_L(file_buf):Q_ERR_OFF "gethead"
7920   IF x<32034 OR x>32128 OR Q_ERR:CLOSE#6:error_message 1:NEXT ST_LOAD_loop
7930   z=CODE(INKEY$(#6)):z=CODE(INKEY$(#6)):CLOSE#6
7940   LBYTES temp$,st_screen
7950   Q_ERR_OFF
7960   IF Q_ERR:error_message 1:NEXT ST_LOAD_loop
7970   st_file$=temp$:file_size=x:IF x=32066:file_size=32034
7980   transfer_type=z+2:IF x=32128:transfer_type=1
7990   defdev$=find_dev$(st_file$)
8000   error_message 0:RETurn
8010  END REPeat ST_LOAD_loop
8020 END DEFine
8030 :
8040 DEFine PROCedure hires
8050  IF pic_addr<(st_screen+file_size-32000):pic_addr=st_screen+file_size-32000
8060 CONV_HI #1,pic_addr
8070 END DEFine
8080 :
8090 DEFine PROCedure medres
8100  qladdr=scr_wid*(INT(((st_screen+file_size-32000)-pic_addr)/160)-1)
8110  IF qladdr<0:qladdr=0
8120  REPeat lp:IF pic_addr>st_screen:EXIT lp:ELSE pic_addr=pic_addr+160
8130  CONV_MED #1,pic_addr,qladdr
8140 END DEFine
8150 :
8160 DEFine PROCedure lo_rez
8170  qladdr=scr_wid*(INT(((st_screen+file_size-32000)-pic_addr)/160)-1)
8180  IF qladdr<0:qladdr=0
8190  REPeat lp:IF pic_addr>st_screen:EXIT lp:ELSE pic_addr=pic_addr+160
8200  CONV_LOW #1,pic_addr,qladdr
8210 END DEFine
8220 :
8230 DEFine PROCedure convert_menu
8240 LOCal lpos,pos
8250  lpos=1:pos=1:defext$=stext$
8260  REPeat main_loop
8270  WINDOW #4,244,120,130,25
8280  SET_MODE 4:CLS
8290  PAPER#4,menu_paper:CLS#4:BORDER#4,1,2
8300  AT#4,0,0:STRIP#4,0:INK#4,7:CSIZE#4,1,0:PRINT#4,"        CONVERT  MENU         ":AT#4,7,0:PRINT#4,"        ST FILE STATUS        "
8310  items=4
8320  m$(1)="LOAD ST SCREEN"
8330  m$(2)="CONVERT SCREEN"
8340  m$(3)="IMAGE PROCESSING"
8350  m$(4)="QUIT PROGRAM"
8360  CSIZE#4,0,0:PAPER#4,menu_paper:INK#4,menu_ink:AT#4,9,2:PRINT#4,"ST FILE :- ";st_file$:AT#4,10,2:PRINT#4,"CONVERT :- ";
8370  SELect ON transfer_type
8380   =1:PRINT#4;"NEO -> MODE 8"
8390   =2:PRINT#4;"PI1 -> MODE 8"
8400   =3:PRINT#4;"PI2 -> MODE 4"
8410   =4:PRINT#4;"PI3 -> MODE 4"
8420  END SELect
8430  operate_menu pos,26,0
8440  PAPER#4,0:BORDER#4,0
8450  SELect ON selected
8460   =1:get_st_screen:pos=2
8470   =2:convert_screen:pos=3:IF leave=2:pos=1
8480   =3:stext$=defext$:return_to_convert=0
8490      SET_MODE ql:touch_up:pos=1:defext$=stext$
8500   =4:stop_prog
8510  END SELect
8520  lpos=pos
8530  END REPeat main_loop
8540 END DEFine
8550 :
8560 DEFine PROCedure convert_screen
8570 LOCal aloop,incr,yoff,xoff
8580  offset=0:x=70:y=60:xoff=0:yoff=0:incr=8:ymin=-14:ymax=0:xold=0:yold=0
8590  IF transfer_type=4:ymin=0:ymax=36:incr=4
8600  IF transfer_type=3:incr=4
8610  IF st_file$=="NONE" THEN
8620   WINDOW#5,150,22,180,100:PAPER#5,menu_paper:INK#5,menu_ink
8630   CLS#5:BORDER#5,1,2:CSIZE#5,0,0:leave=2
8640   PRINT#5,"   NO CURRENT ST FILE         PRESS ANY KEY"
8650   Waits
8660  ELSE
8670   SELect ON transfer_type:=1,2:ql=8:SET_MODE ql:=3,4:IF ql=8:ql=4:SET_MODE ql
8680   CLS:old_offset=offset
8690   REPeat aloop
8700    pic_addr=st_screen+file_size-32000+offset
8710    SELect ON transfer_type
8720     =1,2:lo_rez
8730     =3:medres
8740     =4:hires
8750    END SELect
8760    leave=0
8770    REPeat loopa
8780     a=CODE(INKEY$(-1))
8790     SELect ON a
8800      =192:xoff=xoff+incr:IF xoff>32:xoff=32
8810      =200:xoff=xoff-incr:IF xoff<0:xoff=0
8820      =216:yoff=yoff-1*(yoff>ymin)
8830      =208:yoff=yoff+1*(yoff<ymax)
8840      =27:leave=2
8850      =32,10:leave=1
8860     END SELect
8870     IF leave:EXIT aloop
8880     IF xold<>xoff OR yold<>yoff
8890      xold=xoff:yold=yoff:offset=yoff*4*160*(transfer_type<>4)+yoff*4*80*(transfer_type=4)+xoff*.5*(transfer_type=4)+xoff*(transfer_type<>4)
8900      EXIT loopa
8910     END IF
8920    END REPeat loopa
8930   END REPeat aloop
8940   IF leave=2:offset=old_offset:RETurn
8950   IF transfer_type=1 OR transfer_type=2:WINDOW 512,200,0,ABS(yoff*4)-2*(yoff<0):RECOL 0,2,1,3,4,6,5,7
8960   IF transfer_type=3:WINDOW 512,200,0,ABS(yoff*4)-2*(yoff<0):RECOL 7,7,2,2,4,4,0,0
8970   WINDOW 512,256,0,0
8980   IF transfer_type=4:RECOL 7,1,2,3,0,5,6,7
8990  SCR_CO SCREEN(#1) TO screen1
9000  END IF
9010 END DEFine
9020 :
9030 DEFine PROCedure stop_prog
9040  WINDOW#5,130,22,180,100:PAPER#5,menu_paper:INK#5,menu_ink
9050  CLS#5:BORDER#5,1,2:CSIZE#5,0,0
9060  PRINT#5,"DO YOU REALLY WANT TO  QUIT ( Y / N ) ?"
9070  REPeat end_loop:dummy$=INKEY$(-1):IF dummy$ INSTR 'yYnN':EXIT end_loop
9080  PAPER#5,0:BORDER#5,0
9090  IF dummy$=="y":CLS:STOP
9100 END DEFine
9110 :
9120 DEFine PROCedure set_ratio(pot)
9130  LOCal x,loop,leave
9140  comp_flag=0:leave=0:norm_ink=menu_ink:IF menu_ink=7:norm_ink=4
9150  WINDOW#5,160,42,150,100:PAPER#5,menu_paper:CLS#5:BORDER#5,1,2:CSIZE #5,0,0
9160  STRIP#5,0:INK#5,7
9170  IF pot>1:PRINT#5,"    SET EXPANSION RATIO   ":ELSE :PRINT#5,' SET SIDEWAYS COMPRESSION'
9180  exp1$=' [50%] ':exp2$='  [66%]'
9190  IF pot>1:exp1$='[200%] ':exp2$=' [150%]'
9200  STRIP#5,menu_paper:INK#5,menu_ink:CURSOR#5,10,15:PRINT#5,"RATIO : ";:INK#5,7:PRINT#5,exp1$;:INK#5,norm_ink:PRINT#5;exp2$
9210  STRIP#5,7:INK#5,2:AT#5,3,0:PRINT#5;" <¼ ½> & <SPACE> OR <ESC> "
9220  STRIP#5,menu_paper
9230  REPeat loop
9240   x=CODE(INKEY$(-1))
9250   SELect ON x
9260    =192:comp_flag=0:CURSOR#5,58,15:INK#5,7:PRINT#5;exp1$;
9270         INK#5,norm_ink:PRINT#5;exp2$
9280    =200:comp_flag=1:CURSOR#5,58,15:INK#5,norm_ink:PRINT#5;exp1$;
9290         INK#5,7:PRINT#5,exp2$
9300    =32,10:leave=1
9310    =27:leave=2:comp_flag=-1
9320   END SELect
9330   IF leave:EXIT loop
9340  END REPeat loop
9350 BORDER#5,0:PAPER#5,0
9360 END DEFine
9370 :
9380 DEFine PROCedure operate_menu(pot,offt,escape)
9390  LOCal pos,lpos,xoff
9400  selected=0:lpos=pot:pos=pot
9410  AT#4,0,0:STRIP#4,menu_paper:INK#4,menu_ink:CSIZE#4,1,0
9420  FOR r=1 TO items:WRITE_TEXT r
9430  STRIP #4,menu_ink:INK#4,menu_paper:WRITE_TEXT pos
9440  REPeat loopa
9450   key=CODE(INKEY$(-1))
9460   SELect ON key
9470    =CODE('0') TO CODE('9'):selected=CHR$(key)+1
9480    =CODE('a') TO CODE('z'):selected=key-CODE('a')+11
9490    =CODE('A') TO CODE('Z'):selected=key-CODE('A')+11
9500    =208:pos=pos-1:IF pos=0:pos=1
9510    =216:pos=pos+1*(pos<items)
9520    =32,10:selected=pos
9530    =27:IF escape:leave=99:EXIT loopa
9540   END SELect
9550   IF selected:EXIT loopa
9560   IF pos<>lpos
9570    STRIP#4,menu_paper:INK#4,menu_ink:WRITE_TEXT lpos
9580    STRIP#4,menu_ink:INK#4,menu_paper:WRITE_TEXT pos
9590    lpos=pos
9600   END IF
9610  END REPeat loopa
9620 STRIP#4,menu_paper:INK#4,menu_ink
9630 END DEFine
9640 :
9650 DEFine PROCedure help_screen
9660  SCR_CO SCREEN(#1) TO screen1
9670  WINDOW#5,225*ql/4+ql/2,170+ql/2,32+120*(ql=4),30:PAPER#5,menu_paper:INK#5,menu_ink
9680  CLS#5:BORDER#5,ql/4,2:CSIZE#5,0,0
9690  AT#5,0,0:PAPER#5,0:INK#5,7:CLS#5,3:PRINT#5,"   HELP       [IMAGE PROCESSING]"
9700  PAPER#5,7:INK#5,2:AT#5,16,0:CLS#5,3:PRINT#5,"      PRESS ANY KEY TO CONTINUE"
9710  PAPER#5,menu_paper:INK#5,menu_ink:AT#5,2,0
9720  PRINT#5,"<F1>       : This help screen"
9730  PRINT#5,"<F2>       : Image processing menu"
9740  PRINT#5,"<F3>       : Screen manipulation"
9750  PRINT#5,"<CTRL>-<f> : Fill area"
9760  PRINT#5,"<CTRL>-<s> : Save screen"
9770  PRINT#5,"<CTRL>-<l> : Load screen"
9780  PRINT#5,"<CTRL>-<S> : Save screen to buffer"
9790  PRINT#5,"<CTRL>-<L> : Load screen from buffer"
9800  PRINT#5,"<CTRL>-<i> : Ink/paper colour menu"
9810  PRINT#5,"<CTRL>-<r> : Recolour menu"
9820  PRINT#5,"<CTRL>-<b> : Cursor menu"
9830  PRINT#5,"<CTRL>-<x> : Screen conversion menu"
9840  PRINT#5,"<CTRL>-<q> : Quit"
9850  Waits
9860  SCR_CO screen1 TO SCREEN(#1)
9870 END DEFine
9880 :
9890 DEFine PROCedure SET_MODE(alp)
9900 PAPER 0:PAPER#3,0
9910 IF minerva%=0
9920 IF RMODE<>alp:MODE alp
9930 ELSE
9940 alpa=0:IF alp=8:alpa=1
9950 IF RMODE<>alp:MODE 16512+alpa*8+(16128-256*8),-1
9960 END IF
9970 END DEFine
9980 :
9990 DEFine PROCedure Waits
10000 dummy$=INKEY$(-1)
10010 PAPER#5,0:BORDER#5,0
10020 END DEFine
10030 :
10040 DEFine PROCedure WRITE_TEXT(pot)
10050 AT#4,pot+1,1+(offt-LEN(m$(pot))) DIV 2
10060 pest$=pot-1:IF pot>10:pest$=CHR$(CODE('A')+pot-11)
10070 UNDER#4,1:PRINT#4,pest$;:UNDER#4,0
10080 PRINT#4,!m$(pot)
10090 END DEFine
10100 :
10110 DEFine PROCedure DRAW_BLOK(xa,xb,xc,xd,xe)
10120 IF minerva%=1:BLOCK xa,xb,xc,xd,xe:RETurn
10130 SELect ON xa
10140  =1 TO 511:BLOCK xa,xb,xc,xd,xe
10150  =512:BLOCK 256,xb,xc,xd,xe:BLOCK 256,xb,xc+256,xd,xe
10160 END SELect
10170 END DEFine
