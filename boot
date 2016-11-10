1 REMark SToQL Boot Program Version 2.02
90 REMark The next line contains the device to run SToQL from
100 m$='flp1_'
110 :
120 REMark ST-QL SCREEN CONVERSION PROGRAM - Version 1.30
130 REMark 1991-99 Alan Pemberton & Rich Mellor
140 REMark
150 :
160 REMark If you have lightning or speedscreen, then delete the REMark              from the appropriate line
170 REMark ******* WARNING: make sure that the space set aside with RESPR
180 REMark                  is large enough for your version of Lightning
190 REMark                  Why not use Toolkit II's LRESPR instead?
200 :
210 REMark If you have Toolkit II, then delete the REMark from line 220
220 REMark TK2_EXT
230 :
240 REMark _LNGinit
250 REMark a=RESPR(22360):LBYTES FLP1_LNG_TEXT_EXT,a:CALL a
260 REMark a=RESPR(4316):LBYTES FLP1_LNG_GRAF_EXT,a:CALL a
270 REMark a=RESPR(6938):LBYTES FLP1_LNG_MATH_EXT,a:CALL a
280 REMark _SPEED 1
290 :
300 REMark If you have the new ATR device drivers, delete the REMark             from line 310
310 REMark ATR_DEV
320 :
330 WINDOW 512,256,0,0:PAPER 0:MODE 4
340 INK#0,7
410 WINDOW 448,200,32,16
420 REMark If you want to run SToQL on Minerva's second screen
430 REMark then ensure Minerva is in dual screen mode (press F4 on
440 REMark start-up) and remove the REMark from line 450
450 REMark MODE 64+32,-1
460 REMark Leave space between lines 500 and 1000
999 REMark THIS MUST BE LINE 999
1000 EXEC_W m$&'SToQL_rtm'
1010 :
1020 REMark If you have the Pointer Environment installed, alter line 1000 to read:   1000 EXEC m$&'SToQL_rtm'
