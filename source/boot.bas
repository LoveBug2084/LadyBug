REM ---------------------------
REM Lady Bug arcade style
REM video game for the
REM BBC Computer range
REM based on the original
REM 1981 arcade game
REM by Universal
REM ---------------------------
REM Copyright (C) 2021 LoveBug
REM https://github.com/LoveBug2084/LadyBug
REM ---------------------------
REM This program is free
REM software: you can
REM redistribute it and/or
REM modify it under the terms
REM of the GNU General Public
REM License as published by
REM the Free Software Foundation,
REM either version 3 of the
REM License, or (at your option)
REM any later version.
REM ---------------------------
REM This program is distributed
REM in the hope that it will be
REM useful, but WITHOUT ANY
REM WARRANTY; without even the
REM implied warranty of
REM MERCHANTABILITY or FITNESS
REM FOR A PARTICULAR PURPOSE.
REM See the GNU General Public
REM License for more details.
REM https://www.gnu.org/licenses/
REM ---------------------------



HIMEM=&7800

C%=&7B80
S%=&8010
E%=PAGE+5
F%=&130
M%=&69

ON ERROR PROCerror

PROCreadConfig

PROCintro

REPEAT

PROCshrink
PROCoptions
PROCexpand

k$=FNwaitKey

PROCshrink

IF k$="I" THEN PROCinstructionsGame

IF k$="E" THEN CHAIN"Editor"

IF k$="W" THEN PROCinstructionsEditor

IF k$="R" THEN CHAIN"Reset"

UNTIL k$=CHR$(13)

PROClogo
PROCexpand

Z%=OPENIN("E.Maps")
INPUT#Z%,maze1$,maze2$,maze3$
CLOSE#Z%

OSCLI("LOAD "+maze1$+" 7800")
OSCLI("LOAD "+maze2$+" 7900")
OSCLI("LOAD "+maze3$+" 7A00")
*/Loader

END



DEF PROCintro

IF ?E%<>32 THEN PROCwaitReturn(400):ENDPROC

PROClogo
PROCexpand

PROCsaveConfig

PROCwaitReturn(200)

ENDPROC



DEF PROClogo

RESTORE

FOR Z%=&7C00 TO &7FE7 STEP 4
READ D%
!Z%=D%
NEXT Z%

DATA &2020919A,&20207D6A,&20202020,&20202020,&20202020,&377E2020,&20203029,&60202020,&20202020,&9C9C2020
DATA &2020919A,&20207F6A,&302C3C78,&7C20307C,&643C7834,&357F2020,&74782560,&7E2C6020,&643C7824,&9C9C2020
DATA &2020919A,&20207F6A,&3520357F,&3A307F2B,&78757F21,&377F2021,&7F2A342B,&7F203674,&78757F20,&9C9C2021
DATA &2020919A,&20207F6A,&3520357F,&217F6B20,&7C203520,&357F2030,&2A202560,&7F202037,&7C203520,&9C9C2030
DATA &2020919A,&219E2322,&20922322,&20219120,&92232220,&23229120,&3E782021,&23202020,&92232220,&9C9C2020
DATA &2020929A,&347F2368,&20202020,&207D2B36,&20202060,&20202020,&20239120,&20202020,&20202020,&9C9C2020
DATA &2020929A,&253A746A,&302C3C78,&28203C7D,&307C247E,&78203478,&2C3C7034,&2C3C7830,&643C7830,&9C9C2020
DATA &2020929A,&346F3B60,&3520357F,&2020357F,&7F2B207F,&7A747F32,&70712321,&60357F35,&78757F35,&9C9C2021
DATA &2020929A,&35207F6A,&3520357F,&2020357F,&6B20207F,&217F2B3F,&7F203720,&2B357F35,&7C203521,&9C9C2030
DATA &2020929A,&209E2320,&20232322,&20202023,&20202023,&20212021,&23232220,&20212320,&91232220,&9C9C2020
DATA &2020949A,&20207D6A,&20202020,&20202020,&20202020,&377E2020,&20203029,&60202020,&20202020,&9C9C2020
DATA &2020949A,&20207F6A,&302C3C78,&7C20307C,&643C7834,&357F2020,&74782560,&7E2C6020,&643C7824,&9C9C2020
DATA &2020949A,&20207F6A,&3520357F,&3A307F2B,&78757F21,&377F2021,&7F2A342B,&7F203674,&78757F20,&9C9C2021
DATA &2020949A,&20207F6A,&3520357F,&217F6B20,&7C203520,&357F2030,&2A202560,&7F202037,&7C203520,&9C9C2030
DATA &2020949A,&219E2322,&20912322,&20219420,&91232220,&23229420,&3E782021,&23202020,&91232220,&9C9C2020
DATA &2020959A,&347F2368,&20202020,&207D2B36,&20202060,&20202020,&20239420,&20202020,&20202020,&9C9C2020
DATA &2020959A,&253A746A,&302C3C78,&28203C7D,&307C247E,&78203478,&2C3C7034,&2C3C7830,&643C7830,&9C9C2020
DATA &2020959A,&346F3B60,&3520357F,&2020357F,&7F2B207F,&7A747F32,&70712321,&60357F35,&78757F35,&9C9C2021
DATA &2020959A,&35207F6A,&3520357F,&2020357F,&6B20207F,&217F2B3F,&7F203720,&2B357F35,&7C203521,&9C9C2030
DATA &2020959A,&209E2320,&20232322,&20202023,&20202023,&20212021,&23232220,&20212320,&91232220,&9C9C2020
DATA &2020939A,&20207D6A,&20202020,&20202020,&20202020,&377E2020,&20203029,&60202020,&20202020,&9C9C2020
DATA &2020939A,&20207F6A,&302C3C78,&7C20307C,&643C7834,&357F2020,&74782560,&7E2C6020,&643C7824,&9C9C2020
DATA &2020939A,&20207F6A,&3520357F,&3A307F2B,&78757F21,&377F2021,&7F2A342B,&7F203674,&78757F20,&9C9C2021
DATA &2020939A,&20207F6A,&3520357F,&217F6B20,&7C203520,&357F2030,&2A202560,&7F202037,&7C203520,&9C9C2030
DATA &2020939A,&219E2322,&20912322,&20219320,&91232220,&23229320,&3E782021,&23202020,&91232220,&9C9C9420

ENDPROC



DEF PROCoptions

CLS

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);



PRINT TAB(7,3);CHR$(131);"A remake of the original";
PRINT TAB(6,4);CHR$(129);"Universal 1981 arcade game";

PRINT TAB(7,7);"Programmed by";CHR$(133);"LoveBug";CHR$(135);"2021";



PRINT TAB(16,10);"Options";

PRINT TAB(4,13);CHR$(130);"Press";CHR$(133);"Return";CHR$(130);"to play";CHR$(129);"Lady Bug";

PRINT TAB(4,15);CHR$(131);"Press";CHR$(132);"I";CHR$(131);"for game instructions";

PRINT TAB(4,17);CHR$(129);"Press";CHR$(134);"E";CHR$(129);"to edit the game maps";

PRINT TAB(4,19);CHR$(132);"Press";CHR$(131);"W";CHR$(132);"for edit instructions";

PRINT TAB(4,21);CHR$(133);"Press";CHR$(130);"R";CHR$(133);"to reset the settings";


PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"        Choose an option          ";CHR$(156);

ENDPROC



DEF PROCinstructionsGame

CLS

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);

PRINT TAB(13,3);CHR$(135);"Instructions";TAB(34,3);CHR$(131);"1/2";

PRINT TAB(2,5);CHR$(130);"Guide";CHR$(129);"Lady Bug";CHR$(130);"through the maze";
PRINT TAB(2,6);CHR$(130);"avoiding deadly enemies and skulls";

PRINT TAB(2,8);CHR$(132);"Push the";CHR$(130);"green turnstiles";CHR$(132);"to block";
PRINT TAB(2,9);CHR$(132);"the enemy attack paths";

PRINT TAB(2,11);CHR$(133);"Collect";CHR$(134);"cyan hearts";CHR$(133);"to multiply";
PRINT TAB(2,12);CHR$(133);"an items score value by";CHR$(134);"x2 x3 x5";

PRINT TAB(2,14);CHR$(129);"Collect";CHR$(131);"yellow";CHR$(129);"letters to spell";
PRINT TAB(2,15);CHR$(131);"EXTRA";CHR$(129);"for 2 extra lives";

PRINT TAB(2,17);CHR$(134);"Collect";CHR$(129);"red";CHR$(134);"letters to spell";
PRINT TAB(2,18);CHR$(129);"SPECIAL";CHR$(134);"for 200000 points plus a";
PRINT TAB(2,19);CHR$(134);"skull shield lasting 6 rounds";

PRINT TAB(2,21);CHR$(132);"Collect";CHR$(130);"vegetables";CHR$(132);"to paralyse the";
PRINT TAB(2,22);CHR$(132);"enemy and earn bonus points";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"          Press Return            ";CHR$(156);

PROCexpand
PROCwaitReturn(360000)

L%=0
FOR R%=22 TO 5 STEP -1
L%=L%+1
IF L%>2 THEN L%=L%-3:*FX 19
PRINT TAB(0,R%);SPC(40);
NEXT R%

PRINT TAB(34,3);CHR$(131);"2";:*FX 19

PRINT TAB(2,5);CHR$(133);"A special diamond bonus worth";:*FX 19
PRINT TAB(2,6);CHR$(131);"1 million points";CHR$(133);"will be awarded if";:*FX 19
PRINT TAB(2,7);CHR$(133);"you can reach";CHR$(131);"level 6";CHR$(133);"while only";:*FX 19
PRINT TAB(2,8);CHR$(133);"collecting";CHR$(134);"cyan hearts and letters";:*FX 19
PRINT TAB(2,9);CHR$(133);"and";CHR$(131);"without losing a life";:*FX 19

PRINT TAB(2,11);CHR$(129);"Use the";CHR$(131);": UP";CHR$(129);"and";CHR$(131);"/ DOWN";CHR$(129);"keys to";:*FX 19
PRINT TAB(2,12);CHR$(129);"navigate the menu and";CHR$(131);"RETURN";CHR$(129);"to";:*FX 19
PRINT TAB(2,13);CHR$(129);"adjust the game settings or";:*FX 19
PRINT TAB(2,14);CHR$(129);"redefine the keys";:*FX 19

PRINT TAB(2,16);CHR$(131);"During the game press";CHR$(129);"RETURN";CHR$(131);"to":*FX 19
PRINT TAB(2,17);CHR$(131);"pause, move";CHR$(129);"Lady Bug";CHR$(131);"to unpause";:*FX 19

PRINT TAB(2,19);CHR$(133);"Hold";CHR$(135);"ESC";CHR$(133);"to quit the current game";:*FX 19

PRINT TAB(2,21);CHR$(132);"Reboot the disk to save your";:*FX 19
PRINT TAB(2,22);CHR$(134);"high scores";CHR$(132);"and";CHR$(134);"game settings";

PROCwaitReturn(360000)

ENDPROC



DEF PROCinstructionsEditor

CLS

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);


PRINT TAB(14,4);"Editor controls";


PRINT TAB(6, 7);CHR$(135);"123";CHR$(129);"   Select map";

PRINT TAB(6, 9);CHR$(135);":/ZX";CHR$(130);"  Move cursor";
PRINT TAB(6,10);CHR$(135);"QW";CHR$(131);"    Select tile";
PRINT TAB(6,11);CHR$(135);"Shift";CHR$(133);" Erase tile";
PRINT TAB(6,12);CHR$(135);"Return";CHR$(132);"Draw tile";

PRINT TAB(6,14);CHR$(135);"E";CHR$(134);"     Erase map";

PRINT TAB(6,16);CHR$(135);"L";CHR$(129);"     Load map from disk";
PRINT TAB(6,17);CHR$(135);"S";CHR$(130);"     Save map to disk";

PRINT TAB(6,19);CHR$(135);"C";CHR$(131);"     Catalogue disk";

PRINT TAB(6,21);CHR$(135);"B";CHR$(133);"     Boot disk";


PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"          Press Return            ";CHR$(156);

PROCexpand
PROCwaitReturn(360000)

ENDPROC



DEF PROCshrink

FOR R%=24 TO 0 STEP -2
*FX 19
VDU 23,0,6,R%,0,0,0,0,0,0
NEXT R%
*FX 19

ENDPROC



DEF PROCexpand

FOR R%=1 TO 25 STEP 2
*FX 19
VDU 23,0,6,R%,0,0,0,0,0,0
NEXT R%
*FX 19

ENDPROC



DEF PROCerror

?E%=33

PROCeraseCenter

IF ERR<>201 THEN PROCunexpectedError

PRINT TAB(4,11);CHR$(136);CHR$(129);"Write protected";CHR$(132);"unable to save";
PRINT TAB(5,13);CHR$(136);CHR$(135);"high scores";CHR$(132);"or";CHR$(135);"game settings";

ENDPROC



DEF PROCunexpectedError

CLS

PRINT "An unexpected error has occurred"
REPORT
PRINT " at line ";ERL
PRINT

VDU 23,1,1,0,0,0,0,0,0,0
VDU 23,0,6,25,0,0,0,0,0,0

*FX 4
*FX 200

END

ENDPROC



DEF PROCreadConfig

V%=((F%?0 EOR M%) + (F%?1 EOR M%)) AND &FF 

IF F%?2<>V% THEN FOR Z%=&00 TO &7D:Z%?C%=0:NEXT Z%:ENDPROC

P%=&7B00
[OPT 0
SEI
LDA F%
STA &FE30
LDX #0
.LOOP
LDA S%, X
STA C%, X
INX
CPX #&7E
BNE LOOP
LDA &F4
STA &FE30
CLI
RTS
]

CALL &7B00

ENDPROC



DEF PROCsaveConfig

V%=0
FOR Z%=&00 TO &7C
V%=(V%+(Z%?C% EOR M%)) AND &FF
NEXT Z%

IF V%<>C%?&7D THEN OSCLI("LOAD Config"):ENDPROC

OSCLI("SAVE Config " + STR$~(&FF0000 + C%) + " +7E")

PROCeraseCenter
PRINT TAB(5,11);CHR$(135);"High scores";CHR$(132);"and";CHR$(135);"game settings";
PRINT TAB(11,13);CHR$(132);"saved successfully";

ENDPROC



DEF PROCeraseCenter

*FX 19
PRINT TAB(26,15);" ";TAB(25,15);" ";

FOR R%=14 TO 10 STEP -1
*FX 19
PRINT TAB(2,R%);SPC(38);
NEXT R%

*FX 19
FOR R%=10 TO 14
PRINT TAB(0,R%);"  ";
NEXT R%

ENDPROC


DEF PROCwaitReturn(T%)

TIME=0

REPEAT
K%=INKEY0
UNTIL K%=13 OR TIME>=T%

ENDPROC



DEF FNwaitKey

REPEAT
k$=INKEY$0
UNTIL k$=CHR$(13) OR k$="I" OR k$="E" OR k$="W" OR k$="R"

IF k$<>"" THEN =k$

=CHR$(13)
