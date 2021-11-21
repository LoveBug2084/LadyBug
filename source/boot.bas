REM ---------------------------
REM Lady Bug arcade style
REM video game for the
REM BBC Computer range
REM based on the original
REM arcade game by
REM universal 1981
REM ---------------------------
REM Copyright (C) 2021
REM LoveBug https://lovebug.ml
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

HIMEM=&7B00
H%=HIMEM+&80
S%=&8010
E%=PAGE+5
F%=&130
M%=&69

ON ERROR PROCerror

PROCreadConfig

PROCintro
PROCshrink

PROCinstructionsPage1
PROCexpand
REPEAT K%=GET:UNTIL K%=13
PROCshrink

PROCinstructionsPage2
PROCexpand
REPEAT K%=GET:UNTIL K%=13
PROCshrink

PROClogo
PROCexpand
OSCLI("/Loader")
END

DEF PROCintro
IF ?E%<>32 THEN TIME=0:REPEAT K%=INKEY(0):UNTIL K%=13 OR TIME >=300:ENDPROC
PROClogo
PROCexpand
PROCsaveConfig
TIME=0:REPEAT K%=INKEY(0):UNTIL K%=13 OR TIME >=200
ENDPROC

DEF PROClogo
RESTORE
FOR Z%=&7C00 TO &7FE7 STEP 4:READ D%:!Z%=D%:NEXT Z%
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

DEF PROCinstructionsPage1
CLS
PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);

PRINT TAB(11,3);CHR$(135);"Instructions";CHR$(131);"1/2";

PRINT TAB(2,5);CHR$(130);"Guide";CHR$(129);"Lady Bug";CHR$(130);"through the maze";
PRINT TAB(2,6);CHR$(130);"avoiding deadly enemies and skulls";

PRINT TAB(2,8);CHR$(132);"Push the";CHR$(130);"green turnstiles";CHR$(132);"to block";
PRINT TAB(2,9);CHR$(132);"the enemy attack paths";

PRINT TAB(2,11);CHR$(133);"Collect";CHR$(134);"cyan hearts";CHR$(133);"to multiply";
PRINT TAB(2,12);CHR$(133);"an items score value by x2 x3 x5";

PRINT TAB(2,14);CHR$(129);"Collect";CHR$(131);"yellow letters";CHR$(129);"spelling";
PRINT TAB(2,15);CHR$(131);"""EXTRA""";CHR$(129);"for 2 extra lives";

PRINT TAB(2,17);CHR$(134);"Collect";CHR$(129);"red letters";CHR$(134);"spelling";
PRINT TAB(2,18);CHR$(129);"""SPECIAL""";CHR$(134);"for 200000 points and a";
PRINT TAB(2,19);CHR$(134);"skull shield lasting 6 rounds";

PRINT TAB(2,21);CHR$(132);"Collect";CHR$(130);"vegetables";CHR$(132);"to paralyse the";
PRINT TAB(2,22);CHR$(132);"enemy and earn bonus points";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"           Press Return           ";CHR$(156);
ENDPROC

DEF PROCinstructionsPage2
CLS
PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);

PRINT TAB(11,3);CHR$(135);"Instructions";CHR$(131);"2/2";

PRINT TAB(2,5);CHR$(130);"A special diamond bonus worth";
PRINT TAB(2,6);CHR$(135);"1000000 points";CHR$(130);"will be";
PRINT TAB(2,7);CHR$(130);"awarded if you can reach";CHR$(133);"level 6";
PRINT TAB(2,8);CHR$(130);"without losing a life and only";
PRINT TAB(2,9);CHR$(130);"collecting";CHR$(134);"cyan hearts";CHR$(130);"and";CHR$(134);"letters";

PRINT TAB(2,11);CHR$(129);"Use the";CHR$(131);"up";CHR$(129);"/";CHR$(131);"down";CHR$(129);"controls to";
PRINT TAB(2,12);CHR$(129);"navigate the menu and";CHR$(131);"return";CHR$(129);"to";
PRINT TAB(2,13);CHR$(129);"adjust the game settings or";
PRINT TAB(2,14);CHR$(129);"redefine the player controls";

PRINT TAB(2,16);CHR$(134);"During the game press";CHR$(129);"return";CHR$(134);"to"
PRINT TAB(2,17);CHR$(134);"pause, move Lady Bug to unpause";

PRINT TAB(2,19);CHR$(131);"Hold";CHR$(129);"esc";CHR$(131);"to quit back to the menu";

PRINT TAB(2,21);CHR$(133);"Rebooting the disk will save your";
PRINT TAB(2,22);CHR$(133);"high scores and game settings";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"           Press Return           ";CHR$(156);
ENDPROC

DEF PROCshrink
A%=19:FOR R%=24 TO 0 STEP -1:CALL &FFF4:VDU 23,0,6,R%,0,0,0,0,0,0:NEXT R%:CALL &FFF4
ENDPROC

DEF PROCexpand
A%=19:FOR R%=1 TO 25:CALL &FFF4:VDU 23,0,6,R%,0,0,0,0,0,0:NEXT R%:CALL &FFF4
ENDPROC

DEF PROCerror
?E%=33
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
IF ERR<>201 THEN PROCunexpectedError
PRINT TAB(8,11);CHR$(129);"Disk is write protected!";
PRINT TAB(3,13);CHR$(132);"Unable to save";CHR$(135);"Scores";CHR$(132);"and";CHR$(135);"Settings";
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
END
ENDPROC

DEF PROCreadConfig
V%=((F%?0 EOR M%) + (F%?1 EOR M%)) AND 255 
IF F%?2 <> V% THEN FOR Z%=&00 TO &7D:Z%?H%=0:NEXT Z%:ENDPROC
P%=HIMEM
[OPT 0
SEI
LDA F%
STA &FE30
LDX #0
.LOOP
LDA S%, X
STA H%, X
INX
CPX #&7E
BNE LOOP
LDA &F4
STA &FE30
CLI
RTS
]
CALL HIMEM
ENDPROC

DEF PROCsaveConfig
V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND 255:NEXT Z%
IF V%=H%?&7D THEN OSCLI("SAVE Config " + STR$~(&FF0000 + H%) + " +7E") ELSE OSCLI("LOAD Config")
ENDPROC
