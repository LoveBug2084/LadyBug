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



ON ERROR PRINT TAB(0,21);:REPORT:PRINT'"Press any key";:K%=GET:RUN

MODE 7:VDU23,1,0;0;0;0;

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)

PRINT TAB(12,4);CHR$(135);"Reset Settings"

HIMEM=&7A80
D%=HIMEM+&80
H%=D%+&80
S%=&8010
F%=&130
M%=&69

OSCLI("LOAD _ConfR " + STR$~(&FF0000 + D%))

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

V%=((F%?0 EOR M%) + (F%?1 EOR M%)) AND &FF 
IF F%?2 = V% THEN CALL HIMEM ELSE OSCLI("LOAD _Conf")

V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%
IF V% <> H%?&7D THEN OSCLI("LOAD _Conf")

PRINT TAB(1,7);CHR$(132);"Do you wish to reset the maps";
PRINT TAB(1,8);CHR$(132);"to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" M$:IF M$="y" THEN M$="Y"

PRINT TAB(1,10);CHR$(133);"Do you wish to reset the high score";
PRINT TAB(1,11);CHR$(133);"table to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" H$:IF H$="y" THEN H$="Y"

PRINT TAB(1,13);CHR$(130);"Do you wish to reset the controls and";
PRINT TAB(1,14);CHR$(130);"game settings to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" S$:IF S$="y" THEN S$="Y"

PRINT TAB(0,16);

IF M$="Y" THEN PRINT " ";CHR$(129);"Resetting maps":OSCLI("ACCESS [Maps]"):Z%=OPENOUT("[Maps]"):PRINT#Z%,"_Maze1","_Maze2","_Maze3":CLOSE#Z%:OSCLI("ACCESS [Maps] L")

IF H$="Y" THEN FOR Z%=&00 TO &6F:Z%?H%=Z%?D%:NEXT Z%:PRINT " ";CHR$(129);"Resetting high scores"
IF S$="Y" THEN FOR Z%=&70 TO &7C:Z%?H%=Z%?D%:NEXT Z%:PRINT " ";CHR$(129);"Resetting controls and settings"

IF H$="Y" OR S$="Y" THEN V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%:H%?&7D=V%:OSCLI("ACCESS _Conf"):OSCLI("SAVE _Conf " + STR$~(&FF0000 + H%) + " +7E")::OSCLI("ACCESS _Conf L"):F%?0=0:F%?1=0:F%?2=0

PRINT:PRINT " ";CHR$(132);

IF M$="Y" OR H$="Y" OR S$="Y" THEN PRINT "Done" ELSE PRINT "No changes were made"

PRINT TAB(2,23);"Press any key";:K%=GET:PRINT

*EXEC !Boot

END
