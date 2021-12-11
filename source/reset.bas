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



ON ERROR PRINT TAB(0,20);:REPORT:PRINT:PRINT:CLOSE#0:END

MODE 7

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)

PRINT TAB(12,4);CHR$(135);"Reset Settings"

HIMEM=&7A80
D%=HIMEM+&80
H%=D%+&80
S%=&8010
F%=&130
M%=&69

OSCLI("LOAD D.Config " + STR$~(&FF0000 + D%))

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
IF F%?2 = V% THEN CALL HIMEM ELSE OSCLI("LOAD $.Config")

V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%
IF V% <> H%?&7D THEN OSCLI("LOAD $.Config")

PRINT TAB(1,7);CHR$(132);"Do you wish to reset the maps";
PRINT TAB(1,8);CHR$(132);"to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" M$

PRINT TAB(1,10);CHR$(133);"Do you wish to reset the high score";
PRINT TAB(1,11);CHR$(133);"table to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" H$
IF H$="Y" THEN FOR Z%=&00 TO &6F:Z%?H%=Z%?D%:NEXT Z%

PRINT TAB(1,13);CHR$(130);"Do you wish to reset the game";
PRINT TAB(1,14);CHR$(130);"settings to default Y/N";CHR$(135);"?";CHR$(131);
INPUT "" S$
IF S$="Y" THEN FOR Z%=&70 TO &7C:Z%?H%=Z%?D%:NEXT Z%

PRINT TAB(0,16);

IF M$="Y" THEN PRINT " ";CHR$(129);"Saving maps":Z%=OPENOUT("$.Maps"):PRINT#Z%,"D.Maze1","D.Maze2","D.Maze3":CLOSE#Z%

IF H$="Y" THEN PRINT " ";CHR$(129);"Saving high scores"
IF S$="Y" THEN PRINT " ";CHR$(129);"Saving settings"

IF H$="Y" OR S$="Y" THEN V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%:H%?&7D=V%:OSCLI("SAVE $.Config " + STR$~(&FF0000 + H%) + " +7E"):F%?0=0:F%?1=0:F%?2=0

PRINT:PRINT " ";CHR$(132);"Done"

END
