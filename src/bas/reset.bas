ON ERROR SOUND1,-15,150,4:PRINT TAB(0,19);:REPORT:PRINT'''"Press any key";:K%=GET:RUN

CLS:VDU 23;6,25,0;0;0;23;1,40,0;0;0;

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)

PRINT TAB(1,4);CHR$(135);"Reset";
IF G% THEN PRINT CHR$(133);"High Score Challenge"; ELSE PRINT CHR$(129);"Arcade";
PRINT CHR$(135);"Settings";

HIMEM=&7A80
D%=HIMEM+&80
H%=D%+&80
S%=&8010
F%=&130
M%=&69

IF B% THEN U$="WR":L$="LR" ELSE U$="":L$="L"

IF G% THEN file$="_Hsc " ELSE file$="_Config "

OSCLI("LOAD _Reset " + STR$~(&FF0000 + D%))

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
IF F%?2 = V% THEN CALL HIMEM ELSE OSCLI("LOAD " + file$ + STR$~(&FF0000 + H%))

V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%
IF V% <> H%?&7D THEN OSCLI("LOAD " + file$ + STR$~(&FF0000 + H%))

PRINT TAB(1,7);CHR$(133);"Do you wish to reset the high score";TAB(1,8);CHR$(133);"table to default Y/N";CHR$(135);"?";CHR$(131);:INPUT "" H$:IF H$="y" THEN H$="Y"

PRINT TAB(1,10);CHR$(130);"Do you wish to reset the controls";
IF NOT G% PRINT " and";TAB(1,11);CHR$(130);"game settings"; ELSE PRINT TAB(1,11);
PRINT CHR$(130);"to default Y/N";CHR$(135);"?";CHR$(131);:INPUT "" S$:IF S$="y" THEN S$="Y"

M$="":IF NOT G% THEN PRINT TAB(1,13);CHR$(132);"Do you wish to reset the maps";TAB(1,14);CHR$(132);"to default Y/N";CHR$(135);"?";CHR$(131);:INPUT "" M$:IF M$="y" THEN M$="Y"

PRINT TAB(0,16);

IF H$="Y" THEN PRINT " ";CHR$(129);"Resetting";CHR$(133);"high scores":FOR Z%=&00 TO &6F:Z%?H%=Z%?D%:NEXT Z%

IF S$="Y" THEN PRINT " ";CHR$(129);"Resetting";CHR$(130);"controls";:IF NOT G% THEN PRINT " and settings";
IF S$="Y" THEN PRINT:FOR Z%=&70 TO &7C:Z%?H%=Z%?D%:NEXT Z%

IF NOT G% THEN IF M$="Y" THEN PRINT " ";CHR$(129);"Resetting";CHR$(132);"maps":OSCLI("ACCESS _Maps " + U$):Z%=OPENOUT("_Maps"):PRINT#Z%,"_Map1","_Map2","_Map3":CLOSE#Z%:OSCLI("ACCESS _Maps " + L$)

IF H$="Y" OR S$="Y" THEN V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%:H%?&7D=V%:OSCLI("ACCESS " + file$ + U$):OSCLI("SAVE " + file$ + STR$~(&FF0000 + H%) + " +7E FFFFFF 0"):OSCLI("ACCESS " +file$ + L$):!F%=0

PRINT:PRINT " ";CHR$(131);

IF M$="Y" OR H$="Y" OR S$="Y" THEN PRINT "Done" ELSE PRINT "No changes were made"

PRINT TAB(2,23);"Press any key";:K%=GET:PRINT

FOR R%=24 TO 2 STEP -2
*FX 19
VDU 23;6,R%,0;0;0;
NEXT R%
*FX 19
VDU 23;1,0,0;0;0;

*EXEC !Boot

END

