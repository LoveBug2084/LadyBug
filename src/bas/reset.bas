ON ERROR SOUND1,-15,150,4:PRINT TAB(0,19);:REPORT:PRINT'''"Press any key";:K%=GET:RUN

CLS:VDU 23;6,25,0;0;0;23;1,40,0;0;0;

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);SPC(12);"Lady Bug";SPC(14);CHR$(156)

PRINT TAB(12,4);CHR$(135);"Reset Settings"

HIMEM=&7A80
D%=HIMEM+&80
H%=D%+&80
S%=&8010
F%=&130
M%=&69

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
IF F%?2 = V% THEN CALL HIMEM ELSE OSCLI("LOAD _Config " + STR$~(&FF0000 + H%))

V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%
IF V% <> H%?&7D THEN OSCLI("LOAD _Config " + STR$~(&FF0000 + H%))

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

IF M$="Y" THEN PRINT " ";CHR$(129);"Resetting maps":IF B% THEN OSCLI("ACCESS _Maps WR")
IF M$="Y" THEN IF NOT B% THEN OSCLI("ACCESS _Maps")
IF M$="Y" THEN Z%=OPENOUT("_Maps"):PRINT#Z%,"_Map1","_Map2","_Map3":CLOSE#Z%:IF B% THEN OSCLI("ACCESS _Maps LR")
IF M$="Y" THEN IF NOT B% THEN OSCLI("ACCESS _Maps L")

IF H$="Y" THEN PRINT " ";CHR$(129);"Resetting high scores":FOR Z%=&00 TO &6F:Z%?H%=Z%?D%:NEXT Z%

IF S$="Y" THEN PRINT " ";CHR$(129);"Resetting controls and settings":FOR Z%=&70 TO &7C:Z%?H%=Z%?D%:NEXT Z%

IF H$="Y" OR S$="Y" THEN V%=0:FOR Z%=&00 TO &7C:V%=(V%+(Z%?H% EOR M%)) AND &FF:NEXT Z%:H%?&7D=V%:IF B% THEN OSCLI("ACCESS _Config WR")
IF H$="Y" OR S$="Y" THEN IF NOT B% THEN OSCLI("ACCESS _Config")
IF H$="Y" OR S$="Y" THEN OSCLI("SAVE _Config " + STR$~(&FF0000 + H%) + " +7E FFFFFF 0"):F%?0=0:F%?1=0:F%?2=0:IF B% THEN OSCLI("ACCESS _Maps LR")
IF H$="Y" OR S$="Y" THEN IF NOT B% THEN OSCLI("ACCESS _Config L")

PRINT:PRINT " ";CHR$(132);

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
