E%=FALSE
HIMEM=&7800

C%=&7B80
S%=&8010
F%=&130
M%=&69

A%=5:X%=&70:Y%=0:!X%=&0D240072:B%=(USR(&FFDD)AND&FF)=2
IF B% THEN U$="WR":L$="LR" ELSE U$="":L$="L"

ON ERROR PROCerror

PROCloadConfig

up$=FNconfigKey(CHR$(C%?&7C))
down$=FNconfigKey(CHR$(C%?&7B))

PROCintro

REPEAT

PROCshrink(2)
PROCoptions
PROCexpand(3)

REPEAT

K$=FNwaitKey

IF K$="G" THEN G%=NOT G%:PROCdisplayMode

UNTIL K$<>"G"

PROCshrink(2)

IF K$="I" THEN PROCinstructionsGame

IF K$="W" THEN PROCinstructionsEditor

UNTIL K$="K" OR K$="J" OR K$="U" OR K$="E" OR K$="R"

VDU 23;1,1,0;0;0;

IF K$="R" THEN CHAIN"Reset"

!F%=0:PROCloadConfig
IF K$="K" THEN F%?3=0
IF K$="J" THEN F%?3=1
IF K$="U" THEN F%?3=2
F%?4=G%

IF G% THEN map1$="_Map1":map2$="_Map2":map3$="_Map3" ELSE Z%=OPENIN("_Maps"):INPUT#Z%,map1$,map2$,map3$:CLOSE#Z%

A%=FALSE
Z%=OPENIN(map1$):IF Z%<>0 THEN CLOSE#Z% ELSE map1$="_Map1":A%=TRUE
Z%=OPENIN(map2$):IF Z%<>0 THEN CLOSE#Z% ELSE map2$="_Map2":A%=TRUE
Z%=OPENIN(map3$):IF Z%<>0 THEN CLOSE#Z% ELSE map3$="_Map3":A%=TRUE

IF A% AND NOT G% THEN OSCLI("ACCESS _Maps "+U$):Z%=OPENOUT("_Maps"):PRINT#Z%,map1$,map2$,map3$:CLOSE#Z%:OSCLI("ACCESS _Maps "+L$)

IF K$="E" THEN CHAIN"Editor"

PROCsplash
PROCexpand(1)

OSCLI("LOAD "+map1$+" 7800")
OSCLI("LOAD "+map2$+" 7900")
OSCLI("LOAD "+map3$+" 7A00")
*/Loader

END



DEF PROCintro

IF E% THEN PROCanyKey(400):ENDPROC

PROCsplash
PROCexpand(1)

PROCsaveConfig

PROCanyKey(200)

ENDPROC



DEF PROCsplash

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

PRINT TAB(0,0);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);
PRINT TAB(0,1);CHR$(141);CHR$(129);CHR$(157);CHR$(131);"            Lady Bug              ";CHR$(156);

PROCeraseScreen(2)

PRINT TAB(7,3);CHR$(131);"A remake of the original";
PRINT TAB(6,4);CHR$(129);"Universal 1981 arcade game";

PRINT TAB(6,6);CHR$(133);"Programmed by";CHR$(131);"LoveBug";CHR$(133);"2021";

PRINT TAB(15,9);CHR$(135);"Options";

PRINT TAB(6,11);CHR$(133);"I";CHR$(135);"-";CHR$(132);"Instructions";

PRINT TAB(6,13);CHR$(129);"K";CHR$(135);"-";CHR$(131);"Play";CHR$(129);"Keyboard";

PRINT TAB(6,14);CHR$(130);"J";CHR$(135);"-";CHR$(133);"Play";CHR$(130);"Joystick Analogue";

PRINT TAB(6,15);CHR$(132);"U";CHR$(135);"-";CHR$(134);"Play";CHR$(132);"Joystick User Port";

PRINT TAB(6,17);CHR$(131);"G";CHR$(135);"-";:PROCdisplayMode

PRINT TAB(6,19);CHR$(133);"E";CHR$(135);"-";CHR$(130);"Map Editor";

PRINT TAB(6,20);CHR$(134);"W";CHR$(135);"-";CHR$(132);"Map Editor Keys";

PRINT TAB(6,22);CHR$(129);"R";CHR$(135);"-";CHR$(131);"Reset settings";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"        Choose an option          ";CHR$(156);

ENDPROC



DEF PROCdisplayMode

PRINT TAB(10,17);
IF G% THEN PRINT CHR$(133);"High Score Challenge Mode"; ELSE PRINT CHR$(129);"Arcade Mode";SPC(14);

ENDPROC



DEF PROCinstructionsGame

PROCeraseScreen(2)

PRINT TAB(13,3);CHR$(135);"Instructions";TAB(34,3);CHR$(131);"1/3";

PRINT TAB(2,5);CHR$(130);"Guide";CHR$(129);"Lady Bug";CHR$(130);"through the mazes";
PRINT TAB(2,6);CHR$(130);"avoiding deadly";CHR$(135);"enemies";CHR$(130);"and";CHR$(135);"skulls";

PRINT TAB(2,8);CHR$(132);"Push the";CHR$(130);"green doors";CHR$(132);"to escape";
PRINT TAB(2,9);CHR$(132);"enemies and to block their attacks";

PRINT TAB(2,11);CHR$(133);"Collect";CHR$(134);"cyan hearts";CHR$(133);"to multiply";
PRINT TAB(2,12);CHR$(133);"item score values by";CHR$(134);"x2 x3 x5";

PRINT TAB(2,14);CHR$(129);"Collect";CHR$(131);"yellow letters";CHR$(129);"to spell";
PRINT TAB(2,15);CHR$(131);"EXTRA";CHR$(129);"for";CHR$(135);~Y%;" extra ";:IF Y%=1 THEN PRINT "life"; ELSE PRINT "lives";

PRINT TAB(2,17);CHR$(130);"Collect";CHR$(129);"red letters";CHR$(130);"to spell";
PRINT TAB(2,18);CHR$(129);"SPECIAL";CHR$(130);"for";CHR$(135);~X%;"00000 points";CHR$(130);"plus a";
PRINT TAB(2,19);CHR$(135);"skull shield";CHR$(130);"lasting ";~W%;" rounds";

PRINT TAB(2,21);CHR$(132);"Collect";CHR$(131);"vegetables";CHR$(132);"to paralyse the";
PRINT TAB(2,22);CHR$(132);"enemies and earn bonus points";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"         Press any key            ";CHR$(156);

PROCexpand(3)
PROCanyKey(360000)

PROCshrink(3)
PRINT TAB(34,3);CHR$(131);"2";
PROCeraseScreen(5)

PRINT TAB(2,5);CHR$(132);"A special diamond bonus worth";
PRINT TAB(2,6);CHR$(135);~U%;"00000 points";CHR$(132);"will appear if";
PRINT TAB(2,7);CHR$(132);"you can reach";CHR$(131);"level ";~Q%;CHR$(132);"while only";
PRINT TAB(2,8);CHR$(132);"collecting";CHR$(134);"cyan hearts and letters";
PRINT TAB(2,9);CHR$(132);"and";CHR$(131);"without losing a life";

PRINT TAB(2,11);CHR$(129);"Use the";CHR$(131);up$;CHR$(129);"and";CHR$(131);down$;CHR$(129);"keys to";
PRINT TAB(2,12);CHR$(129);"navigate the game menu and";CHR$(131);"RETURN";
PRINT TAB(2,13);CHR$(129);"to adjust the settings and/or";
PRINT TAB(2,14);CHR$(129);"redefine the controls";

PRINT TAB(2,16);CHR$(130);"During the game press";CHR$(129);"RETURN";CHR$(130);"to";
PRINT TAB(2,17);CHR$(130);"pause, move";CHR$(129);"Lady Bug";CHR$(130);"to unpause";

PRINT TAB(2,19);CHR$(133);"Hold";CHR$(135);"ESC";CHR$(133);"to quit the current game";

PRINT TAB(2,21);CHR$(132);"Reboot the disk to save your";
PRINT TAB(2,22);CHR$(135);"high scores";CHR$(132);"and";CHR$(135);"game settings";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"         Press any key            ";CHR$(156);

PROCexpand(5)
PROCanyKey(360000)

PROCshrink(3)
PRINT TAB(34,3);CHR$(131);"3";
PROCeraseScreen(5)

PRINT TAB(2,5);CHR$(133);"There are a few differences in the";
PRINT TAB(2,6);CHR$(133);"Stardot high score challenge mode";

PRINT TAB(2,8);CHR$(130);"The";CHR$(135);"Diamond";CHR$(130);"bonus is not available";

PRINT TAB(2,10);CHR$(131);"The";CHR$(129);"Special";CHR$(131);"bonus awards";CHR$(129);"shields";
PRINT TAB(2,11);CHR$(131);"but does not award any";CHR$(135);"points";

PRINT TAB(2,13);CHR$(133);"The";CHR$(135);"3 default maze maps";CHR$(133);"will be";
PRINT TAB(2,14);CHR$(133);"used instead of user maze maps";

PRINT TAB(2,16);CHR$(129);"The the default";CHR$(131);"lives";CHR$(129);"and";
PRINT TAB(2,17);CHR$(131);"difficulty";CHR$(129);"will be used";

PRINT TAB(2,19);CHR$(132);"Press";CHR$(131);"G";CHR$(132);"to toggle between";CHR$(129);"Arcade";
PRINT TAB(2,20);CHR$(132);"and";CHR$(133);"High Score Challenge";CHR$(132);"modes";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"         Press any key            ";CHR$(156);

PROCexpand(5)
PROCanyKey(360000)

ENDPROC



DEF PROCinstructionsEditor

PROCeraseScreen(2)

PRINT TAB(13,4);"Map Editor keys";

PRINT TAB(5, 7);CHR$(134);"123   ";CHR$(135);"-";CHR$(129);"Select map";

PRINT TAB(5, 9);CHR$(132);"QW    ";CHR$(135);"-";CHR$(131);"Select tile";
PRINT TAB(5,10);CHR$(133);":/ZX  ";CHR$(135);"-";CHR$(130);"Move cursor";
PRINT TAB(5,11);CHR$(130);"SHIFT ";CHR$(135);"-";CHR$(133);"Erase tile";
PRINT TAB(5,12);CHR$(131);"RETURN";CHR$(135);"-";CHR$(132);"Draw tile";

PRINT TAB(5,14);CHR$(129);"E     ";CHR$(135);"-";CHR$(134);"Erase map";

PRINT TAB(5,16);CHR$(134);"L     ";CHR$(135);"-";CHR$(129);"Load map from disk";
PRINT TAB(5,17);CHR$(133);"S     ";CHR$(135);"-";CHR$(130);"Save map to disk";

PRINT TAB(5,19);CHR$(132);"C     ";CHR$(135);"-";CHR$(131);"Catalogue disk";

PRINT TAB(5,21);CHR$(130);"B     ";CHR$(135);"-";CHR$(133);"Boot disk";

PRINT TAB(0,24);CHR$(136);CHR$(129);CHR$(157);CHR$(131);"         Press any key            ";CHR$(156);

PROCexpand(3)
PROCanyKey(360000)

ENDPROC



DEF PROCshrink(O%)

FOR R%=24 TO O% STEP -2
*FX 19
VDU 23;6,R%,0;0;0;
NEXT R%

ENDPROC



DEF PROCexpand(O%)

FOR R%=O% TO 25 STEP 2
*FX 19
VDU 23;6,R%,0;0;0;
VDU 23;1,40,0;0;0;
NEXT R%
*FX 19

ENDPROC



DEF PROCerror

E%=TRUE

PROCeraseLogo

IF ERR<>201 THEN PROCunexpectedError

PRINT TAB(4,11);CHR$(136);CHR$(129);"Write protected";CHR$(132);"unable to save";
PRINT TAB(5,13);CHR$(136);CHR$(135);"high scores";CHR$(132);"or";CHR$(135);"game settings";

ENDPROC



DEF PROCunexpectedError

VDU 6,12

REPORT
PRINT " at line ";ERL
PRINT

VDU 23,1,1;0;0;0;
VDU 23;6,25,0;0;0;

*FX 4
*FX 200

END

ENDPROC



DEF PROCloadConfig

Z%=OPENIN("_Bonus")
Y%=BGET#Z%:X%=BGET#Z%:W%=BGET#Z%:U%=BGET#Z%:Q%=BGET#Z%
CLOSE#Z%

V%=((F%?0 EOR M%) + (F%?1 EOR M%)) AND &FF 
IF F%?2<>V% THEN IF NOT G% THEN OSCLI("LOAD _Config " + STR$~(&FF0000 + C%)):ENDPROC
IF F%?2<>V% THEN IF G% THEN OSCLI("LOAD _Hsc " + STR$~(&FF0000 + C%)):ENDPROC

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
CPX #&7F
BNE LOOP
LDA &F4
STA &FE30
CLI
RTS
]

CALL &7B00:G%=(C%?&7E)<>0

ENDPROC



DEF PROCsaveConfig

V%=((F%?0 EOR M%) + (F%?1 EOR M%)) AND &FF 
IF F%?2<>V% THEN ENDPROC

IF G% THEN OSCLI("ACCESS _Hsc "+U$):OSCLI("SAVE _Hsc " + STR$~(&FF0000 + C%) + " +7E FFFFFF 0"):OSCLI("ACCESS _Hsc "+L$) ELSE OSCLI("ACCESS _Config "+U$):OSCLI("SAVE _Config " + STR$~(&FF0000 + C%) + " +7E FFFFFF 0"):OSCLI("ACCESS _Config "+L$)

PROCeraseLogo

PRINT TAB(5,11);CHR$(135);"High scores";CHR$(132);"and";CHR$(135);"game settings";
PRINT TAB(11,13);CHR$(132);"saved successfully";
!F%=0

ENDPROC



DEF PROCeraseLogo

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



DEF PROCeraseScreen(O%)

FOR R%=O% TO 24:PRINT TAB(0,R%);SPC(39);:NEXT R%

ENDPROC



DEF PROCanyKey(T%)

REPEAT
K%=INKEY0
UNTIL K%=-1

TIME=0

REPEAT
K%=INKEY0
UNTIL K%<>-1 OR TIME>=T%

ENDPROC



DEF FNwaitKey

REPEAT

K%=INKEY0
IF K%>=ASC("`") THEN K%=K%-ASC(" ")
K$=CHR$(K%)

UNTIL K$="G" OR K$="K" OR K$="J" OR K$="U" OR K$="I" OR K$="E" OR K$="W" OR K$="R"

=K$



DEF FNconfigKey(K$)

IF K$="<" THEN ="UP"
IF K$="=" THEN ="DOWN"
IF K$=">" THEN ="LEFT"
IF K$="?" THEN ="RIGHT"

=K$
