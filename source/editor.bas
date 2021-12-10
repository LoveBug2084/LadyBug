MODE1:HIMEM=&2B00

ONERRORPROCe:PROCa:PROCm

PROCi
PROCds

PROCm

END



DEFPROCe

SOUND1,-15,180,5:COLOUR128:COLOUR3:PRINTTAB(5,29);

IFERR=201THENPRINT"Disk read only";:ENDPROC
IFERR=204THENPRINT"Bad file name";:ENDPROC
IFERR=214THENPRINT"File not found";:ENDPROC

PRINTTAB(0,28);:REPORT:PRINT" line ";ERL;

ENDPROC



DEFPROCm

REPEAT

PROCdc
PROCpk

UNTILFALSE

ENDPROC



DEFPROCds

CLS

FORI%=0TO17:PROCdt(G%+I%,H%,I%):NEXT

PROCdf

COLOUR2:PRINTTAB(26,25);"L";:COLOUR3:PRINT"oad";
COLOUR2:PRINTTAB(26,26);"S";:COLOUR3:PRINT"ave";
COLOUR2:PRINTTAB(26,27);"C";:COLOUR3:PRINT"at";

COLOUR2:PRINTTAB(31,25);"E";:COLOUR3:PRINT "rase";
COLOUR2:PRINTTAB(31,26);"B";:COLOUR3:PRINT "oot";

PROCdm

ENDPROC



DEFPROCdf

FORI%=0TO2

IFI%=M%THENCOLOUR129ELSECOLOUR128
COLOUR3:PRINTTAB(4,25+I%);:IFI%?&2B00<>0THENPRINT"*";ELSEPRINT" ";
COLOUR2:PRINT;1+I%;SPC(19);TAB(7,25+I%);:COLOUR3:PRINTf$(I%);

NEXT

COLOUR128

U%=0

ENDPROC



DEFPROCdc

B%=B%-1
*FX19

IFB%>0THENENDPROC

B%=10
C%=C%EOR1

IFC%=0THENPROCdt(V%+X%,W%+Y%,18):PROCdt(G%+T%,H%,18)ELSEPROCdmt(X%,Y%):PROCdt(G%+T%,H%,T%)

ENDPROC



DEFPROCpk

IFINKEY-98THENIFX%>0THENPROCdmt(X%,Y%):X%=X%-1:C%=1:B%=1
IFINKEY-67THENIFX%<20THENPROCdmt(X%,Y%):X%=X%+1:C%=1:B%=1
IFINKEY-73THENIFY%>0THENPROCdmt(X%,Y%):Y%=Y%-1:C%=1:B%=1
IFINKEY-105THENIFY%<20THENPROCdmt(X%,Y%):Y%=Y%+1:C%=1:B%=1

IFINKEY-1THENPROCpmt(X%,Y%,0):PROCdmt(X%,Y%)
IFINKEY-74THENPROCpmt(X%,Y%,T%):PROCdmt(X%,Y%)

k$=INKEY$0

IFk$="Q"ORk$="q"THENIFT%>0 THEN PROCdt(G%+T%,H%,T%):C%=1:B%=1:T%=T%-1:ENDPROC
IFk$="W"ORk$="w"THENIFT%<17 THEN PROCdt(G%+T%,H%,T%):C%=1:B%=1:T%=T%+1:ENDPROC

IFk$="1"THENIFM%<>0 THEN M%=0:PROCdf:PROCdm:C%=1:B%=1:ENDPROC
IFk$="2"THENIFM%<>1 THEN M%=1:PROCdf:PROCdm:C%=1:B%=1:ENDPROC
IFk$="3"THENIFM%<>2 THEN M%=2:PROCdf:PROCdm:C%=1:B%=1:ENDPROC

IFk$="L"ORk$="l"THENPROClm:ENDPROC
IFk$="S"ORk$="s"THENPROCsm:ENDPROC
IFk$="C"ORk$="c"THENPROCc:ENDPROC
IFk$="E"ORk$="e"THENm$="Erase map":IFFNc THENPROCem:ENDPROC
IFk$="B"ORk$="b"THENm$="Boot disk":IFFNc THENPROCx

IFk$=""THENU%=U%+1:IFU%>=100THENPROCdf

ENDPROC


DEFPROCx

IF!&2B00<>0THENPROCdf:m$="Unsaved map data"+CHR$(13)+CHR$(10)+"     Exit without saving":IFNOTFNc THENENDPROC

OSCLI("EXEC $.!Boot"):END

ENDPROC



DEFPROClm

COLOUR3

IFM%?&2B00<>0THENm$="Unsaved map, save it now":IFFNc THENPROCsm:ENDPROC

VDU28,0,31,39,29,23,1,1;0;0;0;:INPUTTAB(5,0);"Load file name ";l$:VDU23,1,0;0;0;0;26
PROCb

OSCLI("LOAD "+l$+" "+STR$~(&2F19-M%*&E7))
f$(M%)=l$

M%?&2B00=0

PROCdf
PROCdm

f%=OPENOUT("$.Maps")
PRINT#f%,f$(0),f$(1),f$(2)
CLOSE#f%

ENDPROC



DEFPROCsm

COLOUR3
VDU28,0,31,39,29,23,1,1;0;0;0;:INPUTTAB(5,0);"Save file name ";s$:VDU23,1,0;0;0;0;26
PROCb

IFs$=""THENs$=f$(M%)

f%=OPENIN(s$):IFf%<>0THENCLOSE#f%:m$=s$+" exists,overwrite":IFNOTFNc THENENDPROC

OSCLI("SAVE "+s$+" "+STR$~(&2F19-M%*&E7)+" +E7 FFFFFF 0")
f$(M%)=s$

f%=OPENOUT("$.Maps")
PRINT#f%,f$(0),f$(1),f$(2)
CLOSE#f%

M%?&2B00=0

PROCdf

ENDPROC



DEFPROCc

CLS

COLOUR3
*CAT

PROCa
PROCds

ENDPROC



DEFPROCa

COLOUR2
PRINTTAB(5,30);"Press any key";

k$=GET$

REPEAT UNTIL INKEY-1=0 AND INKEY-74=0

PROCb

ENDPROC



DEFFNc

COLOUR3
PRINTTAB(5,29);m$;" ? Y/N";

k$=GET$
PROCb

=k$="Y"



DEFPROCdm

GCOL0,3
MOVEV%*48-24,1020-W%*32+16
DRAW(V%+21)*48+24,1020-W%*32+16
DRAW(V%+21)*48+24,1020-(W%+21)*32-16
DRAWV%*48-24,1020-(W%+21)*32-16
DRAWV%*48-24,1020-W%*32+16

FORy%=0TO20:!&78=y%*11+&2F19-M%*&E7:!&74=(W%+y%)*640+V%*24+&3000:CALL&2B53:NEXT

ENDPROC



DEFPROCdmt(x%,y%)

IFx%>10 THEN x%=20-x%

PROCdt(V%+x%,W%+y%,?(y%*11+x%+&2F19-M%*&E7))
PROCdt(V%+20-x%,W%+y%,?(&2B04+?(y%*11+x%+&2F19-M%*&E7)))

ENDPROC



DEFPROCpmt(x%,y%,t%)

IFx%>10THENx%=20-x%:t%=t%?&2B04

IFt%=0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=1THENIF(x% AND 1)=0THENIF(y% AND 1)=0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=2ORt%=3THENIF(x%AND1)=1AND(y%AND1)=1THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=4THENIFy%>=2THENIF(x%AND1)<>0AND(y%AND1)=0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=5THENIFy%<=18THENIF(x%AND1)<>0AND(y%AND1)=0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=6THENIFx%>=2THENIF(x%AND1)=0AND(y%AND1)<>0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%=7THENIF(x%AND1)=0AND(y%AND1)<>0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%>=8THENIFt%<=11THENIF(x%AND1)<>0AND(y%AND1)<>0THENx%?(y%*11+&2F19-M%*&E7)=t%
IFt%>=12THENIF(x%AND1)<>0OR(y%AND1)<>0THENx%?(y%*11+&2F19-M%*&E7)=t%

M%?&2B00=1

ENDPROC



DEFPROCdt(x%,y%,t%)

!&74=y%*640+x%*24+&3000:A%=t%:CALL&2B16

ENDPROC



DEFPROCem

FORy%=0TO20:FORx%=0TO10
IF(x%AND1)=0AND(y%AND1)=0THEN?(y%*11+x%+&2F19-M%*&E7)=1ELSE?(y%*11+x%+&2F19-M%*&E7)=0
NEXT,

?(&2F19-M%*&E7+&6C)=&0F:?(&2F19-M%*&E7+&77)=&0C:?(&2F19-M%*&E7+&78)=&00:?(&2F19-M%*&E7+&82)=&0A:?(&2F19-M%*&E7+&83)=&0D:?(&2F19-M%*&E7+&BA)=&00:?(&2F19-M%*&E7+&D0)=&00:?(&2F19-M%*&E7+&E6)=&00

M%?&2B00=1

PROCdf
PROCdm

ENDPROC



DEFPROCb

PRINT TAB(0,29);SPC(80);

ENDPROC




DEFPROCi

VDU23,1,0;0;0;0;
VDU19,1,4;0;19,2,5;0;19,3,2;0;

*LOAD E.Editor 2B00

*LOAD E.Tiles 2B83

DIM f$(3)

f%=OPENIN("$.Maps")
INPUT#f%,f$(0),f$(1),f$(2)
CLOSE#f%
FORZ%=0TO2:OSCLI("LOAD "+f$(Z%)+" "+STR$~(&2F19-Z%*&E7)):NEXT
!&2B00=0

V%=3:W%=1
G%=4:H%=23

T%=1
X%=0:Y%=0

C%=1:B%=1

M%=0

m$=""

U%=0

ENDPROC

