HIMEM=&2B00:*/Cls

IF B% THEN u$="WR":l$="LR" ELSE u$="":l$="L"

ONERRORSOUND1,-15,180,5:COLOUR128:COLOUR3:PRINTTAB(5,28);:REPORT:PROCa:PROCz

PROCi:PROCy:PROCz

END



DEFPROCz

REPEAT

PROCw:PROCv:PROCk

UNTILFALSE

ENDPROC



DEFPROCy

CLS

FORI%=0TO17:PROCu(G%+I%,H%,I%):NEXT

!&2B04=-1:PROCw

COLOUR2:PRINTTAB(26,25);"L";:COLOUR3:PRINT"oad";
COLOUR2:PRINTTAB(26,26);"S";:COLOUR3:PRINT"ave";
COLOUR2:PRINTTAB(26,27);"C";:COLOUR3:PRINT"at";

COLOUR2:PRINTTAB(31,25);"E";:COLOUR3:PRINT "rase";
COLOUR2:PRINTTAB(31,26);"B";:COLOUR3:PRINT "oot";

PROCt

ENDPROC



DEFPROCw

IF!&2B00=!&2B04 THENENDPROC

FORI%=0TO2

COLOUR129+NOT(I%=M%):COLOUR3:PRINTTAB(4,25+I%);:IFI%?&2B00<>0THENPRINT"*";ELSEPRINT" ";
COLOUR2:PRINT;1+I%;SPC(19);TAB(7,25+I%);:COLOUR3:PRINTf$(I%);

NEXT

COLOUR128:!&2B04=!&2B00

ENDPROC



DEFPROCv

D%=D%-1:*FX19

IFD%>0THENENDPROC

D%=10:C%=C%EOR1

IFC%=0THENPROCu(V%+X%,W%+Y%,18):PROCu(G%+T%,H%,18)ELSEPROCo(X%,Y%):PROCu(G%+T%,H%,T%)

ENDPROC



DEFPROCk

IFINKEY-98THENIFX%>0THENPROCo(X%,Y%):X%=X%-1:C%=1:D%=1
IFINKEY-67THENIFX%<20THENPROCo(X%,Y%):X%=X%+1:C%=1:D%=1
IFINKEY-73THENIFY%>0THENPROCo(X%,Y%):Y%=Y%-1:C%=1:D%=1
IFINKEY-105THENIFY%<20THENPROCo(X%,Y%):Y%=Y%+1:C%=1:D%=1

IFINKEY-1THENPROCm(X%,Y%,0):PROCo(X%,Y%)
IFINKEY-74THENPROCm(X%,Y%,T%):PROCo(X%,Y%)

K%=INKEY0:IFK%>=97THENK%=K%-32

IFK%=81THENIFT%>0THENPROCu(G%+T%,H%,T%):C%=1:D%=1:T%=T%-1:ENDPROC
IFK%=87THENIFT%<17THENPROCu(G%+T%,H%,T%):C%=1:D%=1:T%=T%+1:ENDPROC

IFK%=49THENIFM%<>0THENM%=0:!&2B04=-1:PROCw:PROCt:C%=1:D%=1:ENDPROC
IFK%=50THENIFM%<>1THENM%=1:!&2B04=-1:PROCw:PROCt:C%=1:D%=1:ENDPROC
IFK%=51THENIFM%<>2THENM%=2:!&2B04=-1:PROCw:PROCt:C%=1:D%=1:ENDPROC

IFK%=76THENPROCl:ENDPROC
IFK%=83THENPROCs:ENDPROC
IFK%=67THENPROCc:ENDPROC
IFK%=69THENm$="Erase map":IFFNc THENPROCn:ENDPROC
IFK%=66THENm$="Boot disk":IFFNc THENPROCx

ENDPROC


DEFPROCx

IF!&2B00<>0THEN!&2B04=-1:PROCw:m$="Unsaved map data"+CHR$(13)+CHR$(10)+"     Exit without saving":IFNOTFNc THENENDPROC

OSCLI("/Cls"):VDU21:OSCLI("EXEC !Boot"):END

ENDPROC



DEFPROCl

COLOUR3

IFM%?&2B00<>0THENm$="Unsaved map, save it now":IFFNc THENPROCs:ENDPROC

VDU28,0,31,39,29,23,1,1;0;0;0;:INPUTTAB(5,0);"Load file name ";x$:VDU23,1,0;0;0;0;26
PROCb

IFx$=""THENx$=f$(M%)

OSCLI("LOAD "+x$+" "+STR$~(&2F19-M%*&E7))
f$(M%)=x$

M%?&2B00=0

!&2B04=-1:PROCw
PROCt

PROCf

ENDPROC



DEFPROCs

COLOUR3:VDU28,0,31,39,29,23,1,1;0;0;0;:INPUTTAB(5,0);"Save file name ";x$:VDU23,1,0;0;0;0;26
PROCb

IFx$=""THENx$=f$(M%)

F%=OPENIN(x$):IFF%<>0THENCLOSE#F%:m$=x$+" exists,overwrite":IFNOTFNc THENENDPROC

OSCLI("SAVE "+x$+" "+STR$~(&2F19-M%*&E7)+" +E7 FFFFFF 0")
f$(M%)=x$

PROCf

M%?&2B00=0:!&2B04=-1:PROCw

ENDPROC



DEFPROCf

OSCLI("ACCESS _Maps "+u$)
F%=OPENOUT("_Maps")
PRINT#F%,f$(0),f$(1),f$(2)
CLOSE#F%
OSCLI("ACCESS _Maps "+l$)

ENDPROC



DEFPROCc

CLS:COLOUR3:*CAT

PROCa:PROCy

ENDPROC



DEFPROCa

COLOUR2:PRINTTAB(5,30);"Press any key";:K%=GET

REPEATK%=INKEY0:UNTILNOTINKEY-1ANDNOTINKEY-74

PROCb

ENDPROC



DEFFNc

COLOUR3:PRINTTAB(5,29);m$;" ? Y/N";

K%=GET:PROCb

=K%=89ORK%=121



DEFPROCt

GCOL0,3
MOVEV%*48-28,1020-W%*32+20
DRAW(V%+21)*48+16,1020-W%*32+20
DRAW(V%+21)*48+16,1020-(W%+21)*32-16
DRAWV%*48-28,1020-(W%+21)*32-16
DRAWV%*48-28,1020-W%*32+20

FORJ%=0TO20:!&78=J%*11+&2F19-M%*&E7:!&74=(W%+J%)*640+V%*24+&3000:CALL&2B57:NEXT

ENDPROC



DEFPROCo(I%,J%)

IFI%>10THENI%=20-I%

PROCu(V%+I%,W%+J%,?(J%*11+I%+&2F19-M%*&E7))
PROCu(V%+20-I%,W%+J%,?(&2B08+?(J%*11+I%+&2F19-M%*&E7)))

ENDPROC



DEFPROCm(I%,J%,L%)

IFI%>10THENI%=20-I%:L%=L%?&2B08

IFL%=0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=1THENIF(I%AND1)=0THENIF(J%AND1)=0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=2ORL%=3THENIF(I%AND1)=1AND(J%AND1)=1THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=4THENIFJ%>=2THENIF(I%AND1)<>0AND(J%AND1)=0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=5THENIFJ%<=18THENIF(I%AND1)<>0AND(J%AND1)=0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=6THENIFI%>=2THENIF(I%AND1)=0AND(J%AND1)<>0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%=7THENIF(I%AND1)=0AND(J%AND1)<>0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%>=8THENIFL%<=11THENIF(I%AND1)<>0AND(J%AND1)<>0THENI%?(J%*11+&2F19-M%*&E7)=L%
IFL%>=12THENIF(I%AND1)<>0OR(J%AND1)<>0THENI%?(J%*11+&2F19-M%*&E7)=L%

M%?&2B00=1

ENDPROC



DEFPROCu(I%,J%,L%)

!&74=J%*640+I%*24+&3000:A%=L%:CALL&2B1A

ENDPROC



DEFPROCn

FORJ%=0TO20:FORI%=0TO10
IF(I%AND1)=0AND(J%AND1)=0THEN?(J%*11+I%+&2F19-M%*&E7)=1ELSE?(J%*11+I%+&2F19-M%*&E7)=0
NEXT,

?(&2F19-M%*&E7+&6C)=&0F:?(&2F19-M%*&E7+&77)=&0C:?(&2F19-M%*&E7+&78)=&00:?(&2F19-M%*&E7+&82)=&0A:?(&2F19-M%*&E7+&83)=&0D:?(&2F19-M%*&E7+&BA)=&00:?(&2F19-M%*&E7+&D0)=&00:?(&2F19-M%*&E7+&E6)=&00

M%?&2B00=1:!&2B04=-1:PROCw:PROCt

ENDPROC



DEFPROCb

PRINT TAB(0,29);SPC(80);

ENDPROC




DEFPROCi

*LOAD EditorM 2B00
*LOAD EditorT 2B83

DIM f$(3)

F%=OPENIN("_Maps")
INPUT#F%,f$(0),f$(1),f$(2)
CLOSE#F%
FORZ%=0TO2:OSCLI("LOAD "+f$(Z%)+" "+STR$~(&2F19-Z%*&E7)):NEXT
!&2B00=0

V%=3:W%=1:G%=4:H%=23:T%=1:X%=0:Y%=0:C%=1:D%=1:M%=0:m$=""

ENDPROC
