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

MODE 7
HIMEM=&7B00

*LOAD $.Default FF7B00
*LOAD $.Config FF7B80

INPUT "Do you wish to reset the high score     table to default Y/N ";H$
IF H$="Y" THEN FOR Z%=0 TO 111:Z%?&7B80=Z%?&7B00:NEXT Z%
PRINT

INPUT "Do you wish to reset the game settings  to default Y/N ";S$
IF S$="Y" THEN FOR Z%=112 TO 125:Z%?&7B80=Z%?&7B00:NEXT Z%
PRINT

IF H$="Y" OR S$="Y" THEN ?&130=0:?&131=0:?&132=0:PRINT "Saving $.Config":*SAVE $.Config FF7B80 +7E
PRINT
