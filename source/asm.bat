@echo off
set gameName=LadyBug
echo.
utils\fontmaker  		img-font.raw													img-font.bin
echo.
utils\pixels				img-font-vegetable.raw				4		8		img-font-vegetable.bin
echo.
utils\pixels				img-points.raw								10	10	img-points.bin
echo.
utils\pixels				img-vegetables.raw						10	10	img-vegetables.bin
echo.
utils\pixels				img-objects.raw								8		8		img-objects.bin
echo.
utils\pixels				img-tiles.raw									6		8		img-tiles.bin
echo.
utils\pixels				img-extra.raw									6		8		img-extra.bin
echo.
utils\pixels				img-ladybug.raw								10	14	img-ladybug.bin
echo.
utils\pixels				img-enemy1.raw								10	14	img-enemy1.bin
echo.
utils\pixels				img-enemy2.raw								10	14	img-enemy2.bin
echo.
utils\pixels				img-enemy3.raw								10	14	img-enemy3.bin
echo.
utils\pixels				img-enemy4.raw								10	14	img-enemy4.bin
echo.
utils\pixels				img-enemy5.raw								10	14	img-enemy5.bin
echo.
utils\pixels				img-enemy6.raw								10	14	img-enemy6.bin
echo.
utils\pixels				img-enemy7.raw								10	14	img-enemy7.bin
echo.
utils\pixels				img-enemy8.raw								10	14	img-enemy8.bin
echo.
utils\pixels				img-angel.raw									10	14	img-angel.bin
set /p build=<build.txt
set "buildText=00000%build%"
set "buildText=%buildText:~-6%"
echo %buildText%>build.bin
echo.
beebasm -title %gameName% -v -i build.asm -do %gameName%.ssd -opt 3 -dd -labels labels.txt > listing.txt
if NOT %ERRORLEVEL% == 0 exit /b %ERRORLEVEL%
echo.
echo build %buildText%
echo.
set /a build+=1
echo %build%>build.txt
