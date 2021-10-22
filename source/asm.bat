@echo off
echo.
fontmaker  		img-font.raw													img-font.bin
echo.
pixels				img-minifont-decimal.raw			4		8		img-minifont-decimal.bin
echo.
pixels				img-minifont-hexadecimal.raw	4		8		img-minifont-hexadecimal.bin
echo.
pixels				img-points.raw								10	10	img-points.bin
echo.
pixels				img-vegetables.raw						10	10	img-vegetables.bin
echo.
pixels				img-objects.raw								8		8		img-objects.bin
echo.
pixels				img-tiles.raw									6		8		img-tiles.bin
echo.
pixels				img-extra.raw									6		8		img-extra.bin
echo.
pixels				img-ladybug.raw								10	14	img-ladybug.bin
echo.
pixels				img-enemy1.raw								10	14	img-enemy1.bin
echo.
pixels				img-enemy2.raw								10	14	img-enemy2.bin
echo.
pixels				img-enemy3.raw								10	14	img-enemy3.bin
echo.
pixels				img-enemy4.raw								10	14	img-enemy4.bin
echo.
pixels				img-enemy5.raw								10	14	img-enemy5.bin
echo.
pixels				img-enemy6.raw								10	14	img-enemy6.bin
echo.
pixels				img-enemy7.raw								10	14	img-enemy7.bin
echo.
pixels				img-enemy8.raw								10	14	img-enemy8.bin
echo.
pixels				img-angel.raw									10	14	img-angel.bin
set /p ladybugBuild=<ladybug-build.txt
set "ladybugBuildPadded=00000%ladybugBuild%"
set "ladybugBuildPadded=%ladybugBuildPadded:~-6%"
echo %ladybugBuildPadded%>ladybug-build-padded.txt
echo.
beebasm -title LadyBug -v -i ladybug.asm -do ladybug.ssd -opt 3 > ladybug.lst
if NOT %ERRORLEVEL% == 0 goto buildFalse
:buildTrue
echo.
echo build %ladybugBuildPadded%
echo.
set /a ladybugBuild+=1
echo %ladybugBuild%>ladybug-build.txt
:buildFalse
