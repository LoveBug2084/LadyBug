@echo off

rem --------------------------------
rem  convert raw image files to bin
rem --------------------------------
echo.
utils\fontmaker  		img-font.raw													img-font.bin
utils\pixels				img-font-vegetable.raw				4		8		img-font-vegetable.bin
utils\pixels				img-points.raw								10	10	img-points.bin
utils\pixels				img-vegetables.raw						10	10	img-vegetables.bin
utils\pixels				img-objects.raw								8		8		img-objects.bin
utils\pixels				img-tiles.raw									6		8		img-tiles.bin
utils\pixels				img-extra.raw									6		8		img-extra.bin
utils\pixels				img-ladybug.raw								10	14	img-ladybug.bin
utils\pixels				img-enemy1.raw								10	14	img-enemy1.bin
utils\pixels				img-enemy2.raw								10	14	img-enemy2.bin
utils\pixels				img-enemy3.raw								10	14	img-enemy3.bin
utils\pixels				img-enemy4.raw								10	14	img-enemy4.bin
utils\pixels				img-enemy5.raw								10	14	img-enemy5.bin
utils\pixels				img-enemy6.raw								10	14	img-enemy6.bin
utils\pixels				img-enemy7.raw								10	14	img-enemy7.bin
utils\pixels				img-enemy8.raw								10	14	img-enemy8.bin
utils\pixels				img-angel.raw									10	14	img-angel.bin

rem -----------------------------------------------------------------------------------------
rem  read project name and current build number, create build number text with leading zeros
rem -----------------------------------------------------------------------------------------
set /p bbcProjectName=<projectname.txt
set /p bbcProjectBuildText=<build.txt
set "bbcProjectBuildBin=00000%bbcProjectBuildText%"
set "bbcProjectBuildBin=%bbcProjectBuildBin:~-6%"

rem ---------------
rem  build project
rem ---------------
echo.
echo %bbcProjectBuildBin%>build.bin
beebasm -title %bbcProjectName% -v -i build.asm -do %bbcProjectName%.ssd -opt 3 -dd -labels labels.txt > listing.txt
if NOT %ERRORLEVEL% == 0 exit /b %ERRORLEVEL%

rem -----------------------
rem  run project in beebem
rem -----------------------
echo.
echo %bbcProjectName% build %bbcProjectBuildBin%
call run.bat
