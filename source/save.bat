@echo off
set /p bbcProjectName=<projectName.txt
set /p bbcProjectBuildBin=<buildNumber.bin
echo creating \backup\%bbcProjectName%\%bbcProjectBuildBin%
mkdir "\backup\%bbcProjectName%\%bbcProjectBuildBin%" > nul
echo copying project to \backup\%bbcProjectName%\%bbcProjectBuildBin%
xcopy *.* "\backup\%bbcProjectName%\%bbcProjectBuildBin%" /y /h /s /e > nul
