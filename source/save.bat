@echo off
set /p projectName=<projectname.txt
set /p backupDir=<build.bin
echo creating \backup\%projectName%\%backupDir%
mkdir \backup\%projectName%\%backupDir% > nul
echo copying project to \backup\%projectName%\%backupDir%
xcopy *.* \backup\%projectName%\%backupDir% /y /h /s /e > nul
