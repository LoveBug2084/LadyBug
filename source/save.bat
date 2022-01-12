@echo off
set /p backupDir=<build.bin
echo creating ..\..\backup\%backupDir%
mkdir ..\..\backup\%backupDir% > nul
echo copying project to ..\..\backup\%backupDir%
xcopy *.* ..\..\backup\%backupDir% /y /h /s /e > nul
