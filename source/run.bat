@echo off
set /p bbcProjectName=<projectName.txt
start /b beebem.lnk "%CD%\%bbcProjectName%.ssd"
