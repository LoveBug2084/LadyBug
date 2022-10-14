@echo off
set /p bbcProjectName=<projectname.txt
start /b beebem.lnk "%CD%\%bbcProjectName%.ssd"
