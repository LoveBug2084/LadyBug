@echo off
set /p projectName=<projectname.txt
start /b beebem.lnk "%CD%\%projectName%.ssd"