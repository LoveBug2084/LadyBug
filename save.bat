@echo off
set /p ladybugBuildPadded=<ladybug-build-padded.txt
echo creating ..\backup\%ladybugBuildPadded%
mkdir ..\backup\%ladybugBuildPadded% > nul
echo copying project to ..\backup\%ladybugBuildPadded%
xcopy *.* ..\backup\%ladybugBuildPadded% /y /h /s /e > nul
