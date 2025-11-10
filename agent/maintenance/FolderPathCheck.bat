@echo off
setlocal
cd /d %~dp0

title batch.FolderPathCheck

rem Setting Class Pathes.
call internalSetEnv.bat

rem Execute.
java -cp %CLASSPATH% batch.maintenance.FolderPathCheck

pause
goto :eof

