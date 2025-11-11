@echo off
setlocal
cd /d %~dp0

title EIMAttributeUpdate.bat

rem Setting Class Pathes.
call internalSetEnv.bat


set FILE_PATH="%1"
set USR_ID="%2"
set PASSWORD="%3"

rem Execute.
java -cp %CLASSPATH% batch.InsertAttributeBatch %FILE_PATH% %USR_ID% %PASSWORD% 

pause
goto :eof

