@echo off
setlocal
cd /d %~dp0

title batch.setting

rem Setting Class Pathes.
call internalSetEnv.bat

rem Execute
java -cp %CLASSPATH% org.mozilla.javascript.tools.shell.Main -f setLayoutToDocumentAttribute.js

rem To use Rhino with command-line prompt, relpace to this line.
rem java -cp %CLASSPATH% org.mozilla.javascript.tools.shell.Main

pause
goto :eof

:addClasspath
set CLASSPATH=%CLASSPATH%;%1
goto :eof
