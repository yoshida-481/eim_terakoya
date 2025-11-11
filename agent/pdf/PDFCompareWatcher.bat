@echo off
setlocal
cd /d %~dp0

rem 二重起動防止
set BATCH_NAME=PDFCompareWatcher
powershell ".\checkDuplicateBatch.ps1 %BATCH_NAME%;exit $LASTEXITCODE"
if %ERRORLEVEL% == -1 (
	echo 既にバッチが起動済みです。
	pause
	goto :eof
)

title batch.PDFCompareWatcher

rem Setting Class Pathes.
call internalSetEnv.bat

rem Execute.
java -cp %CLASSPATH% -Doracle.jdbc.autoCommitSpecCompliant=false -Djava.net.preferIPv4Stack=true -Djgroups.tcp.address=127.0.0.1 -Djgroups.mping.mcast_addr=228.2.4.6 batch.PDFCompareWatcher

pause
goto :eof

