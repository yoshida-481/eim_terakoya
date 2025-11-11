@echo off
rem setlocal
rem cd /d %~dp0

rem Setting DLL Path
set PMU_BIN=D:/YSS/MakeupV225/bin
set PATH=%PMU_BIN%;%PATH%

rem Setting Class Pathes.
set CLASSPATH=.;.\config
for %%f in (.\lib\*.jar) do call :addClasspath %%f

goto :eof

:addClasspath
set CLASSPATH=%CLASSPATH%;%1
goto :eof

