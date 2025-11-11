@echo off
rem setlocal
rem cd /d %~dp0

rem Setting DLL Path
set PMU_BIN=D:/YSS/MakeupV225/bin
set PMU_BIN_IODOC=D:/YSS/IOWebDOC/bin
set PATH=%PMU_BIN%;%PMU_BIN_IODOC%;%PATH%

rem Setting Class Pathes.
set CLASSPATH=.;.\config
for %%f in (.\lib\*.jar) do call :addClasspath %%f

goto :eof

:addClasspath
set CLASSPATH=%CLASSPATH%;%1
goto :eof

