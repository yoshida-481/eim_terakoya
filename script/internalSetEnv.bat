@echo off
rem setlocal
rem cd /d %~dp0

rem Setting Class Pathes.
set CLASSPATH=.\;.\classes;.\config
for %%f in (.\lib\*.jar) do call :addClasspath %%f

goto :eof

:addClasspath
set CLASSPATH=%CLASSPATH%;%1
goto :eof

