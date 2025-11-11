#!/bin/sh
#COMMAND DEFINE
ICONV="/usr/bin/iconv"

#FILE CONVERT
$ICONV -f SJIS -t UTF-8 EIMGROUP.csv > utf8_EIMGROUP.csv
$ICONV -f SJIS -t UTF-8 EIMROLE.csv > utf8_EIMROLE.csv
$ICONV -f SJIS -t UTF-8 EIMUSER.csv > utf8_EIMUSER.csv
