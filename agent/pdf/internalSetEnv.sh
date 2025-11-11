#!/bin/sh

# Setting Class Pathes.
CLASSPATH=.:./config
# Add jars in lib dir
for jar in `ls lib`; do
	CLASSPATH=$CLASSPATH:./lib/$jar
done

export CLASSPATH

LANG=ja_JP.UTF-8
export LANG

PATH=$JAVA_HOME/bin:$PATH
export PATH
