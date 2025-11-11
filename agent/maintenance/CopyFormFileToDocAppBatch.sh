#!/bin/sh

cd `dirname $0`
#cd /home/jboss/eim/CopyFormFileToDocAppBatch

# Setting Class Pathes.
. ./internalSetEnv.sh

# Count Process
RETVAL=0
RETVAL=`ps aux | grep batch.daihatsu.copyformfile.CopyFormFileToDocAppBatch | grep -v grep | wc -l`

if [ $RETVAL -ge 1 ]; then
  echo $"process already running... "
else
  # Execute.
  java -cp $CLASSPATH jp.co.daihatsu.eim.app.document.presentation.batch.CopyFormFileToDocAppBatch
fi