#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

OLDEST=$(pgrep -fo $0)
if [ $$ != $OLDEST ] && [ $PPID != $OLDEST ]; then
    echo "既にバッチが起動済みです。"
    exit
fi

# Execute.
java -cp $CLASSPATH -Djava.net.preferIPv4Stack=true -Djgroups.tcp.address=127.0.0.1 -Djgroups.mping.mcast_addr=228.2.4.6 batch.PDFConvertWatcher

