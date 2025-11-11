#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Execute.
java -cp $CLASSPATH batch.maintenance.CreateDummyFile 原本ドキュメント /tmp/dummyFile.txt

#java -cp $CLASSPATH batch.maintenance.CreateDummyFile 公開ドキュメント /tmp/dummyFile.txt
