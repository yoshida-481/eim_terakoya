#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Execute.
java -cp $CLASSPATH batch.maintenance.FolderPathCheck
