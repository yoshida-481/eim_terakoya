#!/bin/sh
# Box Archive Batch (File Delete) Shell Script
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Execute.
java -cp $CLASSPATH -Dlog4j.configuration=log4j_boxArchive.xml jp.co.ctc_g.eim.framework2.presentation.job.box.PurgeArchivedJob

exit $?
