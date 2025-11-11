#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Execute.
java -cp $CLASSPATH -Doracle.jdbc.autoCommitSpecCompliant=false -Djava.net.preferIPv4Stack=true -Djgroups.tcp.address=127.0.0.1 -Djgroups.mping.mcast_addr=228.2.4.6 batch.ScheduledMail
