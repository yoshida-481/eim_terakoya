#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Execute.
java -Xmx256m -cp $CLASSPATH -Djava.net.preferIPv4Stack=true -Djgroups.tcp.address=127.0.0.1 -Djgroups.mping.mcast_addr=228.2.4.6 jp.co.ctc_g.eim.app.document.presentation.batch.FileCreateWatcher jp.co.ctc_g.eim.app.document.presentation.batch.PDFRegister
