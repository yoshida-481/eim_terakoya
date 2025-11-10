#!/bin/sh

SCRIPT_NAME=`basename $0`
DIR_NAME=`dirname $0`

function usage() {
	echo "usage: sh $SCRIPT_NAME [-help] [-target attributes|fullText|parentRef|adminRef]"
}

if [ -z $1 ]; then
	usage
	exit 1
fi
if [ -z $2 ]; then
	usage
	exit 1
fi
if [ $1 == '-target' ]; then
	if [ $2 == 'attributes' ]; then
		SERVICE_NAME='gatheringContentsAttributesJob'
	elif [ $2 == 'fullText' ]; then
		SERVICE_NAME='gatheringDocsAndFullTextJob'
	elif [ $2 == 'parentRef' ]; then
		SERVICE_NAME='gatheringParentRefJob'
	elif [ $2 == 'adminRef' ]; then
		SERVICE_NAME='gatheringAdminRefJob'
	else
		usage
		exit 1
	fi
else
	usage
	exit 1
fi

echo "--- start $SCRIPT_NAME ---"

cd $DIR_NAME

# Load sync settings
. $DIR_NAME/internalSetEnv.sh

# Prevent duplicative execute.
if [ $$ != `pgrep -fo $SCRIPT_NAME`  ] && [ $PPID != `pgrep -fo $SCRIPT_NAME`  ];
then
	echo "$SCRIPT_NAME is already running..."
	exit 1
fi

echo "script_name = $SCRIPT_NAME"
echo "dir_name = $DIR_NAME"


# Setting Class Pathes.
. $DIR_NAME/internalSetEnv.sh

# Execute.
# The system property "solr.cloud.client.stallTime" is a stall prevention timeout. (default:15000 ms)
java -cp $CLASSPATH \
		-Dlog4j.configuration=log4j_gatheringJob.xml \
		-Dsolr.cloud.client.stallTime=900000 \
		jp.co.ctc_g.eim.search.core.indexBase.presentation.job.GatheringRefAndDataJob $SERVICE_NAME

echo "--- finish $SCRIPT_NAME ---"
exit $?
