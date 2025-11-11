#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

# Setting target folder Ids
UPDATE_ATTR_FOLDER_ID=4644019,3271939,4644872,4157993,1470540,3173675,1478667
#UPDATE_ATTR_FOLDER_ID=4172593

# Setting dummyUser code
ALL_APPROVE_USER_CODE=dmcsys01

java -cp $CLASSPATH batch.PublishDocumentBatch $UPDATE_ATTR_FOLDER_ID $ALL_APPROVE_USER_CODE
