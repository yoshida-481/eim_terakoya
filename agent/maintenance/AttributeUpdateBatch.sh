#!/bin/sh
cd `dirname $0`

# Setting Class Pathes.
. ./internalSetEnv.sh

#Check Target Folder
java -cp $CLASSPATH batch.CheckTargetBatch attributeUpdate

echo -n "Processing starts ok? [y/n]:"
read yn


if [ "${yn}" = "Y" -o "${yn}" = "y" ]

	then
		# Execute.
		java -cp $CLASSPATH batch.AttributeUpdateBatch
	
	else
		# End.
		echo -n " Please press enter..."
		read x

fi
