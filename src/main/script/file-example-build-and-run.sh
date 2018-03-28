#!/bin/bash

LOG_FILE='/tmp/file-example.log'
if [ -n "$1" ]
then
	LOG_FILE="$1"
fi

( echo '------------' ; date ; echo '' ) > "${LOG_FILE}"
mvn clean verify &&
	echo "Tail log-file to see output: ${LOG_FILE}" && (
	(
		src/main/script/file-example.sh -l 1000
		echo ''
		date
		echo '------------'
	) >> "${LOG_FILE}" 2>&1
)
