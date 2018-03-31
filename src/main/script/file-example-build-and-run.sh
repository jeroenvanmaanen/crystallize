#!/bin/bash

BIN="$(cd "$(dirname "$0")" ; pwd)"

LOG_FILE='/tmp/file-example.log'
LIMIT='1000'

while [ ".${1:0:1}" = '.-' ]
do
    OPT="$1"
    shift
    [ ".${OPT}" != '.--' ] || break
    case "${OPT}" in
    -f|--file)
        LOG_FILE="$1"
        shift
        ;;
    -l|--limit)
        LIMIT="$1"
        shift
        ;;
    *)
        echo "Unknown option: [${OPT}]"
        exit 1
    esac
done

( echo '------------' ; date ; echo '' ) > "${LOG_FILE}"
mvn clean verify &&
	echo "Tail log-file to see output: ${LOG_FILE}" && (
	(
		"${BIN}/file-example.sh" -l "${LIMIT}"
		echo ''
		date
		echo '------------'
	) >> "${LOG_FILE}" 2>&1
)
