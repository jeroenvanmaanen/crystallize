#!/bin/bash

SILENT='true'
if [ ".$1" = '.-v' ]
then
    shift
    SILENT='false'
fi

function log() {
    "${SILENT}" || echo ">>> $@" >&2
}

function format-libraries() {
    local MVN_REPO="$1"
    local GROUP
    local IFS=':'
    while read GROUP_ID ARTIFACT TYPE VERSION SCOPE
    do
        GROUP="$(echo "$GROUP_ID" | tr '.' '/')"
        echo -n ":${MVN_REPO}/${GROUP}/${ARTIFACT}/${VERSION}/${ARTIFACT}-${VERSION}.jar"
    done
}

JAVA_HOME="${JAVA_HOME-}"
if [ -z "${JAVA_HOME}" ]
then
    echo "Environment variable JAVA_HOME does not have a value" >&2
    exit 1
fi

SCRIPT="$(cd "$(dirname "$0")" ; pwd)"
MAIN="$(dirname "${SCRIPT}")"
SRC="$(dirname "${MAIN}")"
PROJECT="$(dirname "${SRC}")"
CLASSES="${PROJECT}/target/classes"
RESOURCES="${MAIN}/resources"

MVN_REPO="$(cd "${PROJECT}" ; mvn -X 2>&1 | sed -n -e 's/^.* Using local repository at //p')"
log "MVN_REPO=[${MVN_REPO}]"

LIBRARIES="$(mvn dependency:list 2>&1 | sed -n -e '/:slf4j-jdk14:/s/:test$/:compile/' -e '/:test$/d' -e '/:.*:.*:.*:/s/^[[]INFO[]]    //p' | format-libraries "${MVN_REPO}")"
CLASS_PATH="${CLASSES}:${RESOURCES}${LIBRARIES}"
log "CLASS_PATH=[${CLASS_PATH}]"

(
    cd "${PROJECT}"
    "${JAVA_HOME}/bin/java" -cp "${CLASS_PATH}" -Djava.util.logging.config.file="${SRC}/test/resources/logging.properties" org.leialearns.crystallize.interaction.FileExample "$@" data/kjv-10-simplified.txt
)