#!/bin/bash

function badcmdargs () {
    echo "usage: $0 <number of concurrent clients> <loop count> <transactions per second> <mode>" >&2
    exit 1
}

LOGDIR=logs

PROC_COUNT=$1
LOOPCOUNT=$2
TPS=$3
MODE=$4

test $# -ne 4 && badcmdargs

mkdir -p "${LOGDIR}-${PROC_COUNT}"

for (( i=0; i < $PROC_COUNT; i++ ))
do
    ant "-Dtest-filename=${LOGDIR}-${PROC_COUNT}/${i}.txt" \
        "-Dtest-tps=$TPS" \
        "-Dtest-loop-count=$LOOPCOUNT" \
        "-Dtest-setting=$MODE" \
        test-client > "${LOGDIR}-${PROC_COUNT}/${i}-out.txt" &
done

echo "Running $PROC_COUNT concurrent clients."
wait
