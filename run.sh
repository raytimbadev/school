#!/bin/bash

trap "echo quitting; exit 1;" SIGINT

test -z "$1" && {
    echo "No run type specified." >&2
    exit 1
}

case "$1" in
    "client")
        CP=build/client
        MAIN=client.Client
        ;;

    "middleware")
        CP=build/middleware
        MAIN=middleware.Middleware
        ;;

    "server")
        CP=build/server
        MAIN=server.sockets.Server
        ;;

    *)
        echo "Unknown type $1". >&2
        exit 1
        ;;
esac

shift 1

i=1
while test $i -ne 0
do
    echo "(RE)STARTING"
    java -classpath $CP $MAIN $@
    i=$?
done
