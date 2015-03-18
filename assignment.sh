#!/bin/bash

AUTHOR="Jacob Thomas Errington (260636023)"

COURSEDIR="$1"

if test -z "$COURSEDIR" ; then
    echo 'No course code given.'
    exit 1
fi

COURSEDATA="$COURSEDIR/course.sh"

if ! test -e "$COURSEDATA" ; then
    echo 'The course does not contain a course.sh data file.'
    exit 1
fi

source "$COURSEDATA"

if test -z "$COURSE" ; then
    echo 'The course.sh data file did not export a COURSE variable.'
    exit 1
fi

if test -z "$CODE" ; then
    echo 'The course.sh data file did not export a CODE variable.'
    exit 1
fi

TYPE="$2"

ASSDIR="$COURSEDIR/assignments"

if [ "$TYPE" = "new" ] ; then
    ASSNO=1

    while test -e "$ASSDIR/$ASSNO" ; do
        ASSNO=$(( $ASSNO + 1 ))
    done
else
    # this should be a number, but could be anything as long as it's not the
    # string "new"
    ASSNO="$TYPE"
fi

mkdir -p "$ASSDIR/$ASSNO"

start_assignment --author "$AUTHOR" --code "$CODE" --course "$COURSE" \
    --number "$ASSNO" $PROGRAMMING $MATH > "$ASSDIR/$ASSNO/writeup.tex"

echo "$ASSDIR/$ASSNO"
