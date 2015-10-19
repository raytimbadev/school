#!/bin/bash

CLIENT="client/bin/client"

test \! -x "$CLIENT" && eval "echo 'run make first' ; exit 1"

while read line && read duration ; do
    $CLIENT $duration "$line"
done <<EOF
Captain: What happen ?
1
Mechanic: Somebody set up us the bomb.
2
Operator: We get signal.
1
Captain: What !
1
Operator: Main screen turn on.
2
Captain: It’s you !!
1
CATS: How are you gentlemen !!
2
CATS: All your base are belong to us.
2
CATS: You are on the way to destruction.
2
Captain: What you say !!
1
CATS: You have no chance to survive make your time.
2
CATS: Ha ha ha ha ...
4
Operator: Captain !!
1
Captain: Take off every ‘ZIG’!!
1
Captain: You know what you doing.
2
Captain: Move ‘ZIG’.
1
Captain: For great justice."
1
EOF

wait
