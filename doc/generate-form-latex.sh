#!/bin/bash

# Match the part of the line up to the colon as the item to describe, and the
# part after that as the description.
echo '\begin{description}'
cat form-data.txt | perl -pe 's|^(.*?):(.*?)$|\\item\[\1\]\2|'
echo '\end{description}'
