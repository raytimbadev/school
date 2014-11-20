#!/bin/bash

# Match the part of the line up to the colon. That will go between the curly
# braces in \q{}. The remainder of the line, if any, will go after, as a
# pre-supplied value for that field.
echo '\begin{description}'
cat form-data.txt | perl -pe 's|^(.*?):(.*?)$|\\item\[\1\]\2|'
echo '\end{description}'
