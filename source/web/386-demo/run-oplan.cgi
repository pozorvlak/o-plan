#!/bin/bash

echo Content-type: text/html
echo

cd ../..

export DISPLAY PATH
DISPLAY=:0.0
PATH=$PATH:/usr/X386/bin

tmpfile=../web-tmp/run-oplan-$$

# Run O-Plan in the background.
# Note that we have to redirect output so Mosaic doesn't wait for
# O-Plan to finish.

./oplan-386 -eval "(protect-terminal-io)" > $tmpfile &

echo "<P>" Done!
