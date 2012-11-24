#!/bin/sh

echo Content-type: text/html
echo

echo "<TITLE>TPN results</TITLE>"
echo "<H1>TPN results</H1>"

cd ../..

# /\/ kcl-image because int-char isn't working in Lucid and is
# used in :text arg conversion.

exec ./web/aiai-demo/remote-oplan-cgi kcl-image \
	-load web/demo/simple-tpn-support \
	-do "(simple-tpn)"
