#!/bin/sh

echo Content-type: text/html
echo

echo "<TITLE>Island Rescue results</TITLE>"
echo "<H1>Island Rescue results</H1>"

cd ../..

exec ./web/aiai-demo/remote-oplan-cgi \
	-load web/demo/island-rescue-support \
	-do "(island-rescue)"
