#!/bin/sh

echo Content-type: text/html
echo

echo "<TITLE>Demo plan results</TITLE>"
echo "<H1>Demo plan results</H1>"

cd ../..

# echo PATH_INFO = $PATH_INFO

exec ./web/aiai-demo/remote-oplan-cgi \
	-load web/demo/demo-tf-support \
	-do "(demo-tf)"
