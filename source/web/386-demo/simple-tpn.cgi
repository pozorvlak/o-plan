#!/bin/bash

echo Content-type: text/html
echo

echo "<TITLE>TPN results</TITLE>"
echo "<H1>TPN results</H1>"

cd ../..

exec ./web/386-demo/oplan-cgi \
	-load web/demo/simple-tpn-support \
	-do "(simple-tpn)"
