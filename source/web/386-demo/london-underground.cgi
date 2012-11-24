#!/bin/bash

echo Content-type: text/html
echo

echo "<TITLE>Your route as a plan</TITLE>"
echo "<H1>Your route as a plan</H1>"

cd ../..

echo "<H2>Demo output</H2>"

exec ./web/386-demo/oplan-cgi \
	-load web/demo/london-underground-support \
	-do "(london-underground)"
