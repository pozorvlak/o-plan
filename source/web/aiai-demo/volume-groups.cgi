#!/bin/bash

echo Content-type: text/html
echo

echo "<TITLE>VG results</TITLE>"
echo "<H1>VG results</H1>"

cd ../..

exec ./web/aiai-demo/remote-oplan-cgi \
	-load web/demo/volume-groups-support \
	-do "(volume-groups-cgi)"
