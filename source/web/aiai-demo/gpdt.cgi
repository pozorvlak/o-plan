#!/bin/bash

echo Content-type: text/html
echo

cd ../..

exec ./web/aiai-demo/remote-oplan-cgi \
	-load web/demo/cgi-matrix-support \
	-load web/demo/gpdt-support \
	-do "(gpdt-cgi)"
