#!/bin/bash

echo Content-type: text/html
echo

cd ../..

exec ./web/386-demo/oplan-cgi \
	-load web/demo/cgi-matrix-support \
	-load web/demo/pacifica-coas-support \
	-do "(pacifica-coas-cgi)"
