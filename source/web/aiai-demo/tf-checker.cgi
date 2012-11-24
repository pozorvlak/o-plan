#!/bin/bash

# echo Content-type: text/html
# echo

cd ../..

exec ./web/aiai-demo/oplan-cgi \
	-load web/demo/get-url-support \
	-load web/demo/tf-checker-support \
	-do "(tf-checker-cgi)"
