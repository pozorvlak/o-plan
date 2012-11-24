#!/bin/sh

# echo Content-type: text/plain
# echo

cd ../..

exec ./web/aiai-demo/remote-oplan-cgi \
	-load web/demo/get-url-support \
	-do "(get-http-url-cgi)"
