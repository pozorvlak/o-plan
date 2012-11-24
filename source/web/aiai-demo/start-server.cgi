#!/bin/bash

# Runs O-Plan to fork an HTTP server on a new port.

# # /\/: We'll do the initial output for now, so "Location:"
# # redirection won't work.

# echo Content-type: text/html
# echo

echo Content-type: text/html
echo

cd ../..

exec ./web/aiai-demo/remote-oplan-cgi kcl-image \
	-load web/demo/http-server \
	-do "(start-server)"
