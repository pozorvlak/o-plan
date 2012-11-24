#!/bin/bash

# Runs O-Plan to fork an HTTP server on a new port.

# # /\/: We'll do the initial output for now, so "Location:"
# # redirection won't work.

# echo Content-type: text/html
# echo

cd ../..

# /\/ Spec localhost for now to avoid confusing applet
# /\/ May instead want to change web-config to say spottisvax.nonet.

exec ./web/386-demo/oplan-cgi \
	-load-system matrix-server-support \
	-server-host localhost \
	-do "(server-cgi)"
