#!/bin/bash

# Runs O-Plan to fork an HTTP server on a new port.  O-Plan will use
# a "Location:" header in the response to redirect the browser to the
# server.  See web/demo/http-server.lisp.

# Updated: Thu Apr  8 04:54:44 1999 by Jeff Dalton

# Note that O-plan has to write the response headers.  When debugging,
# it may be useful to do that here instead, in which case the "Location:"
# header will not take effect (if O-Plan manages to output it).  To
# write the headers here, uncomment the following two lines:

# echo Content-type: text/html
# echo

# remote-oplan-cgi causes the demo to run on a machine other than
# www.aiai.ed.ac.uk.  To avoid this leap, exec oplan-cgi instead.
# You will also have to have a command-line-argument:
#    -server-host www.aiai.ed.ac.uk

# Note that you cannot specify an arbitrary server.  It must be the
# same machine that runs O-Plan as a CGI program, because the server
# is created by forking.

# The current "remote" machine is Gairsay (a.k.a. "oplan"),
# but see oplan-remote-cgi.

cd ../..

#exec ./web/aiai-demo/remote-oplan-cgi kcl-image \
#	-load-system matrix-server-support
#	-server-host oplan.aiai.ed.ac.uk \
#	-do "(server-cgi)" # -server-host www.aiai.ed.ac.uk

exec ./web/aiai-demo/oplan-cgi kcl-image \
	-load-system matrix-server-support \
	-server-host www.aiai.ed.ac.uk \
	-do "(server-cgi)"
