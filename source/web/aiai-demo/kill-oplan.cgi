#!/bin/sh

# Edit this to kill a user=oplan process that must die.

enemy_pid=7343

echo Content-type: text/html
echo

echo "<TITLE>Kill runaway process</TITLE>"
echo "<H1>Kill runaway process</H1>"

cd ../..

# Uses kcl-image because it's smaller and may already be running
# (the runaway is probably a recent, http-based, demo, hence running
# in GCL (= kcl-image)).  Also, we can do more C-like stuff in GCL
# more easily than in the other Lisps.

# exec ./web/aiai-demo/remote-oplan-cgi kcl-image \
# 	-load web/demo/kill-runaway-process \
# 	-enemy-pid $enemy_pid \
# 	-do "(kill-runaway-process)"

exec ./web/aiai-demo/oplan-cgi kcl-image \
	-load web/demo/kill-runaway-process \
	-enemy-pid $enemy_pid \
	-do "(kill-runaway-process)"
