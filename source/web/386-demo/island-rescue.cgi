#!/bin/bash

echo Content-type: text/html
echo

echo "<TITLE>Island Rescue results</TITLE>"
echo "<H1>Island Rescue results</H1>"

cd ../..

# echo "<H2>Environment</H2>"
# 
# echo "<LISTING>"
# 
# echo "pwd = " `pwd`
# 
# echo
# echo "printenv:"
# printenv
# 
# echo
# echo Argv[0]=$0
# echo Args: "$@"
# 
# echo "</LISTING>"

echo "<H2>Demo output</H2>"

exec ./web/386-demo/oplan-cgi \
	-load web/demo/island-rescue-support \
	-do "(island-rescue)"
