#!/bin/sh
echo "HTTP/1.0 200"
echo "Content-type: multipart/x-mixed-replace;boundary=---ThisRandomString---"
echo ""
echo "---ThisRandomString---"
while true
do
echo "Content-type: text/html"
echo ""
echo "<h2>Processes on this machine updated every 5 seconds</h2>"
echo "time: "
date
echo "<p>"
echo "<xmp>"
ps -el
echo "</xmp>"
echo "---ThisRandomString---"
sleep 5
done
