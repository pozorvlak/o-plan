# File: Makefile
# Author: Jeff Dalton <J.Dalton@ed.ac.uk>
# Created: January 1997
# Updated: Thu May 18 16:45:33 2000 by Jeff Dalton
# Copyright: (c) 1993, 1994, 2000 AIAI, University of Edinburgh

# >>> This is the wrong Makefile for compiling and building O-Plan.
# >>> Use source/Makefile instead.

# Uses GNU tar.  Unfortunately, it's not always called "tar" or "gnutar"
# or any other name that we can rely on.

release:
	${MAKE} TarFile=../oplan-3.3-`date +%d%h%y`.tar.gz backup

backup:
	gtar zcvf ${TarFile} .

Objects=/tmp/oplan-objects

prebuilt:
	rm -f ${Objects}
	echo bin >  ${Objects}
	echo lib >> ${Objects}
	find source \( -name "*.o" -o -name "*.sbin" -o -name "*.s2bin" \
                       -o -name "*.fasl" \) \
           -print | sed "s:^./::" >> ${Objects}
	gtar zcvTf ${Objects} ../oplan-3.3-bin-`date +%d%h%y`.tar.gz 
