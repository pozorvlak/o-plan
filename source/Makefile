# File: Makefile
# Contains: Source code maintenance and development makefile
# Author: Jeff Dalton <J.Dalton@ed.ac.uk>
# Created: 15 April 1993
# Updated: Sat Feb  2 01:44:04 2008 by Jeff Dalton
# Copyright: (c) 1993 - 1999, AIAI, University of Edinburgh

# This Makefile can be used to perform various operations on the
# "source" subdirectory of the O-Plan file tree.  It can be used to
# compile O-Plan, to install files in the right part of the O-Plan 
# tree, and to make and unpack backup copies of the sources.

# The Lucid targets (such as lucid-build) use Lucid-specific command-line
# arguments; the KCL targets are much less KCL-specific and are therefore
# easier to generalize.  The "generic-" targets are one such generalization.
# The "386" targets are for KCL with 386BSD, NetBSD, and the like.

# /\/: Need to get Lucid to exit with bad status if something goes wrong.
# Right now, a "make lucid-oplan" might continue after compiling,
# no matter what.

# Edit the following line(s) to define commands for running Lisp.
Allegro = /usr/local/acl/5.0/lisp
Lucid   = lucid4.1+clos
KCL     = gcl
GCL     = gcl
SBCL    = sbcl

# This is the default lisp for the "generic" make targets:
Lisp = ${SBCL}

# Here are some default image names:
Lucid-image   = lucid-image
KCL-image     = kcl-image
Allegro-image = allegro-image.dxl
SBCL-image    = sbcl-image

# "make all" and "make install" assume Allegro Common Lisp.
#
# To make a Lucid version, use
#
#    make Target=lucid-oplan Image=lucid-image all install
#
# To make a KCL version, use
#
#    make Target=kcl-oplan Image=kcl-image all install
#
# To explicitly make an Allegro version use
#
#    make Target=allegro-oplan Image=allegro-image.dxl all install
#

Base-dir  = ..
Lib-dir   = ${Base-dir}/lib
Bin-dir   = ${Base-dir}/bin

Image = ${SBCL-image}
Target = sbcl-oplan

AuxProgs = xmenu/xmenu control-panel/xuim

all:
	${MAKE} ${AuxProgs}
	${MAKE} ${Target}

install: ${Lib-dir}
	${MAKE} install-aux
	mv ${Image} ${Lib-dir}

install-aux: ${Lib-dir}
	cp -p config/* ${Lib-dir}
	/bin/rm -f ${Lib-dir}/*~
	-mkdir ${Bin-dir}
	cp -p ${AuxProgs} ${Bin-dir}
	cp -p scripts/* ${Bin-dir}
	/bin/rm -f ${Bin-dir}/*~

${Lib-dir}:
	mkdir ${Lib-dir}


# Release date
#
# Writes something like (defvar *oplan-release-date* "29-Oct-94")
# to a new oplan-release-date.lsp.
#
# %h works for the month with both SunOS 4.1.3 and FreeBSD.
#
rdate:
	rm -f oplan-release-date.lsp
	echo '(in-package :user)' > oplan-release-date.lsp
	echo '(defvar *oplan-release-date* "'`date +%d-%h-%y`'")' \
		>> oplan-release-date.lsp


# For gcc, when /tmp is too small, setenv TMPDIR = .
# This can happen when using KCL.

# Compile and build combinations

lucid-oplan:
	${MAKE} lucid-compile-everything
	${MAKE} lucid-build

kcl-oplan:
	${MAKE} KCL=${KCL} kcl-compile-everything
	${MAKE} KCL=${KCL} kcl-build

allegro-oplan:
	${MAKE} allegro-compile-everything
	${MAKE} allegro-build

sbcl-oplan:
	${MAKE} sbcl-compile-everything
	${MAKE} sbcl-build


# Solaris 2 Lucid at AIAI

solaris-compile-warnings:
	${MAKE} Lucid=random/lucid4.1.1+clos lucid-compile-warnings

solaris-compile-everything:
	${MAKE} Lucid=random/lucid4.1.1+clos lucid-compile-everything

solaris-build:
	${MAKE} Lucid=random/lucid4.1.1+clos Lucid-image=lucid2-image \
			lucid-build

solaris-clean:
	${MAKE} Lucid=random/lucid4.1.1+clos Lucid-image=lucid2-image \
			lucid-clean


# Liquid (formarly Lucid) CL

# /\/: Liquid will eventually replace the Lucid and Solaris sections.

liquid-compile:
	${MAKE} Lucid=lcl-5_0-clos lucid-compile

liquid-compile-warnings:
	${MAKE} Lucid=lcl-5_0-clos lucid-compile-warnings

liquid-compile-everything:
	${MAKE} Lucid=lcl-5_0-clos lucid-compile-everything

liquid-build:
	${MAKE} Lucid=lcl-5_0-clos lucid-build

liquid-clean:
	${MAKE} Lucid=lcl-5_0-clos lucid-clean


# Image-building

lucid-build:
	${Lucid} -load oplan.lsp \
		 -eval "(load-system 'oplan)" \
		 -eval "(oplan:save-oplan \"${Lucid-image}\")" \
		 -quit

kcl-build:
	echo "(load \"oplan.lsp\") \
	      (load-system 'oplan) \
	      (oplan:save-oplan \"${KCL-image}\") \
	      (bye)" \
	  | ${KCL}

kcl-build-386:
	${MAKE} KCL-image=/usr/tmp/kcl-image kcl-build

gcl-build:
	${MAKE} KCL=${GCL} kcl-build

allegro-build:
	${MAKE} Lisp=${Allegro} Image=${Allegro-image} generic-build

sbcl-build:
	${MAKE} Lisp=${SBCL} Image=${SBCL-image} generic-build

generic-build:
	echo "(load \"oplan.lsp\") \
	      (load-system 'oplan) \
	      (oplan:save-oplan \"${Image}\") \
	      (oplan-util:exit-lisp)" \
	  | ${Lisp}


# Compilation

lucid-compile:
	${Lucid} -load oplan.lsp \
		 -eval "(compile-system 'oplan)" \
		 -quit

lucid-compile-everything:
	${Lucid} -load oplan.lsp \
		 -eval "(compile-system 'everything)" \
		 -quit

lucid-compile-warnings:
	${Lucid} -load oplan.lsp \
		 -eval "(setq *break-on-warnings* t)" \
		 -eval "(compile-system 'everything)" \
		 -quit

lucid-compile-changed-files:
	${Lucid} -load oplan.lsp \
		 -eval "(setq *break-on-warnings* t)" \
		 -eval "(compile-system 'everything :ignore-depends t)" \
		 -quit

kcl-compile:
	echo "(load \"oplan.lsp\") \
	      (si:use-fast-links t) \
	      (compile-system 'oplan) \
	      (bye)" \
	  | ${KCL}

kcl-compile-everything:
	echo "(load \"oplan.lsp\") \
	      (si:use-fast-links t) \
	      (compile-system 'everything) \
	      (bye)" \
	  | ${KCL}

gcl-compile:
	${MAKE} KCL=${GCL} kcl-compile

gcl-compile-everything:
	${MAKE} KCL=${GCL} kcl-compile-everything

allegro-compile-everything:
	${MAKE} Lisp=${Allegro} generic-compile-everything

allegro-compile-warnings:
	${MAKE} Lisp=${Allegro} generic-compile-warnings

sbcl-compile-everything:
	${MAKE} Lisp=${SBCL} generic-compile-everything

sbcl-compile-warnings:
	${MAKE} Lisp=${SBCL} generic-compile-warnings

generic-compile-everything:
	echo "(load \"oplan.lsp\") \
	      (compile-system 'everything) \
	      (oplan-util:exit-lisp)" \
	  | ${Lisp}

generic-compile-warnings:
	echo "(load \"oplan.lsp\") \
	      (setq *break-on-warnings* t) \
	      (compile-system 'everything) \
	      (oplan-util:exit-lisp)" \
	  | ${Lisp}


# Clean

lucid-clean:
	${Lucid} -load oplan.lsp \
		 -eval "(clean-system 'everything)" \
		 -quit

kcl-clean:
	echo "(load \"oplan.lsp\") \
	      (clean-system 'everything) \
	      (bye)" \
	  | ${KCL}

allegro-clean:
	${MAKE} Lisp=${Allegro} generic-clean

sbcl-clean:
	${MAKE} Lisp=${SBCL} generic-clean

generic-clean:
	# Loads dependent-support to get util:exit-lisp.
	echo "(load \"oplan.lsp\") \
	      (load-system 'dependent-support) \
	      (clean-system 'everything) \
	      (oplan-util:exit-lisp)" \
	  | ${Lisp}

clean-tilde-files:
	find . -print | grep "~" | xargs rm

# Auxiliary programs

control-panel/xuim:
	(cd control-panel; xmkmf; make)

xmenu/xmenu:
	(cd xmenu; xmkmf; make)


# Some transformations

.SUFFIXES: .xml .html

.xml.html: $< xml/plan-to-html.xsl
	rsp $< xml/plan-to-html.xsl > $@


# Other useful things

# BSD "make" handles "TAGS: *.lsp */*.lsp */*/*.lsp", but others may not.
# In that case, use "make newTAGS".

TAGS: *.lsp */*.lsp */*/*.lsp
	etags *.lsp */*.lsp */*/*.lsp

newTAGS:
	etags *.lsp */*.lsp */*/*.lsp


# Release preparation
#
# "make pre-release" is used when constructing a directory that contains
# only the release.  It's chief task is to move some things into better
# locations in order to simplify the building and installation process
# later on.

pre-release:
	mv -i web aiai-web


# Tarfile construction for backup and release.
#
# Many of the make targets in this part of the Makefile are chiefly
# for "internal" use by the Makefile itself.  External targets are:
# 
#   release
#   backup
#   file-backup
#   386-backup
#
# "backup" includes more than "release"
#
# The following targets can be used to extract files from the tarfiles.
#
#   restore
#   386-restore
#
# The targets that construct tarfiles determine what files to include
# by an inverse method: construct a list of files to _exclude_ and then
# include everything else.  The "exclude" file is for use with the
# GNU tar "X" option.
#
# This approach may seem bizarre, but it worked on the machines where
# I needed it to work, when a number of more obvious things didn't, and
# it's easier and more reliable than maintaining by hand a list of all
# files to include.  The most likely failure is to get some extra
# files, which is better than leaving some out.
#
# Note that an entire backup or release tarfile fits on one floppy.


# Making a "release" .tar file

# Note that this still uses compress rather than gzip.

TAR = tar
RelExclude = /tmp/oplan-release-exclude
RelTar = ../release.tar.gz

release:
	${MAKE} release-exclude
	rm -f ${RelTar}
	${TAR} zcvXf ${RelExclude} ${RelTar} .

# /\/: Not clear that we can exclude random, now that we expect
# people to use the release with GCL, because some things in random
# are used.

release-exclude:
	rm -f ${RelExclude}
	echo transfer-log > ${RelExclude}
	echo Notes >> ${RelExclude}
	echo TAGS >> ${RelExclude}
	echo core >> ${RelExclude}
	# echo tmp >> ${RelExclude}
	echo random >> ${RelExclude}
	# echo test-tf >> ${RelExclude}
	# echo test-results >> ${RelExclude}
	echo experimental-tf >> ${RelExclude}
	echo obsolete >> ${RelExclude}
	# echo web >> ${RelExclude}
	echo worldsim >> ${RelExclude}
	echo execlet >> ${RelExclude}
	echo ex-patch >> ${RelExclude}
	echo shared >> ${RelExclude}
	echo tf-manual >> ${RelExclude}
	${MAKE} Dir=doc Exclude=${RelExclude} tex-exclude
	echo doc/plug-in-cms-in-3-1.tex >> ${RelExclude}
	echo noplan >> ${RelExclude}
	echo oplan-386 >> ${RelExclude}
	echo oplan-init >> ${RelExclude}
	echo oplan-header >> ${RelExclude}
	# echo xmenu/Makefile >> ${RelExclude}
	# find . -name "SCCS"    -print | sed "s:^./::" >> ${RelExclude}
	find . -name "*~"      -print | sed "s:^./::" >> ${RelExclude}
	${MAKE} Exclude=${RelExclude} exclude-objects
	${MAKE} Exclude=${RelExclude} tex-exclude
	# Now exclude executable non-directories that are > 8K bytes.
	# The idea is that these will not be shell scripts.
	find . -type f -perm -100 -size +16 -print \
	   | sed "s:^./::" >> ${RelExclude}


# A "demo release" is for one of us to take somewhere.  So it has
# more than a proper release but less than a backup.

demo-release:
	${MAKE} Exclude=${RelExclude} exclude
	echo Notes >> ${RelExclude}
	echo web >> ${RelExclude}
	${TAR} zcvXf ${RelExclude} ../demo-release.tar.gz .


# Floppy backup and restore

# Used to have a common backup target and have 386-backup do
#   ${MAKE} TarFile=/dev/rfd0a backup
# but the Suns blow it on writing the floppy if we compress.

TarFile = /dev/rfd0

Exclude = /tmp/jeffs-exclude

TarTemp = ../backup.tar.gz

backup:
	${MAKE} exclude
	rm -f ${TarTemp}
	${TAR} zcvXf ${Exclude} ${TarTemp} .
	dd if=${TarTemp} of=${TarFile} bs=9k conv=sync
	echo `date` backup on `hostname` >> transfer-log

# Solaris is a major pain all on its own.
# N.B. do "volcheck" before and "eject" after

solaris-backup:
	${MAKE} TarFile=/vol/dev/diskette0/unlabeled backup

file-backup:
	${MAKE} TarFile=../source.`date +%d%h%y`.tar.gz nice-backup
	echo `date` file backup on `hostname` >> transfer-log

# /\/: should the %h above be %b -- see man 3 strftime ?

active-backup:
	${MAKE} Exclude=${Exclude} exclude
	${TAR} -zcvX ${Exclude} -f ../active.`date +%d%h%y`.tar.gz \
		Makefile oplan-release-date.lsp \
		Notes/.log web
	echo `date` active backup on `hostname` >> transfer-log

# Once active:
#		Makefile Notes/.log web support \
#		system-definitions.lsp oplan.lsp \
#		random top Notes/old-coa-visibility

386-backup:
	${MAKE} TarFile=/dev/rfd0a nice-backup
	echo `date` 386 backup on `hostname` >> transfer-log

nice-backup:
	${MAKE} Exclude=${Exclude} exclude
	${TAR} -zcvX ${Exclude} -f ${TarFile} .

exclude:
	rm -f ${Exclude}
	echo transfer-log > ${Exclude}
	echo TAGS >> ${Exclude}
	echo core >> ${Exclude}
	echo tmp >> ${Exclude}
	echo test-results >> ${Exclude}
	echo imports >> ${Exclude}
	# echo xmenu >> ${Exclude}
	echo random/webdoc >> ${Exclude}
	echo shared >> ${Exclude}
	echo tf-manual >> ${Exclude}
	echo worldsim >> ${Exclude}
	echo obsolete >> ${Exclude}
	echo execlet >> ${Exclude}
	# find . -name "SCCS"   -print | sed "s:^./::" >> ${Exclude}
	${MAKE} Exclude=${Exclude} exclude-objects
	${MAKE} Dir=doc Exclude=${Exclude} tex-exclude
	${MAKE} Dir=tf-manual Exclude=${Exclude} tex-exclude
	# Emacs backup files can make it too large for one floppy.
	find . -name "*~"     -print | sed "s:^./::" >> ${Exclude}
	# Now exclude executable non-directories that are > 8K bytes.
	# The idea is that these will not be shell scripts.
	# find . -type f -perm -u+x -size +16 -print \
	#    | sed "s:^./::" >> ${Exclude}
	find . -type f -perm -100 -size +16 -print \
	   | sed "s:^./::" >> ${Exclude}
	# xmenu has an Imakefile, so we don't want the Makefile
	# echo xmenu/Makefile >> ${Exclude}

exclude-objects:
	find . \( -name "*.o" -o -name "*.sbin" -o -name "*.s2bin" \
                  -o -name "*.fasl" -o -name "gazonk*" -o -name "*.dxl" \) \
	   -print | sed "s:^./::" >> ${Exclude}
	find execlet \( -name "*.o" \
                  -o -name "*.class" -o -name "*.javaimport" \) \
	   -print | sed "s:^./::" >> ${Exclude}


tex-exclude:
	find ${Dir} \
                 \( -name "*.dvi" -o -name "*.aux" -o -name "*.log" \
                    -o -name "*.toc" -o -name "*.idx" \) -print \
	   | sed "s:^./::" >> ${Exclude}

restore:
	echo `date` restore started on `hostname` >> transfer-log
	${TAR} zxvf ${TarFile}
	echo `date` restore finished on `hostname` >> transfer-log

386-restore:
	echo `date` 386 restore started on `hostname` >> transfer-log
	${MAKE} TarFile=/dev/rfd0a restore
	echo `date` 386 restore finished on `hostname` >> transfer-log

# Solaris is again a major pain all on its own.
# N.B. do "volcheck" before and "eject" after

solaris-restore:
	${MAKE} TarFile=/vol/dev/diskette0/unlabeled restore

# End
