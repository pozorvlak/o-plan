;;;; File: web-config.lisp
;;; Contains: Spottisvax configuration for the Web / CGI demos
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1997
;;; Updated: Tue Mar 16 22:21:11 1999 by Jeff Dalton
;;; Copyright: (c) 1997 AIAI, University of Edinburgh

(in-package :oplan)

(setq *web-demo-dir-url*
      "http://localhost/~jeff/oplan/web/demo")

(setq *web-special-url*
      "http://localhost/~jeff/oplan/web/386-demo")

(setq *web-cgi-url*
      "http://localhost/cgi-bin/oplan-demo/386-demo")

;;; /\/: Or it could be:
#+:undef
(setq *web-cgi-url*
      "http://localhost/~jeff/oplan/web/386-demo")

;;; End

