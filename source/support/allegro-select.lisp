;;; Select in Allegro
;;; Jeff Dalton, May 1999

(in-package :oplan-util)

(defun real-select (streams &optional timeout)
  (mp:wait-for-input-available streams :timeout timeout))

(defun real-select-p (streams &optional timeout)
  (mp:wait-for-input-available streams :timeout timeout))

(setf (symbol-function 'pprocess:select-input)
      #'real-select)

(setf (symbol-function 'pprocess:select-input-p)
      #'real-select-p)

(set-parameter :select-type :real)
