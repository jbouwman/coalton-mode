(in-package :coalton-mode)

(defun %log (level message args)
  (apply #'format t
         (concatenate 'string ";; ~s coalton-mode: " message "~%")
         level args))

(defun /trace (message &rest args)
  #++ (%log :trace message args))

(defun /debug (message &rest args)
  (%log :debug message args))

(defun /info (message &rest args)
  (%log :info message args))

(defun /warn (message &rest args)
  (%log :warn message args))

(defun /error (message &rest args)
  (%log :error message args))
