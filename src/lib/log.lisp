(in-package :coalton-mode)

(defun %log (level message args)
  (apply #'format t
         (concatenate 'string ";; ~s coalton-mode: " message "~%")
         level args))

(defun /info (message &rest args)
  (%log :info message args))

(defun /warn (message &rest args)
  (%log :warn message args))

(defun /error (message &rest args)
  (%log :error message args))
