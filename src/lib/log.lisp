(in-package :coalton-mode)

(defun write-timestamp (stream)
  (multiple-value-bind (ss mm hh)
      (decode-universal-time (get-universal-time))
    (format stream "~2,'0d:~2,'0d:~2,'0d" hh mm ss)))

(defvar *context* nil)

(defun add-context (context k f)
  (if (some (lambda (e)
              (eq k (car e)))
            context)
      context
      (nconc context (list (cons k f)))))

(defmacro with-logging-context ((k f) &body body)
  `(let ((*context* (add-context *context* ,k ,f)))
     ,@body))

(defun write-context (stream)
  (dolist (c *context*)
    (funcall (cdr c) stream)
    (write-string " : " stream)))

(defgeneric write-log (destination level format format-args metadata))

(defclass stream-log-destination ()
  ((stream :initarg :stream)
   (level :initform :info
          :initarg :level
          :reader log-level)
   (lock :initform (bt:make-lock))))

(defvar *levels*
  '(:trace :debug :info :warn :error))

(defun level<= (a b)
  (<= (position a *levels*)
      (position b *levels*)))

(defmethod write-log ((self stream-log-destination) level format format-args metadata)
  (with-slots (stream lock) self
    (when (level<= (log-level self) level)
      (bt:with-lock-held (lock)
        (write-string ";; " stream)
        (write-timestamp stream)
        (write-string " : " stream)
        (princ level stream)
        (write-string " : " stream)
        (write-context stream)
        (apply #'format stream format format-args)
        (terpri stream)))))

(defparameter *logger*
  (make-instance 'stream-log-destination :stream t :level ':info))

(defun %log (level message args)
  (write-log *logger* level message args nil))

(defun set-log-level (level)
  (setf (slot-value *logger* 'level) level))

(defun /trace (message &rest args)
  (%log :trace message args))

(defun /debug (message &rest args)
  (%log :debug message args))

(defun /info (message &rest args)
  (%log :info message args))

(defun /warn (message &rest args)
  (%log :warn message args))

(defun /error (message &rest args)
  (%log :error message args))
