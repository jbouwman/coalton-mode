;;; Behavior common to classes that manage processes

(in-package :coalton-mode)

(defgeneric start (process)
  (:documentation "Start a process. The started process is returned."))

(defgeneric name (process)
  (:documentation "Process name.")
  (:method (process) "Coalton LSP Process"))

(defgeneric run (process)
  (:documentation "Run a process. This function is run by a newly started thread. If the function returns, the thread will halt."))

(defgeneric stop (process)
  (:documentation "Stop a process. The stopped process is returned."))

(defclass process ()
  ((thread :initform nil)
   (lock :initform (bt:make-recursive-lock))))

(defmacro with-lock-held ((process) &body scope)
  `(bt:with-recursive-lock-held ((slot-value ,process 'lock))
     ,@scope))

(defmethod start ((self process))
  (format t ";; Start ~a~%" self)
  (with-slots (thread) self
    (setf thread
          (bt:make-thread (lambda ()
                            (run self))
                          :name (name self))))
  self)


