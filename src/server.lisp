;;; Usocket-based TCP server

(in-package :coalton-mode)

(defclass server (process)
  ((config :initarg :config)
   (listener :initform nil)
   (sessions :initform nil))
  (:documentation "A Coalton LSP server"))

(defmethod print-object ((self server) stream)
  (with-slots (config) self
    (destructuring-bind (&key host port &allow-other-keys) config
      (print-unreadable-object (self stream :type t :identity t)
        (format stream "~a:~a" host port)))))

(defmethod run ((self server))
  "Listening socket is open: wait for input, accept, repeat."
  (with-slots (listener sessions) self
    (handler-case
        (loop :do
          (usocket:wait-for-input listener)
          (with-lock-held (self)
            (when (and listener (usocket::state listener))
              (push (start (make-instance 'session
                             :server self
                             :socket (usocket:socket-accept listener :element-type 'character)))
                    sessions))))
      (usocket:bad-file-descriptor-error ()
        nil))))

(defun delete-session (server session)
  (with-slots (lock sessions) server
    (bt:with-recursive-lock-held (lock)
      (delete session sessions))))

(defmethod start ((server server))
  (with-slots (config listener thread) server
    (destructuring-bind (&key interface host port &allow-other-keys) config
      (setf listener (usocket:socket-listen (or interface host) port :reuse-address t))))
  (call-next-method))

(defmethod stop ((self server))
  (with-lock-held (self)
    (with-slots (listener) self
      (when listener
        (usocket:socket-close listener)
        (setf listener nil))))
  self)


