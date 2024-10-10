;;;; TCP LSP server

(in-package :coalton-mode)

(defclass server (process)
  ((session-id :initform 0
               :accessor session-id)
   (config :initarg :config)
   (listener :initform nil)
   (sessions :initform nil))
  (:documentation "A Coalton LSP server"))

(defun server-address (server)
  (with-slots (config) server
    (destructuring-bind (&key host port &allow-other-keys) config
      (format nil "~a:~a" host port))))

(defmethod print-object ((self server) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (write-string (server-address self) stream)))

(defmethod run ((self server))
  "Listening socket is open: wait for input, accept, repeat."
  (with-slots (listener sessions) self
    (handler-case
        (loop :do
          (usocket:wait-for-input listener)
          (with-lock-held (self)
            (when (and listener (usocket::state listener))
              (let ((socket (usocket:socket-accept listener
                                                   :element-type 'character)))
                (/debug "accept connection on ~a" socket)
                (create-session self socket)))))
      (usocket:bad-file-descriptor-error ()
        nil))))

(defun create-session (server socket)
  (push (start (make-instance 'session
                 :id (incf (session-id server))
                 :server server
                 :socket socket))
        (slot-value server 'sessions)))

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
    (with-slots (sessions listener) self
      (when listener
        (usocket:socket-close listener)
        (setf listener nil))
      (dolist (session sessions)
        (stop session))))
  self)

;;; Start and stop the LSP server

(defvar *server* nil
  "The server process.")

(defun start-server (port)
  (cond (*server*
         (/info "server running at tcp:~a" (server-address *server*)))
        ((null port)
         (error "server port must be specified"))
        (t
         (setf *server*
               (start (make-instance 'server
                        :config (list :port port
                                      :host "127.0.0.1"))))
         (/info "server started at tcp:~a" (server-address *server*)))))

(defun stop-server ()
  (cond ((null *server*)
         (error "server not running"))
        (t
         (stop *server*)
         (setf *server* nil)
         (/info "server halted"))))

(defun restart-server ()
  (when *server*
    (stop-server))
  (start-server 10001)
  (/info "restart complete")
  (values))

(defun main (&key port)
  "Run a Coalton LSP server on PORT."
  (start-server port)
  (handler-case
      (loop (sleep 1))
    (sb-sys:interactive-interrupt ()
      (/info "server halted")
      (terpri)
      (cl-user::quit))))
