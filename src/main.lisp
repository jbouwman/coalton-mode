;;;; Start, restart and stop the LSP server

(in-package :coalton-mode)

(defvar *server* nil
  "The server process.")

(defun start-server (port)
  (cond (*server*
         (error "Server already running: ~a" *server*))
        ((null port)
         (error "Server TCP port must be specified"))
        (t
         (setf *server*
               (start (make-instance 'server
                        :config (list :port port
                                      :host "127.0.0.1")))))))

(defun stop-server ()
  (cond ((null *server*)
         (error "Server not running"))
        (t
         (stop *server*)
         (setf *server* nil))))

(defun restart-server ()
  (when *server*
    (stop-server))
  (start-server 10001))

;; (restart-server)

(defun main (&key port)
  "Run a Coalton server on PORT.  PORT must be specified."
  (start-server port)
  (/info "server started: ~a~%Shutdown with C-c" *server*)
  (handler-case
      (loop (sleep 1))
    (sb-sys:interactive-interrupt ()
      (/info "server halted")
      (terpri)
      (cl-user::quit))))
