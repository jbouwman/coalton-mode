;;;; Per-socket connection LSP session

(in-package :coalton-mode)

(defclass session (process)
  ((server :initarg :server)
   (socket :initarg :socket)
   (state :accessor session-state
          :initform 'uninitialized)
   (root-uri :initform nil
             :accessor root-uri))
  (:documentation "Per-connection session data & runloop."))

(defun session-stream (session)
  (usocket:socket-stream (slot-value session 'socket)))

(define-condition session-exit ()
  ())

(defun read-message (session)
  (handler-case
      (with-lock-held (session)
        (read-rpc (session-stream session)))
    (sb-int:closed-stream-error ()
      (/info "remote session disconnected (stream closed)")
      (signal 'session-exit))
    (end-of-file ()
      (/info "remote session disconnected (end of file)")
      (signal 'session-exit))
    (error (c)
      (/info "incomplete read: session shutdown: ~a" c)
      (signal 'session-exit))))

(defun make-request (rpc-message)
  (make-message (message-type rpc-message)
                (parsed-content rpc-message)))

(defun make-response (request)
  (let ((response (make-message 'response-message)))
    (set-field response :jsonrpc "2.0")
    (set-field response :id (get-field request :id))))

(defun make-notification (method params)
  (let ((notification (make-message 'notification-message)))
    (set-field notification :jsonrpc "2.0")
    (set-field notification :method method)
    (set-field notification :params params)
    notification))

(defun response-error (request error-code args)
  (apply #'/error args)
  (unless (get-field request :id)
    ;; No id: it was a notification: don't reply
    (return-from response-error nil))
  (let ((response (make-response request)))
    (set-field response (list :error :code) error-code)
    (set-field response (list :error :message) (apply #'format nil args))
    response))

(defun invalid-request (request &rest args)
  (response-error request :invalid-request args))

(defun method-not-found (request &rest args)
  (response-error request :method-not-found args))

(defun request-method (request)
  (let ((rpc-version (get-field request :jsonrpc))
        (method (get-field request :method)))
    (cond ((not (string-equal rpc-version "2.0"))
           (invalid-request request "Bad rpc version ~a" rpc-version))
          ((not method)
           (invalid-request request "Missing method")))
    method))

(defvar *request-methods*
  (make-hash-table :test 'equal))

(defmacro define-handler ((method params-message-class) &body body)
  `(setf (gethash ,method *request-methods*)
         (cons ',params-message-class
               (lambda ,@body))))

(defun request-params (request)
  "Return REQUEST's params message."
  (let* ((method (request-method request))
         (params-message-class (car (gethash method *request-methods*))))
    (when params-message-class
      (make-message params-message-class (get-field request :params)))))

(defun process-request (session request)
  (let* ((method (request-method request))
         (message-handler (cdr (gethash method *request-methods*))))
    (cond ((not message-handler)
           (method-not-found request "Unsupported method '~a'" method))
          (t
           (funcall message-handler session request)))))

(defun write-message (session message)
  (with-lock-held (session)
    (write-rpc (message-value message)
               (session-stream session))))

(defun process-one-message (session)
  (let* ((request (make-request (read-message session)))
         (response (process-request session request)))
    (when response
      (write-message session response))))

(defmethod run ((session session))
  (handler-case
      (loop :do (process-one-message session))
    (session-exit ()
      (stop session))))

(defmethod stop ((session session))
  (/info "stopping session ~a" session)
  (with-slots (server socket) session
    (usocket:socket-close socket)
    (delete-session server session))
  session)
