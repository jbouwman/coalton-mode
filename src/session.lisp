;;;; Per-socket connection LSP session

(in-package :coalton-mode)

(defclass session (process)
  ((server :initarg :server)
   (socket :initarg :socket)
   (state :accessor session-state
          :initform 'uninitialized))
  (:documentation "Per-connection session data & runloop."))

(defun session-stream (session)
  (usocket:socket-stream (slot-value session 'socket)))

(defun read-message (session)
  (handler-case
      (with-lock-held (session)
        (read-rpc (session-stream session)))
    (sb-int:closed-stream-error ()
      (/info "remote session disconnected (stream closed)")
      nil)
    (end-of-file ()
      (/info "remote session disconnected (end of file)")
      nil)))

(defun make-response (request)
  (let ((response (new-message 'response-message)))
    (set-field response :jsonrpc "2.0")
    (set-field response :id (get-field request :id))))

(defun response-error (request error-code args)
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
        (id (get-field request :id))
        (method (get-field request :method)))
    (cond ((not (string-equal rpc-version "2.0"))
           (invalid-request "Bad rpc version ~a" rpc-version))
          ((not id)
           (invalid-request "Missing message id"))
          ((not method)
           (invalid-request "Missing method")))
    method))

(defvar *request-methods*
  (make-hash-table :test 'equal))

(defmacro define-handler ((method request-message-class) &body body)
  `(setf (gethash ,method *request-methods*)
         (cons ',request-message-class
               (lambda ,@body))))

(defun get-request (request)
  (let* ((method (request-method request))
         (message-class (car (gethash method *request-methods*))))
    (when message-class
      (new-message message-class (get-field request :params)))))

(defun process-request (session request)
  (let ((method (request-method request)))
    (destructuring-bind (message-class . message-handler)
        (gethash method *request-methods*)
      (cond ((not message-class)
             (method-not-found request "Unsupported method '~a'" method))
            (t
             (funcall message-handler session request))))))

(defun write-message (session message)
  (with-lock-held (session)
    (write-rpc message (session-stream session))))

(defun process-one-message (session)
  (let ((message (read-message session)))
    (unless message
      (stop session)
      (return-from process-one-message))
    (write-message session
                   (process-request session
                                    (new-message 'request-message
                                                 (slot-value message 'content))))))

(defmethod run ((session session))
  (loop :do (process-one-message session)))

(defmethod stop ((session session))
  (/info "stopping session ~a" session)
  (with-slots (server socket) session
    (usocket:socket-close socket)
    (delete-session server session))
  session)
