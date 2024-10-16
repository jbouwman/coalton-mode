;;;; Read and write JSON-RPC messages
;;;;
;;;; https://www.jsonrpc.org/specification

(in-package :coalton-mode)

(defun read-header (stream)
  "Read a HTTP-header-formatted key value pair from STREAM."
  (declare (optimize (speed 3)))
  (flet ((make-buf ()
           (make-array 0 :adjustable t :fill-pointer t :element-type 'character)))
    (let ((state :begin-line)
          (prev-state nil)
          (buf (make-buf))
          (k nil))
      (loop :for c := (read-char stream)
            :do (ecase state
                  (:begin-line
                   (cond ((char= c #\Return)
                          (setf state :cr prev-state :key))
                         (t
                          (vector-push-extend c buf)
                          (setf state :key))))
                  (:key
                   (cond ((char= c #\:)
                          (setf k buf buf (make-buf) state :after-key))
                         (t
                          (vector-push-extend c buf))))
                  (:after-key
                   (cond ((char= c #\Space))
                         (t
                          (vector-push-extend c buf)
                          (setf state :value))))
                  (:value
                   (cond ((char= c #\Return)
                          (setf state :cr prev-state :value))
                         (t
                          (vector-push-extend c buf))))
                  (:cr
                   (cond ((char= c #\Newline)
                          (return))
                         (t
                          (vector-push-extend #\Return buf)
                          (vector-push-extend c buf)
                          (setf state prev-state prev-state nil))))))
      (when (< 0 (length k))
        (cons k buf)))))

(defun read-headers (stream)
  (loop :for kv := (read-header stream) :while kv :collect kv))

(defun write-crlf (stream)
  (write-char #\Return stream)
  (write-char #\Newline stream))

(defun write-header (stream kv)
  "Write a HTTP-header-formatted key value pair to STREAM."
  (write-string (car kv) stream)
  (write-char #\: stream)
  (write-char #\Space stream)
  (write-string (princ-to-string (cdr kv)) stream)
  (write-crlf stream))

(defun write-headers (stream kvs)
  (loop :for kv :in kvs :do (write-header stream kv))
  (write-crlf stream))

(defun get-header (map key)
  (cdr (assoc key map :test #'string-equal)))

(defun content-length (headers)
  (let ((value (get-header headers "Content-Length")))
    (when value
      (parse-integer value))))

(defclass rpc-message ()
  ((headers :initarg :headers)
   (content :initarg :content)))

(defun message-id (rpc-message)
  (cdr (assoc "id" (slot-value rpc-message 'content) :test #'string-equal)))

(defmethod print-object ((self rpc-message) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "id: ~a" (or (message-id self) "NONE"))))

(defun read-rpc (stream)
  (/info "enter read-rpc")
  (let* ((headers (read-headers stream))
         (content-length (content-length headers))
         (content (make-array content-length :element-type 'character)))
    (read-sequence content stream :start 0 :end content-length)
    (make-instance 'rpc-message
      :headers headers
      :content (json:decode-json-from-string content))))

(defun write-rpc (message stream)
  (let ((content (json:encode-json-to-string message)))
    (write-headers stream `(("Content-Length" . ,(length content))
                            ("Content-Type" . "application/json-rpc")))
    (write-sequence content stream)
    (force-output stream)
    (break)))
