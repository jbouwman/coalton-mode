(in-package #:coalton-mode)

(defun import-json-object (object)
  (typecase object
    (hash-table
     (loop :for key :being :the :hash-keys :of object
           :for value :being :the :hash-values :of object
           :collect (cons key (import-json-object value))))
    (t object)))

(defun decode-json (json)
  (import-json-object (com.inuoe.jzon:parse json)))

(defun export-json-object (object)
  (typecase object
    (list
     (let ((h (make-hash-table :test 'equal)))
       (loop :for (key . value) :in object
             :do (setf (gethash key h)
                       (export-json-object value)))
       h))
    (t object)))

(defun encode-json (object)
  (with-output-to-string (stream)
    (com.inuoe.jzon:with-writer (writer :stream stream)
      (com.inuoe.jzon:write-value writer (export-json-object object)))))
