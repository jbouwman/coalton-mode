(in-package #:coalton-mode)

(defvar *message-classes*
  (make-hash-table))

(defclass message-type ()
  ((name :initarg :name
         :reader name)))

(defclass message-atom (message-type)
  ())

(defmethod accept-p ((self message-atom) value)
  (unless (typep value (slot-value self 'name))
    (error "Atom ~a does not accept value ~a" self value))
  value)

(defclass message-field (message-type)
  ((type :initarg :type)
   (optional :initarg :optional)
   (vector :initarg :vector)))

(defmethod print-object ((self message-field) stream)
  (with-slots (name type) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~a)" name type))))

(defun parse-field-type (type)
  (etypecase type
    (list
     (destructuring-bind (type &key optional vector) type
       (list type optional vector)))
    (symbol
     (list type nil nil))))

(defun make-field (spec)
  (destructuring-bind (name type) spec
    (destructuring-bind (type optional vector) (parse-field-type type)
      (get-message-class type)
      (assert (stringp name))
      (make-instance 'message-field
        :name name
        :type type
        :optional optional
        :vector vector))))

(defclass message-class (message-type)
  ((fields :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod print-object ((self message-class) stream)
  (with-slots (name fields) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~d fields)" name (length fields)))))

(defun add-field (class field)
  (vector-push-extend field (slot-value class 'fields)))

(defun %get-field (class name)
  (loop :for field :across (slot-value class 'fields)
        :when (eq (slot-value field 'name) name)
          :do (return-from %get-field field))
  (error "Field not found: ~a" name))

(defmacro define-atom (name)
  `(setf (gethash ',name *message-classes*)
         (make-instance 'message-atom :name ',name)))

(defmacro define-message (name parent-classes &body field-defs)
  `(let ((message (make-instance 'message-class :name ',name)))
     (loop :for class :in ',parent-classes
           :do (loop :for field :across (slot-value (get-message-class class) 'fields)
                     :do (add-field message field)))
     (loop :for field-def :in ',field-defs
           :do (add-field message (make-field field-def)))
     (setf (gethash ',name *message-classes*) message)))

(defun get-message-class (name)
  (or (gethash name *message-classes*)
      (error "Message class ~a is unknown" name)))

;;; Enums

(defclass message-enum (message-type)
  ((values :initform (make-hash-table))))

(defmethod print-object ((self message-enum) stream)
  (with-slots (name values) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a" name))))

(defmethod accept-p ((self message-enum) value)
  (with-slots (values) self
    (or (gethash value values)
        (error "Enum ~a does not accept value ~a" self value))))

(defmacro define-enum (name class-args &body value-defs)
  (declare (ignore class-args))
  `(let ((message (make-instance 'message-enum :name ',name)))
     (loop :for (k v) :in ',value-defs
           :do (setf (gethash k (slot-value message 'values)) v))
     (setf (gethash ',name *message-classes*) message)))

;;; Unions

(defclass message-union (message-type)
  ((types :initarg :types)))

(defmethod print-object ((self message-union) stream)
  (with-slots (name) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a" name))))

(defmethod accept-p ((self message-union) value)
  (error "Fixme, accept-p must not signal a condition"))

(defmacro define-union (name union-classes)
  `(setf (gethash ',name *message-classes*)
         (make-instance 'message-union
                        :name ',name
                        :types (mapcar #'get-message-class
                                       ',union-classes))))

;;; Messages compose a class and a value

(defclass message ()
  ((class :initarg :class)
   (value :initarg :value
          :initform nil
          :reader message-value)))

(defmethod print-object ((self message) stream)
  (with-slots (class value) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a ~a" (name class) value))))

(defun make-message (name &optional value)
  (make-instance 'message
                 :class (get-message-class name)
                 :value value))

;; (declaim (optimize (debug 3)))

(defun get-field-info (class path)
  (let* ((path (listify path))
         (field (%get-field class (car path))))
    (when field
      (let* ((type (slot-value field 'type))
             (field-class (get-message-class type)))
        (if (= 1 (length path))
            (values field-class field) 
            (get-field-info field-class (cdr path)))))))

;; Message API

(defun get-field (message path)
  (with-slots (class value) message
    (get-field-info class path)         ; prevent requests for nonexistent fields
    (loop :with value := value
          :for element :in (listify path)
          :do (setf value (cdr (assoc element value)))
          :finally (return value))))

(defun set-field (message path field-value)
  (with-slots (class value) message
    (multiple-value-bind (field-class parent-class)
        (get-field-info class path)
      (cond ((slot-value parent-class 'vector)
             (setf value
                   (set-value value path (mapcar (lambda (value)
                                                   (accept-p field-class value))
                                                 field-value))))
            (t
             (setf value
                   (set-value value path (accept-p field-class field-value))))))
    message))
