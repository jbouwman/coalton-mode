(in-package #:coalton-mode)

(defvar *message-classes*
  (make-hash-table))

(defclass primitive-type ()
  ((name :initarg :name)))

(defmethod accept-p ((self primitive-type) value)
  (unless (typep value (slot-value self 'name))
    (error "Primitive ~a does not accept value ~a" self value))
  value)

(defclass message-class ()
  ((name :initarg :name :reader name)
   (fields :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod print-object ((self message-class) stream)
  (with-slots (name fields) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~d fields)" name (length fields)))))

(defclass message-field ()
  ((name :initarg :name)
   (type :initarg :type)
   (optional :initarg :optional)))

(defmethod print-object ((self message-field) stream)
  (with-slots (name type) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a (~a)" name type))))

(defun parse-field-type (type)
  (etypecase type
    (list
     (destructuring-bind (type &key optional) type
       (list type optional)))
    (symbol
     (list type nil))))

(defun make-field (spec)
  (destructuring-bind (name type) spec
    (destructuring-bind (type optional) (parse-field-type type)
      (get-message-class type)
      (make-instance 'message-field
        :name name
        :type type
        :optional optional))))

(defun add-field (class field)
  (vector-push-extend field (slot-value class 'fields)))

(defun %get-field (class name)
  (loop :for field :across (slot-value class 'fields)
        :when (eq (slot-value field 'name) name)
          :do (return-from %get-field field))
  (error "Field not found: ~a" name))

(defmacro define-primitive (name)
  `(setf (gethash ',name *message-classes*)
         (make-instance 'primitive-type :name ',name)))

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

(defclass message-enum ()
  ((name :initarg :name)
   (values :initform (make-hash-table))))

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

(defclass message-union ()
  ((name :initarg :name)
   (types :initarg :types)))

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
          :initform nil)))

(defmethod print-object ((self message) stream)
  (with-slots (class value) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a ~a" (name class) value))))

(defun new-message (name &optional value)
  (make-instance 'message
                 :class (get-message-class name)
                 :value value))

(defun listify (x)
  (if (listp x) x (list x)))

(defun set-value (alist path value)
  (let ((path (listify path)))
    (if (null path)
        value
        (let ((key (car path)))
          (if (null (cdr path))
              (acons key value (remove key alist :key #'car))
              (let ((sub-alist (assoc key alist)))
                (if sub-alist
                    (acons key
                           (set-value (cdr sub-alist) (cdr path) value)
                           (remove key alist :key #'car))
                    (acons key
                           (set-value nil (cdr path) value)
                           alist))))))))

(defun get-class-field (class path)
  (let ((path (listify path)))
    (if (null (car path))
        class
        (let ((field (%get-field class (car path))))
          (when field
            (let* ((type (slot-value field 'type))
                   (class (get-message-class type)))
              (get-class-field class (cdr path))))))))

;; Message API

(defun get-field (message path)
  (with-slots (class value) message
    (get-class-field class path)
    (loop :with value := value
          :for element :in (listify path)
          :do (setf value (cdr (assoc element value)))
          :finally (return value))))

(defun set-field (message path new-value)
  (with-slots (class value) message
    (let ((field (get-class-field class path)))
      (setf value
            (set-value value path (accept-p field new-value))))
    message))
