(in-package #:coalton-mode)

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
