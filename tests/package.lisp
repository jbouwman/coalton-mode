(fiasco:define-test-package #:coalton-mode/tests
  (:use
   #:cl)
  (:local-nicknames
   (#:cm #:coalton-mode)))

(in-package #:coalton-mode/tests)

(defun is-string= (a b &optional (message "input"))
  "If strings A and B differ, signal a failure reporting the first position where this is true."
  (let ((compare-len (min (length a)
                          (length b))))
    (loop :for i :from 0 :below compare-len
          :unless (char= (aref a i)
                         (aref b i))
            :do (is (string= a b)
                    (format nil "Strings differ at offset ~A of ~A:~%A: ~A~%B: ~A"
                            i message a b))
                (return-from is-string=))
    (is (= (length a)
           (length b))
        (format nil "Strings differ at offset ~A of ~A:~%~A~%~A"
                compare-len message a b))))
