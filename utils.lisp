(require :screamer)
(in-package :screamer-user)

(defun update-plist (plist indicator new-value)
  (let ((other-properties nil))
    (loop while plist
          for property = (pop plist)
          for value = (pop plist)
          when (eq property indicator)
          do (return-from update-plist (list* property new-value
                                              (append other-properties plist)))
          else do (push value other-properties)
          (push property other-properties))
    (list* indicator new-value other-properties)))


(defun vector->value-int (v val)
  (if (null v)
      val
      (let ((d (car v))
            (r (cdr v)))
        (if d
            (vector->value-int r (1+ (* 2 val)))
            (vector->value-int r (* 2 val))))))

(defun vector->value (v)
  (vector->value-int (mapcar #'value-of v) 0))
(defun vector->binstr (v)
  (make-array (length v)
    :element-type 'character
    :initial-contents (mapcar #'(lambda (b) (if b #\1 #\0)) v)))
(defun vector->hexstr (v) (format nil "~x" (vector->value v)))

(defun value->binstr (value bits) (format nil "~v,'0b" bits value))
(defun value->hexstr (value bits) (format nil "~v,'0x" (/ bits 4) value))

(deftest-fun-args test-vecval-t1 vector->value ('()) 0)
(deftest-fun-args test-vecval-t2 vector->value ('(nil)) 0)
(deftest-fun-args test-vecval-t3 vector->value ('(nil nil)) 0)
(deftest-fun-args test-vecval-t4 vector->value ('(t)) 1)
(deftest-fun-args test-vecval-t5 vector->value ('(nil t)) 1)
(deftest-fun-args test-vecval-t6 vector->value ('(t nil)) 2)
(deftest-fun-args test-vecval-t7 vector->value ('(t t)) 3)

(deftest-fun-args test-vecbin-t1 vector->binstr ('(nil t t nil)) "0110")
(deftest-fun-args test-vecbin-t2 vector->binstr ('(t nil t t nil)) "10110")
(deftest-fun-args test-vecbin-t3 vector->binstr ('(nil nil)) "00")
(deftest-fun-args test-vecbin-t4 vector->binstr ('(nil nil t t t t)) "001111")

(deftest-fun-args test-vechex-t1 vector->hexstr ('(t nil t nil)) "A")
(deftest-fun-args test-vechex-t2 vector->hexstr ('(t t nil t)) "D")
(deftest-fun-args test-vechex-t3 vector->hexstr ('(t t t t t t)) "3F")
(deftest-fun-args test-vechex-t4 vector->hexstr ('(t t nil nil t)) "19")

(deftest-fun-args test-valbin-t1 value->binstr (24 8) "00011000")
(deftest-fun-args test-valbin-t2 value->binstr (124 16) "0000000001111100")
(deftest-fun-args test-valhex-t1 value->hexstr (1696 16) "06A0")
(deftest-fun-args test-valhex-t2 value->hexstr (4239987432 32) "FCB912E8")

(deftest test-vecval ()
  (combine-results
   (test-vecval-t1)
   (test-vecval-t2)
   (test-vecval-t3)
   (test-vecval-t4)
   (test-vecval-t5)
   (test-vecval-t6)
   (test-vecval-t7)))

(deftest test-vecbin ()
  (combine-results
   (test-vecbin-t1)
   (test-vecbin-t2)
   (test-vecbin-t3)
   (test-vecbin-t4)))

(deftest test-vechex ()
  (combine-results
   (test-vechex-t1)
   (test-vechex-t2)
   (test-vechex-t3)
   (test-vechex-t4)))

(deftest test-valstr ()
  (combine-results
   (test-valbin-t1)
   (test-valbin-t2)
   (test-valhex-t1)
   (test-valhex-t2)))

(deftest test-utils ()
  (combine-results
   (test-vecval)
   (test-vecbin)
   (test-vechex)
   (test-valstr)))
