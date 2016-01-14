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


(defun slice-seq (seq length slice-length slice-list)
  (if (> length slice-length)
      (let* ((rem-length (- length slice-length))
             (rem-seq (subseq seq 0 rem-length))
             (slice (subseq seq rem-length length)))
        (slice-seq rem-seq rem-length slice-length (cons slice slice-list)))
      (cons seq slice-list)))

(deftest-fun-args test-slice-seq-t1
  slice-seq ('(a b c d e f g h) 8 1 nil) '((A) (B) (C) (D) (E) (F) (G) (H)))
(deftest-fun-args test-slice-seq-t2
  slice-seq ('(a b c d e f g h) 8 2 nil) '((A B) (C D) (E F) (G H)))
(deftest-fun-args test-slice-seq-t3
  slice-seq ('(a b c d e f g h) 8 3 nil) '((A B) (C D E) (F G H)))
(deftest-fun-args test-slice-seq-t4
  slice-seq ('(a b c d e f g h) 8 4 nil) '((A B C D) (E F G H)))

(defun slice-string (string slice-length sep-char)
  (if (> slice-length 0)
      (let ((slice-list (slice-seq string (length string) slice-length nil))
            (format-str (format nil "~~{~~A~~^~c~~}" sep-char)))
        (format nil format-str slice-list))
      string))

(deftest-fun-args test-slice-string-t1 slice-string ("123456" 0 #\_) "123456")
(deftest-fun-args test-slice-string-t2 slice-string ("10" 4 #\_) "10")
(deftest-fun-args test-slice-string-t3 slice-string ("1100" 4 #\_) "1100")
(deftest-fun-args test-slice-string-t4 slice-string ("1011001100" 4 #\_) "10_1100_1100")
(deftest-fun-args test-slice-string-t5 slice-string ("abcdefghgijklmnop" 8 #\:) "a:bcdefghg:ijklmnop")

(deftest test-slice ()
  (combine-results
   (test-slice-seq-t1)
   (test-slice-seq-t2)
   (test-slice-seq-t3)
   (test-slice-seq-t4)
   (test-slice-string-t1)
   (test-slice-string-t2)
   (test-slice-string-t3)
   (test-slice-string-t4)
   (test-slice-string-t5)))

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

(defun vector->binstr (v &key (slice 4) (sep #\_))
  (let ((str (make-array (length v)
                         :element-type 'character
                         :initial-contents (mapcar #'(lambda (b) (if b #\1 #\0)) v))))
    (slice-string str slice sep)))

(defun vector->hexstr (v  &key (slice 8) (sep #\:))
  (let* ((slice-list (slice-seq v (length v) 4 nil)) ; cut vector into nibbles
         (str (make-array (length slice-list)
                          :element-type 'character
                          :initial-contents (mapcar #'nibble-vector->hexchar
                                                    slice-list))))
    (slice-string str slice sep)))

(defun value->binstr (value bits &key (slice 4) (sep #\_))
  (slice-string (format nil "~v,'0b" bits value) slice sep))
(defun value->hexstr (value bits &key (slice 8) (sep #\:))
  (slice-string (format nil "~v,'0x" (/ bits 4) value) slice sep))

(deftest-fun-args test-vecval-t1 vector->value ('()) 0)
(deftest-fun-args test-vecval-t2 vector->value ('(nil)) 0)
(deftest-fun-args test-vecval-t3 vector->value ('(nil nil)) 0)
(deftest-fun-args test-vecval-t4 vector->value ('(t)) 1)
(deftest-fun-args test-vecval-t5 vector->value ('(nil t)) 1)
(deftest-fun-args test-vecval-t6 vector->value ('(t nil)) 2)
(deftest-fun-args test-vecval-t7 vector->value ('(t t)) 3)

(deftest-fun-args test-vecbin-t1 vector->binstr ('(nil t t nil)) "0110")
(deftest-fun-args test-vecbin-t2 vector->binstr ('(t nil t t nil)) "1_0110")
(deftest-fun-args test-vecbin-t3 vector->binstr ('(nil nil)) "00")
(deftest-fun-args test-vecbin-t4 vector->binstr ('(nil nil t t t t)) "00_1111")
(deftest-fun-args test-vecbin-t5
  vector->binstr ('(t nil t nil t nil t nil t t t t nil nil nil nil t t t t t nil))
  "10_1010_1011_1100_0011_1110")
(deftest-fun-args test-vecbin-t6
  vector->binstr ('(t nil t nil t nil t nil t t t t nil nil nil nil t t t t t nil)
                  :slice 0)
  "1010101011110000111110")

(deftest-fun-args test-vechex-t1 vector->hexstr ('(t nil t nil)) "A")
(deftest-fun-args test-vechex-t2 vector->hexstr ('(t t nil t)) "D")
(deftest-fun-args test-vechex-t3 vector->hexstr ('(t t t t t t)) "3F")
(deftest-fun-args test-vechex-t4 vector->hexstr ('(t t nil nil t)) "19")
(deftest-fun-args test-vechex-t5
  vector->hexstr ('(t nil t nil t nil t nil t t t t nil nil nil nil t t t t t t
                      t t t t nil t nil t t t t nil nil nil t t t t t nil))
  "2AB:C3FF5E3E")
(deftest-fun-args test-vechex-t6
  vector->hexstr ('(t nil t nil t nil t nil t t t t nil nil nil nil t t t t t t
                      t t t t nil t nil t t t t nil nil nil t t t t t nil)
                  :sep #\-)
  "2AB-C3FF5E3E")
(deftest-fun-args test-vechex-t7
  vector->hexstr ('(nil nil nil nil nil nil nil nil nil nil nil nil t nil nil t
                        t t t t nil t t nil)
                  :slice 2)
  "00:09:F6")

(deftest-fun-args test-valbin-t1 value->binstr (24 8) "0001_1000")
(deftest-fun-args test-valbin-t2 value->binstr (124 16) "0000_0000_0111_1100")
(deftest-fun-args test-valbin-t3
  value->binstr (191634543 32) "0000_1011_0110_1100_0001_1100_0110_1111")
(deftest-fun-args test-valbin-t4
  value->binstr (191634543 32 :slice 8 :sep #\:) "00001011:01101100:00011100:01101111")

(deftest-fun-args test-valhex-t1 value->hexstr (1696 16) "06A0")
(deftest-fun-args test-valhex-t2 value->hexstr (4239987432 32) "FCB912E8")
(deftest-fun-args test-valhex-t3
  value->hexstr ((* 12345 12345 12345 12345) 64) "0052836F:752FD261")
(deftest-fun-args test-valhex-t4
  value->hexstr ((* 12345 12345 12345 12345) 64 :slice 0) "0052836F752FD261")

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
   (test-vecbin-t4)
   (test-vecbin-t5)
   (test-vecbin-t6)))

(deftest test-vechex ()
  (combine-results
   (test-vechex-t1)
   (test-vechex-t2)
   (test-vechex-t3)
   (test-vechex-t4)
   (test-vechex-t5)
   (test-vechex-t6)
   (test-vechex-t7)))

(deftest test-valstr ()
  (combine-results
   (test-valbin-t1)
   (test-valbin-t2)
   (test-valbin-t3)
   (test-valbin-t4)
   (test-valhex-t1)
   (test-valhex-t2)
   (test-valhex-t3)
   (test-valhex-t4)))


(deftest test-utils ()
  (combine-results
   (test-slice)
   (test-vecval)
   (test-vecbin)
   (test-vechex)
   (test-valstr)))
