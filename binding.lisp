(require :screamer)
(in-package :screamer-user)

;; binding-bin and binding-hex: bind a vector to certain bit values.
;;
;; Each bit can have three values: T, NIL and 'don't care'. Bits
;; marked as 'don't care' do not receive any binding.
;;
;; When specifying the binding value as hex or bin, separator
;; characters ' ', ':', '-' or '_' may be arbitrarily added to make
;; the string easier to comprehend. These separators are skipped and
;; ignored.
;;
;; In a binary binding, 'x' means 'don't care'.  In a hex binding, 'x'
;; means don't care for all the corresponding four bits.

(defun filter-bindstr (bindstr)
  (remove #\Space (remove #\_ (remove #\- (remove #\: bindstr)))))

(defun bindstr-hex->bin (hex)
  (with-output-to-string (s)
     (dolist (c (coerce (string-downcase hex) 'list))
       (princ (nibble-hexchar->binstr c) s))))

(defun binding-body (a bindstr)
  (unroll-groups (list a)
    `(cond ((eq #\0 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) nil)))
           ((eq #\1 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) t)))
           (t nil))))

(defmacro binding-bin (a bindstr)
  (let ((bindctl (filter-bindstr bindstr)))
    `(progn ,@(binding-body a bindctl))))

(defmacro binding-hex (a bindstr)
  (let ((bindctl (bindstr-hex->bin (filter-bindstr bindstr))))
    `(progn ,@(binding-body a bindctl))))

;; test macroexpansions

(deftest-fun-args test-binding-body
  binding-body ('(:name x :width 4 :start 3 :inc -1) "1010" )
  '((assert! (equalv x-3 t))
    (assert! (equalv x-2 nil))
    (assert! (equalv x-1 t))
    (assert! (equalv x-0 nil))))

(deftest-fun-args test-binding-bin-t1
  macroexpand-1 ('(binding-bin (:name a :width 8 :start 7 :inc -1) "01:01-x1:x1"))
  '(progn
    (assert! (equalv a-7 nil))
    (assert! (equalv a-6 t))
    (assert! (equalv a-5 nil))
    (assert! (equalv a-4 t))
    (assert! (equalv a-2 t))
    (assert! (equalv a-0 t))))

(deftest-fun-args test-binding-bin-t2
  macroexpand-1 ('(binding-bin (:name a :width 8 :start 7 :inc -1) "x1:x0 - x1_x1"))
  '(progn
    (assert! (equalv a-6 t))
    (assert! (equalv a-4 nil))
    (assert! (equalv a-2 t))
    (assert! (equalv a-0 t))))

(deftest-fun-args test-binding-hex-t1
  macroexpand-1 ('(binding-hex (:name c :width 8 :start 7 :inc -1) "a9"))
  '(progn
    (assert! (equalv c-7 t))
    (assert! (equalv c-6 nil))
    (assert! (equalv c-5 t))
    (assert! (equalv c-4 nil))
    (assert! (equalv c-3 t))
    (assert! (equalv c-2 nil))
    (assert! (equalv c-1 nil))
    (assert! (equalv c-0 t))))

(deftest-fun-args test-binding-hex-t2
  macroexpand-1 ('(binding-hex (:name z :width 8 :start 7 :inc -1) "A:D"))
  '(progn
    (assert! (equalv z-7 t))
    (assert! (equalv z-6 nil))
    (assert! (equalv z-5 t))
    (assert! (equalv z-4 nil))
    (assert! (equalv z-3 t))
    (assert! (equalv z-2 t))
    (assert! (equalv z-1 nil))
    (assert! (equalv z-0 t))))


(deftest test-binding ()
  (combine-results
   (test-binding-body)
   (test-binding-bin-t1)
   (test-binding-bin-t2)
   (test-binding-hex-t1)
   (test-binding-hex-t2)))
