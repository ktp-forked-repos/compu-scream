(require :screamer)
(in-package :screamer-user)

;; mk-binding: bind a vector to certain bit values.
;;
;; Each bit can have three values: T, NIL and 'don't care'. Bits
;; marked as 'don't care' do not receive any binding.
;;
;; When specifying the binding value as hex or bin, separator
;; characters ':' or '-' may be arbitrarily added to make the string
;; easier to comprehend. These separators are skipped and ignored.
;;
;; In a binary binding, 'x' means 'don't care'.  In a hex binding, 'x'
;; means don't care for all the corresponding four bits.

(defun format-bindstr (bindstr)
  (remove #\- (remove #\: bindstr)))

(defun bindstr-hex-to-bin (hex)
  (with-output-to-string (s)
     (dolist (c (coerce (string-downcase hex) 'list))
       (cond ((eq c #\0) (princ "0000" s))
             ((eq c #\1) (princ "0001" s))
             ((eq c #\2) (princ "0010" s))
             ((eq c #\3) (princ "0011" s))
             ((eq c #\4) (princ "0100" s))
             ((eq c #\5) (princ "0101" s))
             ((eq c #\6) (princ "0110" s))
             ((eq c #\7) (princ "0111" s))
             ((eq c #\8) (princ "1000" s))
             ((eq c #\9) (princ "1001" s))
             ((eq c #\a) (princ "1010" s))
             ((eq c #\b) (princ "1011" s))
             ((eq c #\c) (princ "1100" s))
             ((eq c #\d) (princ "1101" s))
             ((eq c #\e) (princ "1110" s))
             ((eq c #\f) (princ "1111" s))
             (t          (princ "xxxx" s))))))

(defun mk-binding-body (a bindstr)
  (unroll-groups (list a)
    `(cond ((eq #\0 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) nil)))
           ((eq #\1 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) t)))
           (t nil))))

(defmacro mk-binding-bin (name a bindstr)
  (let ((bindctl (format-bindstr bindstr)))
    `(defun ,name ,(group-syms a)
       ,@(mk-binding-body a bindctl))))

(defmacro mk-binding-hex (name a bindstr)
  (let ((bindctl (bindstr-hex-to-bin (format-bindstr bindstr))))
    `(defun ,name ,(group-syms a)
       ,@(mk-binding-body a bindctl))))

;; example
(mac (mk-binding-bin my-bind-fun
                     (:name a :width 8 :start 7 :inc -1)
                     "01:01-x1:x1"))

(mac (mk-binding-hex my-bind-fun
                     (:name a :width 8 :start 7 :inc -1)
                     "A:D"))

;; should expand into:
(defun my-bind-fun (a-7 a-6 a-5 a-4 a-3 a-2 a-1 a-0)
  (assert! (equalv a-7 nil))
  (assert! (equalv a-6 t))
  (assert! (equalv a-5 nil))
  (assert! (equalv a-4 t))
  (assert! (equalv a-2 t))
  (assert! (equalv a-0 t)))
