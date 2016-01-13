(require :screamer)
(in-package :screamer-user)

;; mk-binding: bind a vector to certain bit values.
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

(defun mk-binding-body (a bindstr)
  (unroll-groups (list a)
    `(cond ((eq #\0 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) nil)))
           ((eq #\1 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) t)))
           (t nil))))

(defmacro mk-binding-bin (name a bindstr)
  (let ((bindctl (filter-bindstr bindstr)))
    `(defun ,name ,(group-syms a)
       ,@(mk-binding-body a bindctl))))

(defmacro mk-binding-hex (name a bindstr)
  (let ((bindctl (bindstr-hex->bin (filter-bindstr bindstr))))
    `(defun ,name ,(group-syms a)
       ,@(mk-binding-body a bindctl))))

;; example
(mac (mk-binding-bin my-bind-fun
                     (:name a :width 8 :start 7 :inc -1)
                     "01:01 - x1_x1"))

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
