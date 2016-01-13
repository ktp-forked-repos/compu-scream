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

;; examples

(mac (binding-bin
      (:name a :width 8 :start 7 :inc -1)
      "01:01-x1:x1"))

(mac (binding-hex
      (:name c :width 4 :start 3 :inc -1)
      "a"))

(mac (binding-bin
      (:name a :width 8 :start 7 :inc -1)
      "01:01 - x1_x1"))

(mac (binding-hex
      (:name a :width 8 :start 7 :inc -1)
      "A:D"))
