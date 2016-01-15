(require :screamer)
(in-package :screamer-user)

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro strcat (&rest args)
  `(concatenate 'string ,@args))

(deftest-fun-args test-strcat strcat ("abc" "def" "ghi") "abcdefghi")

;; Define a function where all args are first converted from hex
;; strings to their numeric value, then the body is evaluated as
;; usual, and its return value converted from numeric to hex string.
;; Useful for creating test functions.
(defmacro defun-hexify (name args &body body)
  `(defun ,name ,args
     (let ,(mapcar #'(lambda (x) `(,x (hexstr->value ,x))) args)
       (value->hexstr ,@body 32))))

(deftest test-macros ()
  (combine-results
   (test-strcat)))
