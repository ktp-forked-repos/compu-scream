(require :screamer)
(in-package :screamer-user)

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro strcat (&rest args)
  `(concatenate 'string ,@args))

(deftest-fun-args test-strcat strcat ("abc" "def" "ghi") "abcdefghi")

(deftest test-macros ()
  (combine-results
   (test-strcat)))
