(require :screamer)
(in-package :screamer-user)

;; Unit test code taken from Peter Seibel's book Practical Common Lisp:
;; http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html
;; changed liberally to accommodate our needs

(defvar *test-name* nil)
(defvar *test-count* 0)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
    "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
    `(defun ,name ,parameters
       (let ((*test-name* (append *test-name* (list ',name))))
         ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
                `(let ((,result t))
                   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
                   ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (incf *test-count*)
  (format t "~:[FAIL~;pass~] ~3d ~a~%" result *test-count* *test-name*)
  (if (null result)
      (format t "failed test:~%~a~%" form))
  result)

;; convenience macros

(defmacro deftest-fun (name result)
  (let ((fun (intern (concatenate 'string (symbol-name name) "-F"))))
    `(deftest ,name () (check (equal (,fun) ,result)))))

(defmacro deftest-fun-args (name fun args result)
  `(deftest ,name () (check (equal (,fun ,@args) ,result))))
