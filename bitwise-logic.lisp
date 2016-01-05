(require :screamer)
(in-package :screamer-user)

;; extensions to allow gate-style logic computations

(defmacro nandv (&rest v)
  `(notv (andv ,@v)))

(defmacro norv (&rest v)
  `(notv (orv ,@v)))

(defmacro xor2v (x y)
  `(orv (andv ,x (notv ,y)) (andv (notv ,x) ,y)))

(defmacro xor3v (x y z)
  `(xor2v ,x (xor2v ,y ,z)))

(defmacro xor4v (x y z w)
  `(xor2v (xor2v ,x ,y) (xor2v ,z ,w)))

(defmacro chv (x y z)
  `(xor2v (andv ,x ,y)
          (andv (notv ,x) ,z)))

(defmacro majv (x y z)
  `(xor3v (andv ,x ,y)
          (andv ,x ,z)
          (andv ,y ,z)))

;; test function generators

(defun mk-vars (arity)
  (let ((vars nil))
    (dotimes (i arity)
      (push (gensym) vars))
    vars))

(defmacro mk-test-body (fun arity)
  (let ((vars (mk-vars arity)))
    `(let ,(mapcar #'(lambda (v) (list v '(a-booleanv))) vars)
       (assert! (,fun ,@vars))
       (list ,@vars))))

(defmacro mk-testfun (name fun arity)
  `(defun ,name ()
     (all-values
      (solution
       (mk-test-body ,fun ,arity)
       (reorder #'domain-size
                #'(lambda (x) (declare (ignore x)) nil)
                #'<
                #'linear-force)))))

(mk-testfun test-not-f notv 1)
(mk-testfun test-and-f andv 2)
(mk-testfun test-nand-f nandv 2)
(mk-testfun test-nand3-f nandv 3)
(mk-testfun test-or-f orv 2)
(mk-testfun test-nor-f norv 2)
(mk-testfun test-nor3-f norv 3)
(mk-testfun test-xor2-f xor2v 2)
(mk-testfun test-xor3-f xor3v 3)
(mk-testfun test-xor4-f xor4v 4)
(mk-testfun test-chv-f chv 3)
(mk-testfun test-majv-f majv 3)

(load "test.lisp")

(deftest-fun test-not '((NIL)) )
(deftest-fun test-and '((T T)) )
(deftest-fun test-nand '((T NIL) (NIL T) (NIL NIL)) )
(deftest-fun test-nand3 '((T T NIL) (T NIL T) (T NIL NIL) (NIL T T)
                          (NIL T NIL) (NIL NIL T) (NIL NIL NIL)) )
(deftest-fun test-or '((T T) (T NIL) (NIL T)) )
(deftest-fun test-nor '((NIL NIL)) )
(deftest-fun test-nor3 '((NIL NIL NIL)) )
(deftest-fun test-xor2 '((T NIL) (NIL T)) )
(deftest-fun test-xor3 '((T T T) (T NIL NIL) (NIL T NIL) (NIL NIL T)) )
(deftest-fun test-xor4 '((T T T NIL) (T T NIL T) (T NIL T T) (T NIL NIL NIL)
                         (NIL T T T) (NIL T NIL NIL) (NIL NIL T NIL)
                         (NIL NIL NIL T)) )
(deftest-fun test-chv '((T T T) (T T NIL) (NIL T T) (NIL NIL T)) )
(deftest-fun test-majv '((T T T) (T T NIL) (T NIL T) (NIL T T)) )

(deftest test-bitwise-logic ()
  (combine-results
   (test-not)
   (test-and) (test-nand) (test-nand3)
   (test-or) (test-nor) (test-nor3)
   (test-xor2) (test-xor3) (test-xor4)
   (test-chv) (test-majv)))

(test-bitwise-logic)

;;;;;
;;
;; Now we need to build infrastructure to specify constraints on
;; vectors of boolean variables
;;  - allow them to be constrained as hex string "dead:beef:1234"
;;  - allow their results to be printed as hex string
;;  - need support for ROTR, SHR, + (integer addition) and friends
;;
;;;;;
