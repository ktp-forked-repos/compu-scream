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

(defmacro deftest (name fun arity)
  `(defun ,name ()
     (all-values
      (solution
       (mk-test-body ,fun ,arity)
       (reorder #'domain-size
                #'(lambda (x) (declare (ignore x)) nil)
                #'<
                #'linear-force)))))

(deftest not-test notv 1)
(deftest and-test andv 2)
(deftest nand-test nandv 2)
(deftest nand-test3 nandv 3)
(deftest or-test orv 2)
(deftest nor-test norv 2)
(deftest nor-test3 norv 3)
(deftest xor2-test xor2v 2)
(deftest xor3-test xor3v 3)
(deftest xor4-test xor4v 4)
(deftest chv-test chv 3)
(deftest majv-test majv 3)

;; TODO add automated test running the above functions and
;; verifying that the results match

;;;;;
;;
;; Now we need to build infrastructure to specify constraints on
;; vectors of boolean variables
;;  - allow them to be constrained as hex string "dead:beef:1234"
;;  - allow their results to be printed as hex string
;;  - need support for ROTR, SHR, + (integer addition) and friends
;;
;;;;;
