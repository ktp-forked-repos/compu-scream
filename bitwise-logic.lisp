(require :screamer)
(in-package :screamer-user)

(load "test.lisp")
(load "utils.lisp")

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

(defmacro mk-testfun-body (fun arity)
  (let ((vars (mk-vars arity)))
    `(let ,(mapcar #'(lambda (v) (list v '(a-booleanv))) vars)
       (assert! (,fun ,@vars))
       (list ,@vars))))

(defmacro mk-testfun (name fun arity)
  `(defun ,name ()
     (sort (mapcar #'vector->binstr
                   (all-values
                    (solution
                     (mk-testfun-body ,fun ,arity)
                     (reorder #'domain-size
                              #'(lambda (x) (declare (ignore x)) nil)
                              #'<
                              #'linear-force))))
           #'string<)))

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

(deftest-fun test-not '("0"))
(deftest-fun test-and '("11"))
(deftest-fun test-nand '("00" "01" "10"))
(deftest-fun test-nand3 '("000" "001" "010" "011" "100" "101" "110"))
(deftest-fun test-or '("01" "10" "11"))
(deftest-fun test-nor '("00"))
(deftest-fun test-nor3 '("000"))
(deftest-fun test-xor2 '("01" "10") )
(deftest-fun test-xor3 '("001" "010" "100" "111"))
(deftest-fun test-xor4 '("0001" "0010" "0100" "0111"
                         "1000" "1011" "1101" "1110"))
(deftest-fun test-chv '("001" "011" "110" "111"))
(deftest-fun test-majv '("011" "101" "110" "111"))

(deftest test-logic-gates ()
  (combine-results
   (test-not)
   (test-and) (test-nand) (test-nand3)
   (test-or) (test-nor) (test-nor3)
   (test-xor2) (test-xor3) (test-xor4)
   (test-chv) (test-majv)))

;; some basic circuits for digital computations

(defmacro half-adder (a b c s)
  `(progn (assert! (equalv ,s (xor2v ,a ,b)))
          (assert! (equalv ,c (andv ,a ,b)))))

(defmacro full-adder (a b ci co s)
  `(progn (assert! (equalv ,s (xor3v ,a ,b ,ci)))
          (assert! (equalv ,co (orv (andv ,a ,b)
                                    (andv ,ci
                                          (xor2v ,a ,b)))))))

;; test function generators

(defmacro mk-testcirc-body (fun arity)
  (let ((vars (mk-vars arity)))
    `(let ,(mapcar #'(lambda (v) (list v '(a-booleanv))) vars)
       (,fun ,@vars)
       (list ,@vars))))

(defmacro mk-testcirc (name fun arity)
  `(defun ,name ()
     (sort (mapcar #'vector->binstr
                   (all-values
                    (solution
                     (mk-testcirc-body ,fun ,arity)
                     (reorder #'domain-size
                              #'(lambda (x) (declare (ignore x)) nil)
                              #'<
                              #'linear-force))))
           #'string<)))

(mk-testcirc test-half-adder-f half-adder 4)
(mk-testcirc test-full-adder-f full-adder 5)

(deftest-fun test-half-adder '("0000" "0101" "1001" "1110"))
(deftest-fun test-full-adder '("00000" "00101" "01001" "01110"
                               "10001" "10110" "11010" "11111"))

(deftest test-basic-circuits ()
  (combine-results
   (test-half-adder)
   (test-full-adder)))

(deftest test ()
  (combine-results
   (test-utils)
   (test-logic-gates)
   (test-basic-circuits)))

(test)

;;;;;
;;
;; Now we need to build infrastructure to specify constraints on
;; vectors of boolean variables
;;  - allow them to be constrained as hex string "dead:beef:1234"
;;  - allow their results to be printed as hex string
;;  - need support for ROTR, SHR, + (integer addition) and friends
;;
;;;;;

(defun test-half-adder-generator ()
  (all-values
   (let ((a (a-boolean))
         (b (a-boolean))
         (c (a-boolean))
         (s (a-boolean)))
     (if (and (equalv s (xor2v a b))
              (equalv c (andv a b)))
         (vector->binstr (list a b s c))
       (fail)))))

(defun test-half-adder-constraint ()
  (mapcar #'vector->binstr
          (all-values
           (solution
            (let ((a (a-booleanv))
                  (b (a-booleanv))
                  (s (a-booleanv))
                  (c (a-booleanv)))
              (half-adder a b s c)
              (list a b s c))
            (reorder #'domain-size
                     #'(lambda (x) (declare (ignore x)) nil)
                     #'<
                     #'linear-force)))))
