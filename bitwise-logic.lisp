(require :screamer)
(in-package :screamer-user)

(load "test.lisp")
(load "macros.lisp")
(load "utils.lisp")
(load "group.lisp")

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

(defmacro full-adder-s (a b ci s)
  `(assert! (equalv ,s (xor3v ,a ,b ,ci))))

(defmacro full-adder (a b ci co s)
  `(progn (full-adder-s ,a ,b ,ci ,s)
          (assert! (equalv ,co (orv (andv ,a ,b)
                                    (andv ,ci
                                          (xor2v ,a ,b)))))))

(defmacro rc-adder (a2 a1 a0 b2 b1 b0 s2 s1 s0)
  `(let ((c2 (a-booleanv))
         (c1 (a-booleanv)))
     (half-adder   ,a0 ,b0 c1    ,s0)
     (full-adder   ,a1 ,b1 c1 c2 ,s1)
     (full-adder-s ,a2 ,b2 c2    ,s2)))

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
(mk-testcirc test-rc-adder-f rc-adder 9)

(deftest-fun test-half-adder '("0000" "0101" "1001" "1110"))
(deftest-fun test-full-adder '("00000" "00101" "01001" "01110"
                               "10001" "10110" "11010" "11111"))
(deftest-fun test-rc-adder
  '("000000000" "000001001" "000010010" "000011011"
    "000100100" "000101101" "000110110" "000111111"
    "001000001" "001001010" "001010011" "001011100"
    "001100101" "001101110" "001110111" "001111000"
    "010000010" "010001011" "010010100" "010011101"
    "010100110" "010101111" "010110000" "010111001"
    "011000011" "011001100" "011010101" "011011110"
    "011100111" "011101000" "011110001" "011111010"
    "100000100" "100001101" "100010110" "100011111"
    "100100000" "100101001" "100110010" "100111011"
    "101000101" "101001110" "101010111" "101011000"
    "101100001" "101101010" "101110011" "101111100"
    "110000110" "110001111" "110010000" "110011001"
    "110100010" "110101011" "110110100" "110111101"
    "111000111" "111001000" "111010001" "111011010"
    "111100011" "111101100" "111110101" "111111110"))

(deftest test-basic-circuits ()
  (combine-results
   (test-half-adder)
   (test-full-adder)
   (test-rc-adder)))

(deftest test ()
  (combine-results
   (test-macros)
   (test-utils)
   (test-group)
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
