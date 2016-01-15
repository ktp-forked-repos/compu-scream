(require :screamer)
(in-package :screamer-user)

(load "test.lisp")
(load "macros.lisp")
(load "hexutils.lisp")
(load "utils.lisp")
(load "arith.lisp")
(load "group.lisp")
(load "solver.lisp")
(load "binding.lisp")
(load "test-logic.lisp")
(load "basic-logic.lisp")
(load "sha-logic.lisp")

;; Examples where we
;; - declare multiple groups,
;; - constrain them to certain logical relations,
;; - bind some of them fully or partially,
;; - evaluate the results.

(def-solver ex1 all-values vector->binstr
  ((:name a :width 4 :start 3 :inc -1)
   (:name b :width 4 :start 3 :inc -1)
   (:name s :width 4 :start 3 :inc -1))

  ;; TODO from this point, write a instead of (:name a ...)
  (rc-adder (:name a :width 4 :start 3 :inc -1)
            (:name b :width 4 :start 3 :inc -1)
            (:name s :width 4 :start 3 :inc -1))
  (binding-bin (:name a :width 4 :start 3 :inc -1) "x1:01")
  (binding-hex (:name b :width 4 :start 3 :inc -1) "A")
  (binding-bin (:name s :width 4 :start 3 :inc -1) "xx:xx"))

(ex1)


(def-solver ex2 all-values vector->hexstr
  ((:name a :width 32 :start 31 :inc -1)
   (:name b :width 32 :start 31 :inc -1)
   (:name c :width 32 :start 31 :inc -1)
   (:name x :width 32 :start 31 :inc -1)
   (:name y :width 32 :start 31 :inc -1)
   (:name z :width 32 :start 31 :inc -1))
  (rc-adder (:name a :width 32 :start 31 :inc -1)
            (:name b :width 32 :start 31 :inc -1)
            (:name c :width 32 :start 31 :inc -1))
  (rc-adder (:name x :width 32 :start 31 :inc -1)
            (:name y :width 32 :start 31 :inc -1)
            (:name z :width 32 :start 31 :inc -1))
  (binding-hex (:name a :width 32 :start 31 :inc -1) "0000000x")
  (binding-hex (:name b :width 32 :start 31 :inc -1) "12345678")
  (binding-hex (:name x :width 32 :start 31 :inc -1) "x2345677")
  (binding-hex (:name y :width 32 :start 31 :inc -1) "12345678"))

(ex2)


(def-solver ex3 all-values vector->binstr
  ((:name a :width 4 :start 3 :inc -1)
   (:name b :width 4 :start 3 :inc -1)
   (:name c :width 4 :start 3 :inc -1)
   (:name d :width 4 :start 3 :inc -1))

  (vectorize ((:name a :width 4 :start 3 :inc -1)
              (:name b :width 4 :start 3 :inc -1)
              (:name c :width 4 :start 3 :inc -1)
              (:name d :width 4 :start 3 :inc -1))
;;   `(assert! (equalv ,a (xor2v ,b ,c))))
     `(assert! (equalv ,a (chv ,b ,c ,d))))
;;   `(assert! (equalv ,a (majv ,b ,c ,d))))

  (binding-bin (:name a :width 4 :start 3 :inc -1) "1111")
  (binding-bin (:name b :width 4 :start 3 :inc -1) "1111"))

(ex3)


(def-solver ex4 all-values vector->hexstr
  ((:name a :width 32 :start 31 :inc -1)
   (:name b :width 32 :start 31 :inc -1))

  (bsig0 (:name a :width 32 :start 31 :inc -1)
         (:name b :width 32 :start 31 :inc -1))

  (binding-hex (:name a :width 32 :start 31 :inc -1) "xxxxxxxx")
  (binding-hex (:name b :width 32 :start 31 :inc -1) "00000001"))

(ex4)


;; y = T1(e f g h k w)
(def-solver ex5 all-values vector->hexstr
  ((:name y :width 32 :start 31 :inc -1)
   (:name e :width 32 :start 31 :inc -1)
   (:name f :width 32 :start 31 :inc -1)
   (:name g :width 32 :start 31 :inc -1)
   (:name h :width 32 :start 31 :inc -1)
   (:name k :width 32 :start 31 :inc -1)
   (:name w :width 32 :start 31 :inc -1))

  (sha-t1 y e f g h k w)

  (binding-hex (:name e :width 32 :start 31 :inc -1) "a1b2c3d4")
  (binding-hex (:name f :width 32 :start 31 :inc -1) "19191919")
  (binding-hex (:name g :width 32 :start 31 :inc -1) "77775555")
  (binding-hex (:name h :width 32 :start 31 :inc -1) "aa55aa55")
  (binding-hex (:name k :width 32 :start 31 :inc -1) "f8e7d6c5")
  (binding-hex (:name w :width 32 :start 31 :inc -1) "12345678")
  (binding-hex (:name y :width 32 :start 31 :inc -1) "xxxxxxxx"))

(ex5)

;; test:
(defun-hexify hex-t1 (e f g h k w)
  (arith-add (arith-add h (arith-add (arith-bsig1 e)
                                     (arith-ch e f g) 32) 32)
             (arith-add k w 32) 32))

;; (hex-t1 "a1b2c3d4" "19191919" "77775555" "aa55aa55" "f8e7d6c5" "12345678")


;; y = T2(a b c)
(def-solver ex6 all-values vector->hexstr
  ((:name y :width 32 :start 31 :inc -1)
   (:name a :width 32 :start 31 :inc -1)
   (:name b :width 32 :start 31 :inc -1)
   (:name c :width 32 :start 31 :inc -1))

  (sha-t2 y a b c)

  (binding-hex (:name a :width 32 :start 31 :inc -1) "a1b2c3d4")
  (binding-hex (:name b :width 32 :start 31 :inc -1) "99999999")
  (binding-hex (:name c :width 32 :start 31 :inc -1) "12345678")
  (binding-hex (:name y :width 32 :start 31 :inc -1) "xxxxxxxx"))

(ex6)

;; test:
(defun-hexify hex-t2 (a b c)
  (arith-add (arith-bsig0 a)
             (arith-maj a b c)
             32))

;; (hex-t2 "a1b2c3d4" "99999999" "12345678")

;; Test suite

(deftest test ()
  (combine-results
   (test-macros)
   (test-utils)
   (test-arith)
   (test-group)
   (test-binding)
   (test-logic-gates)
   (test-basic-logic)
   (test-sha-logic)))

(test)
