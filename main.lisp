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


;; y = BSIG0(e) + CH(e,f,g)
(def-solver ex5 all-values vector->hexstr
  ((:name y :width 32 :start 31 :inc -1)
   (:name e :width 32 :start 31 :inc -1)
   (:name f :width 32 :start 31 :inc -1)
   (:name g :width 32 :start 31 :inc -1))

  (let-groups ((:name ch :width 32 :start 31 :inc -1)
               (:name bsig :width 32 :start 31 :inc -1))

     (vectorize ((:name ch :width 32 :start 31 :inc -1)
                 (:name e :width 32 :start 31 :inc -1)
                 (:name f :width 32 :start 31 :inc -1)
                 (:name g :width 32 :start 31 :inc -1))
        `(assert! (equalv ,ch (chv ,e ,f ,g))))

     (bsig0 (:name bsig :width 32 :start 31 :inc -1)
            (:name e :width 32 :start 31 :inc -1))

     (rc-adder (:name ch :width 32 :start 31 :inc -1)
               (:name bsig :width 32 :start 31 :inc -1)
               (:name y :width 32 :start 31 :inc -1)))

  (binding-hex (:name e :width 32 :start 31 :inc -1) "12345678")
  (binding-hex (:name f :width 32 :start 31 :inc -1) "fedcba98")
  (binding-hex (:name g :width 32 :start 31 :inc -1) "1f2e3d4c")
  (binding-hex (:name y :width 32 :start 31 :inc -1) "xxxxxxxx"))

(ex5)
;; -> ("85329F90:12345678:FEDCBA98:1F2E3D4C")
;;
;; check:
;;
;; (defun-hexify ytest (e f g)
;;   (arith-add (arith-bsig0 e)
;;              (arith-ch e f g)
;;              32))
;;
;; (ytest "12345678" "FEDCBA98" "1F2E3D4C") -> "85329F90"


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
