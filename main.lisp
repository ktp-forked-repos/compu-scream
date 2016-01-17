(require :screamer)
(in-package :screamer-user)

(setf (sb-ext:gc-logfile) "gc.log")

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

;; (ex1)


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

;; (ex2)


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

;; (ex3)


(def-solver ex4 all-values vector->hexstr
  ((:name a :width 32 :start 31 :inc -1)
   (:name b :width 32 :start 31 :inc -1))

  (bsig0 (:name a :width 32 :start 31 :inc -1)
         (:name b :width 32 :start 31 :inc -1))

  (binding-hex (:name a :width 32 :start 31 :inc -1) "xxxxxxxx")
  (binding-hex (:name b :width 32 :start 31 :inc -1) "00000001"))

;; (ex4)


;; y = T1(e f g h k w)
(def-solver ex5 all-values vector->hexstr
  (def-groups 'y 'e 'f 'g 'h 'k 'w)

  (const-groups (e "a1b2c3d4") (f "19191919") (g "77775555") (h "aa55aa55")
                (k "f8e7d6c5") (w "12345678")
                (y "xxxxxxxx"))

  (sha-t1 y e f g h k w))

(ex5)

;; test:
(defun-hexify hex-t1 (e f g h k w)
  (arith-add (arith-add h (arith-add (arith-bsig1 e)
                                     (arith-ch e f g) 32) 32)
             (arith-add k w 32) 32))

;; (hex-t1 "a1b2c3d4" "19191919" "77775555" "aa55aa55" "f8e7d6c5" "12345678")
;; -> "FE3A03AA"


;; y = T2(a b c)
(def-solver ex6 all-values vector->hexstr
  (def-groups 'y 'a 'b 'c)

  (const-groups (a "a1b2c3d4") (b "99999999") (c "12345678")
                (y "xxxxxxxx"))
  (sha-t2 y a b c))

(ex6)

;; test:
(defun-hexify hex-t2 (a b c)
  (arith-add (arith-bsig0 a)
             (arith-maj a b c)
             32))

;; (hex-t2 "a1b2c3d4" "99999999" "12345678") -> "8F77C3BD"


;; SHA row
(def-solver ex7 all-values vector->hexstr
  (def-groups
    'h-0 'h-1 'h-2 'h-3 'h-4 'h-5 'h-6 'h-7
    'k-0
    'w-0
    't1-0 't2-0
    'a-0 'e-0)

  (const-groups (h-0 "6a09e667") (h-1 "bb67ae85") (h-2 "3c6ef372") (h-3 "a54ff53a")
                (h-4 "510e527f") (h-5 "9b05688c") (h-6 "1f83d9ab") (h-7 "5be0cd19"))
  (const-groups (k-0 "428a2f98"))
  (const-groups (w-0 "12345678"))
  (sha-row 0))

;; try something out of the scope of solver...
(def-global-groups h-0 h-1 h-2 h-3 h-4 h-5 h-6 h-7
                   k-0 w-0 t1-0 t2-0 a-0 e-0)
(const-groups (h-0 "6a09e667") (h-1 "bb67ae85") (h-2 "3c6ef372") (h-3 "a54ff53a")
              (h-4 "510e527f") (h-5 "9b05688c") (h-6 "1f83d9ab") (h-7 "5be0cd19"))
(const-groups (k-0 "428a2f98"))
(const-groups (w-0 "12345678"))
(sha-row 0)

;; (ex7)

;; test:
(defun-hexify hex-sha-row (az1 az2 az3 az4 ez1 ez2 ez3 ez4 k w)
  (let* ((t1 (arith-t1 ez1 ez2 ez3 ez4 k w))
         (t2 (arith-t2 az1 az2 az3))
         (a (arith-add t1 t2 32))
         (e (arith-add az4 t1 32)))
    (+ (ash t1 96)
       (ash t2 64)
       (ash a 32)
       e)))

(hex-sha-row "6a09e667" "bb67ae85" "3c6ef372" "a54ff53a"
             "510e527f" "9b05688c" "1f83d9ab" "5be0cd19"
             "428a2f98" "12345678")
;; -> "05AC43E0:08909AE5:0E3CDEC5:AAFC391A"

;; SHA row with message schedule
(def-solver-lite ex8 all-values vector->hexstr
  (def-groups
    'h-0 'h-1 'h-2 'h-3 'h-4 'h-5 'h-6 'h-7
    'k-0
    'w-0 'w-1 'w-2 'w-3 'w-4 'w-5 'w-6 'w-7
    'w-8 'w-9 'w-10 'w-11 'w-12 'w-13 'w-14 'w-15
    'w-16 'w-17 'w-18 'w-19
    't1-0 't2-0
    'a-0 'e-0)

  (const-groups (h-0 "6a09e667") (h-1 "bb67ae85") (h-2 "3c6ef372") (h-3 "a54ff53a")
                (h-4 "510e527f") (h-5 "9b05688c") (h-6 "1f83d9ab") (h-7 "5be0cd19"))
  (const-groups (k-0 "428a2f98"))

  (const-groups (w-0 "00000001") (w-1 "00000002") (w-2 "00000003") (w-3 "00000000")
                (w-4 "00000000") (w-5 "00000000") (w-6 "00000000") (w-7 "00000000")
                (w-8 "00000000") (w-9 "00000000") (w-10 "00000000") (w-11 "00000000")
                (w-12 "00000000") (w-13 "00000000") (w-14 "00000000") (w-15 "00000000"))

  (sha-msg-sched 16) (sha-msg-sched 17) (sha-msg-sched 18) (sha-msg-sched 19)
  (sha-row 0))

;; try something out of the scope of solver...
(def-global-groups h-0 h-1 h-2 h-3 h-4 h-5 h-6 h-7
                   k-0
                   w-0 w-1 w-2 w-3 w-4 w-5 w-6 w-7
                   w-8 w-9 w-10 w-11 w-12 w-13 w-14 w-15
                   w-16 w-17 w-18 w-19
                   t1-0 t2-0
                   a-0 e-0)
(const-groups (h-0 "6a09e667") (h-1 "bb67ae85") (h-2 "3c6ef372") (h-3 "a54ff53a")
              (h-4 "510e527f") (h-5 "9b05688c") (h-6 "1f83d9ab") (h-7 "5be0cd19"))
(const-groups (k-0 "428a2f98"))

(const-groups (w-0 "00000001") (w-1 "00000002") (w-2 "00000003") (w-3 "00000000")
              (w-4 "00000000") (w-5 "00000000") (w-6 "00000000") (w-7 "00000000")
              (w-8 "00000000") (w-9 "00000000") (w-10 "00000000") (w-11 "00000000")
              (w-12 "00000000") (w-13 "00000000") (w-14 "00000000") (w-15 "00000000"))

(sha-msg-sched 16) (sha-msg-sched 17) (sha-msg-sched 18) (sha-msg-sched 19)
(sha-row 0)

;; create a solver on the problem bits
;; TODO macro def-global-solver
(sort
 (mapcar #'vector->hexstr
         (all-values
          (solution
           (list-global-groups a-0 e-0)
           #'linear-force)))
 #'string<)


;; (ex8)

(defun-hexify hex-sha-msg-sched (msg)
  (let ((w (make-array 64 :element-type 'integer)))
    (do ((i 0 (1+ i))
         (m msg (cdr m)))
        ((= i 64) w)
      (if (< i 16)
          (if m (setf (aref w i) (car m)))
          (setf (aref w i) (arith-wt (aref w (- i 2))
                                     (aref w (- i 7))
                                     (aref w (- i 15))
                                     (aref w (- i 16))))))))


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
