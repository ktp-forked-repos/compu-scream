(require :screamer)
(in-package :screamer-user)


;; functions for SHA computations
;; optimized to not require intermediate signals

;;    a          b             c              d
;; BSIG0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)
(defun bsig-body (a b c d)
  (let ((groups (list a b c d)))
    (unroll-groups groups
       ``(assert! (equalv ,,(group-var a)
                          (xor3v ,,(group-var b)
                                 ,,(group-var c)
                                 ,,(group-var d)))))))

(defmacro bsig0 (a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-rot2 (rotate-group w 2 a b))
         (b-rot13 (group-var! (rotate-group w 13 a b) 'c))
         (b-rot22 (group-var! (rotate-group w 22 a b) 'd)))
    `(progn ,@(bsig-body a b-rot2 b-rot13 b-rot22))))

(mk-testcirc/groups test-bsig0-f partial-test-gen (bsig0 16) 32 32)
(deftest-fun test-bsig0 (bsig0-output 16))

;; BSIG1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)
(defmacro bsig1 (a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-rot6 (rotate-group w 6 a b))
         (b-rot11 (group-var! (rotate-group w 11 a b) 'c))
         (b-rot25 (group-var! (rotate-group w 25 a b) 'd)))
    `(progn ,@(bsig-body a b-rot6 b-rot11 b-rot25))))

(mk-testcirc/groups test-bsig1-f partial-test-gen (bsig1 16) 32 32)
(deftest-fun test-bsig1 (bsig1-output 16))

;; SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)
(defun ssig-body (a b c d)
  (let ((groups (list a b c d)))
    (unroll-groups groups
       ``(assert! (equalv ,,(group-var a)
                          (xor3v ,,(group-var b)
                                 ,,(group-var c)
                                 ,,(group-var d)))))))

(defun ssig-body (bits a b c d)
  (let ((groups (list a b c d)))
    (unroll-groups groups
      `(cond ((< idx ,bits) `(assert! (equalv ,,(group-var a)
                                              (xor2v ,,(group-var b)
                                                     ,,(group-var c)))))
             (t             `(assert! (equalv ,,(group-var a)
                                              (xor3v ,,(group-var b)
                                                     ,,(group-var c)
                                                     ,,(group-var d)))))))))

(defmacro ssig0 (a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-rot7 (rotate-group w 7 a b))
         (b-rot18 (group-var! (rotate-group w 18 a b) 'c))
         (b-rot3 (group-var! (rotate-group w 3 a b) 'd)))
    `(progn ,@(ssig-body 3 a b-rot7 b-rot18 b-rot3))))

(mk-testcirc/groups test-ssig0-f partial-test-gen (ssig0 16) 32 32)
(deftest-fun test-ssig0 (ssig0-output 16))

;; SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)
(defmacro ssig1 (a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-rot17 (rotate-group w 17 a b))
         (b-rot19 (group-var! (rotate-group w 19 a b) 'c))
         (b-rot10 (group-var! (rotate-group w 10 a b) 'd)))
    `(progn ,@(ssig-body 10 a b-rot17 b-rot19 b-rot10))))

(mk-testcirc/groups test-ssig1-f partial-test-gen (ssig1 16) 32 32)
(deftest-fun test-ssig1 (ssig1-output 16))

;; T1 = h + BSIG1(e) + CH(e,f,g) + Kt + Wt
;;  y   h         e       e f g    k    w
;;
;; ch   = CH(e,f,g)
;; bsig = BSIG1(e)
;; sum1 = bsig + ch
;; sum2 = h + sum1
;; sum3 = Kt + Wt
;; T1   = sum2 + sum3
(defmacro sha-t1 (y e f g h k w)
  (let ((ch (gensym))
        (bsig (gensym))
        (sum1 (gensym))
        (sum2 (gensym))
        (sum3 (gensym)))

    `(let-groups ,(def-groups ch bsig sum1 sum2 sum3)

       (vectorize ((:name ,ch :width 32 :start 31 :inc -1)
                   (:name ,e :width 32 :start 31 :inc -1)
                   (:name ,f :width 32 :start 31 :inc -1)
                   (:name ,g :width 32 :start 31 :inc -1))
        `(assert! (equalv ,,ch (chv ,,e ,,f ,,g))))

       (bsig1 (:name ,bsig :width 32 :start 31 :inc -1)
              (:name ,e :width 32 :start 31 :inc -1))

       (adder32 ,ch ,bsig ,sum1)
       (adder32 ,h ,sum1 ,sum2)
       (adder32 ,k ,w ,sum3)
       (adder32 ,sum2 ,sum3 ,y))))

;; T2 = BSIG0(a) + MAJ(a,b,c)
;;  y         a        a b c
;;
;; bsig = BSIG0(a)
;; maj  = MAJ(a,b,c)
(defmacro sha-t2 (y a b c)
  (let ((bsig (gensym))
        (maj (gensym)))

    `(let-groups ,(def-groups bsig maj)

       (vectorize ((:name ,maj :width 32 :start 31 :inc -1)
                   (:name ,a :width 32 :start 31 :inc -1)
                   (:name ,b :width 32 :start 31 :inc -1)
                   (:name ,c :width 32 :start 31 :inc -1))
        `(assert! (equalv ,,maj (majv ,,a ,,b ,,c))))

       (bsig0 (:name ,bsig :width 32 :start 31 :inc -1)
              (:name ,a :width 32 :start 31 :inc -1))

       (adder32 ,bsig ,maj ,y))))

;; generate constraints for a row of the SHA computation table
;; t = 0..63
;; T1[t] = e[t-4] + BSIG1(e[t-1]) + CH(e[t-1], e[t-2], e[t-3]) + K[t] + W[t]
;; T2[t] = BSIG0(a[t-1]) + MAJ(a[t-1], a[t-2], a[t-3])
;; a[t] = T1[t] + T2[t]
;; e[t] = a[t-4] + T1[t]

;; generate corresponding symbol with index [i-z] for a[i] and e[i]
;; handle special cases where i-z < 0
(defun mk-sym-z (g i z)
  (cond ((= i 0) (cond ((= z 1) (if (eql g 'a) 'h-0 'h-4))
                       ((= z 2) (if (eql g 'a) 'h-1 'h-5))
                       ((= z 3) (if (eql g 'a) 'h-2 'h-6))
                       ((= z 4) (if (eql g 'a) 'h-3 'h-7))))
        ((= i 1) (cond ((= z 1) (mk-signal-sym g 0))
                       ((= z 2) (if (eql g 'a) 'h-0 'h-4))
                       ((= z 3) (if (eql g 'a) 'h-1 'h-5))
                       ((= z 4) (if (eql g 'a) 'h-2 'h-6))))
        ((= i 2) (cond ((= z 1) (mk-signal-sym g 1))
                       ((= z 2) (mk-signal-sym g 0))
                       ((= z 3) (if (eql g 'a) 'h-0 'h-4))
                       ((= z 4) (if (eql g 'a) 'h-1 'h-5))))
        ((= i 3) (cond ((= z 1) (mk-signal-sym g 2))
                       ((= z 2) (mk-signal-sym g 1))
                       ((= z 3) (mk-signal-sym g 0))
                       ((= z 4) (if (eql g 'a) 'h-0 'h-4))))
        (t       (mk-signal-sym g (- i z)))))

(defmacro sha-row (i)
  (let ((a (mk-signal-sym 'a i))
        (az1 (mk-sym-z 'a i 1))
        (az2 (mk-sym-z 'a i 2))
        (az3 (mk-sym-z 'a i 3))
        (az4 (mk-sym-z 'a i 4))
        (e (mk-signal-sym 'e i))
        (ez1 (mk-sym-z 'e i 1))
        (ez2 (mk-sym-z 'e i 2))
        (ez3 (mk-sym-z 'e i 3))
        (ez4 (mk-sym-z 'e i 4))
        (t1 (mk-signal-sym 't1 i))
        (t2 (mk-signal-sym 't2 i))
        (k (mk-signal-sym 'k i))
        (w (mk-signal-sym 'w i)))
    `(progn (sha-t1 ,t1 ,ez1 ,ez2 ,ez3 ,ez4 ,k ,w)
            (sha-t2 ,t2 ,az1 ,az2 ,az3)
            (adder32 ,t1 ,t2 ,a)
            (adder32 ,az4 ,t1 ,e))))

;; generate constraints for the SHA message schedule, i = 16..63
;; Wt = SSIG1(W(t-2)) + W(t-7) + SSIG0(w(t-15)) + W(t-16)
(defmacro sha-msg-sched (i)
  (let ((wi (mk-signal-sym 'w i))
        (wiz2 (mk-signal-sym 'w (- i 2)))
        (wiz7 (mk-signal-sym 'w (- i 7)))
        (wiz15 (mk-signal-sym 'w (- i 15)))
        (wiz16 (mk-signal-sym 'w (- i 16)))
        (ssig0 (gensym))
        (ssig1 (gensym))
        (sum1 (gensym))
        (sum2 (gensym)))

    `(let-groups ,(def-groups ssig0 ssig1 sum1 sum2)

       (ssig1 (:name ,ssig1 :width 32 :start 31 :inc -1)
              (:name ,wiz2 :width 32 :start 31 :inc -1))
       (adder32 ,ssig1 ,wiz7 ,sum1)

       (ssig0 (:name ,ssig0 :width 32 :start 31 :inc -1)
              (:name ,wiz15 :width 32 :start 31 :inc -1))
       (adder32 ,ssig0 ,wiz16 ,sum2)

       (adder32 ,sum2 ,sum1 ,wi))))

(defmacro sha-out-adders ()
  `(progn (adder32 h-0 a-63 sha-0)
          (adder32 h-1 a-62 sha-1)
          (adder32 h-2 a-61 sha-2)
          (adder32 h-3 a-60 sha-3)
          (adder32 h-4 e-63 sha-4)
          (adder32 h-5 e-62 sha-5)
          (adder32 h-6 e-61 sha-6)
          (adder32 h-7 e-60 sha-7)))

(defmacro sha-logic-constraints ()
  `(progn (sha-unroll sha-msg-sched 16 63)
          (sha-unroll sha-row 0 63)
          (sha-out-adders)))


(defmacro sha-unroll (sha-form start end) ;; range inclusive!
  (do ((i start (1+ i))
       (body nil))
      ((> i end) `(progn ,@(nreverse body)))
    (setf body (cons `(,sha-form ,i) body))))

(defmacro sha-const-k ()
  `(const-groups (k-0 "428a2f98") (k-1 "71374491") (k-2 "b5c0fbcf") (k-3 "e9b5dba5")
                 (k-4 "3956c25b") (k-5 "59f111f1") (k-6 "923f82a4") (k-7 "ab1c5ed5")
                 (k-8 "d807aa98") (k-9 "12835b01") (k-10 "243185be") (k-11 "550c7dc3")
                 (k-12 "72be5d74") (k-13 "80deb1fe") (k-14 "9bdc06a7") (k-15 "c19bf174")
                 (k-16 "e49b69c1") (k-17 "efbe4786") (k-18 "0fc19dc6") (k-19 "240ca1cc")
                 (k-20 "2de92c6f") (k-21 "4a7484aa") (k-22 "5cb0a9dc") (k-23 "76f988da")
                 (k-24 "983e5152") (k-25 "a831c66d") (k-26 "b00327c8") (k-27 "bf597fc7")
                 (k-28 "c6e00bf3") (k-29 "d5a79147") (k-30 "06ca6351") (k-31 "14292967")
                 (k-32 "27b70a85") (k-33 "2e1b2138") (k-34 "4d2c6dfc") (k-35 "53380d13")
                 (k-36 "650a7354") (k-37 "766a0abb") (k-38 "81c2c92e") (k-39 "92722c85")
                 (k-40 "a2bfe8a1") (k-41 "a81a664b") (k-42 "c24b8b70") (k-43 "c76c51a3")
                 (k-44 "d192e819") (k-45 "d6990624") (k-46 "f40e3585") (k-47 "106aa070")
                 (k-48 "19a4c116") (k-49 "1e376c08") (k-50 "2748774c") (k-51 "34b0bcb5")
                 (k-52 "391c0cb3") (k-53 "4ed8aa4a") (k-54 "5b9cca4f") (k-55 "682e6ff3")
                 (k-56 "748f82ee") (k-57 "78a5636f") (k-58 "84c87814") (k-59 "8cc70208")
                 (k-60 "90befffa") (k-61 "a4506ceb") (k-62 "bef9a3f7") (k-63 "c67178f2")))

(deftest test-sha-logic ()
  (combine-results
   (test-bsig0)
   (test-bsig1)
   (test-ssig0)
   (test-ssig1)))
