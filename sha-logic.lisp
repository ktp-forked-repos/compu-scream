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


(deftest test-sha-logic ()
  (combine-results
   (test-bsig0)
   (test-bsig1)
   (test-ssig0)
   (test-ssig1)))
