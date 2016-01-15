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

    `(let-groups ((:name ,ch :width 32 :start 31 :inc -1)
                  (:name ,bsig :width 32 :start 31 :inc -1)
                  (:name ,sum1 :width 32 :start 31 :inc -1)
                  (:name ,sum2 :width 32 :start 31 :inc -1)
                  (:name ,sum3 :width 32 :start 31 :inc -1))

       (vectorize ((:name ,ch :width 32 :start 31 :inc -1)
                   (:name ,e :width 32 :start 31 :inc -1)
                   (:name ,f :width 32 :start 31 :inc -1)
                   (:name ,g :width 32 :start 31 :inc -1))
        `(assert! (equalv ,,ch (chv ,,e ,,f ,,g))))

       (bsig1 (:name ,bsig :width 32 :start 31 :inc -1)
              (:name ,e :width 32 :start 31 :inc -1))

       (rc-adder (:name ,ch :width 32 :start 31 :inc -1)
                 (:name ,bsig :width 32 :start 31 :inc -1)
                 (:name ,sum1 :width 32 :start 31 :inc -1))

       (rc-adder (:name ,h :width 32 :start 31 :inc -1)
                 (:name ,sum1 :width 32 :start 31 :inc -1)
                 (:name ,sum2 :width 32 :start 31 :inc -1))

       (rc-adder (:name ,k :width 32 :start 31 :inc -1)
                 (:name ,w :width 32 :start 31 :inc -1)
                 (:name ,sum3 :width 32 :start 31 :inc -1))

       (rc-adder (:name ,sum2 :width 32 :start 31 :inc -1)
                 (:name ,sum3 :width 32 :start 31 :inc -1)
                 (:name ,y :width 32 :start 31 :inc -1)))))

;; T2 = BSIG0(a) + MAJ(a,b,c)
;;  y         a        a b c
;;
;; bsig = BSIG0(a)
;; maj  = MAJ(a,b,c)
(defmacro sha-t2 (y a b c)
  (let ((bsig (gensym))
        (maj (gensym)))

    `(let-groups ((:name ,bsig :width 32 :start 31 :inc -1)
                  (:name ,maj :width 32 :start 31 :inc -1))

       (vectorize ((:name ,maj :width 32 :start 31 :inc -1)
                   (:name ,a :width 32 :start 31 :inc -1)
                   (:name ,b :width 32 :start 31 :inc -1)
                   (:name ,c :width 32 :start 31 :inc -1))
        `(assert! (equalv ,,maj (majv ,,a ,,b ,,c))))

       (bsig0 (:name ,bsig :width 32 :start 31 :inc -1)
              (:name ,a :width 32 :start 31 :inc -1))

       (rc-adder (:name ,bsig :width 32 :start 31 :inc -1)
                 (:name ,maj :width 32 :start 31 :inc -1)
                 (:name ,y :width 32 :start 31 :inc -1)))))


(deftest test-sha-logic ()
  (combine-results
   (test-bsig0)
   (test-bsig1)
   (test-ssig0)
   (test-ssig1)))
