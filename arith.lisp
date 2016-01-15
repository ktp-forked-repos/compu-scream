(require :screamer)
(in-package :screamer-user)

;; ADD a and b on width w
(defun arith-add (a b w)
  (logand (+ a b)
          (1- (ash 1 w))))

(deftest-fun-args test-add-t1 arith-add (14 1 4) 15)
(deftest-fun-args test-add-t2 arith-add (14 2 4) 0)
(deftest-fun-args test-add-t3 arith-add (254 1 8) 255)
(deftest-fun-args test-add-t4 arith-add (254 2 8) 0)

(deftest test-arith-add ()
  (combine-results
   (test-add-t1)
   (test-add-t2)
   (test-add-t3)
   (test-add-t4)))

(defun arith-ch (x y z)
  (logxor (logand x y)
          (logand (lognot x) z)))

(deftest-fun-args test-ch-t1 arith-ch (#x96 #xAA #x55) #xC3)
(deftest-fun-args test-ch-t2 arith-ch (#xAB #xCD #xEF) #xCD)
(deftest-fun-args test-ch-t3 arith-ch (#x12 #x34 #x56) #x54)

(deftest test-arith-ch ()
  (combine-results
   (test-ch-t1)
   (test-ch-t2)
   (test-ch-t3)))

(defun arith-maj (x y z)
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

(deftest-fun-args test-maj-t1 arith-maj (#x96 #xAA #x55) #x96)
(deftest-fun-args test-maj-t2 arith-maj (#xAB #xCD #xEF) #xEF)
(deftest-fun-args test-maj-t3 arith-maj (#x12 #x34 #x56) #x16)

(deftest test-arith-maj ()
  (combine-results
   (test-maj-t1)
   (test-maj-t2)
   (test-maj-t3)))

;; ROTL and ROTR given amount of bits on x of width w
(defun arith-rotl (x w bits)
  (logior (logand (ash x (mod bits w))
                  (1- (ash 1 w)))
          (logand (ash x (- (- w (mod bits w))))
                  (1- (ash 1 w)))))

(defun arith-rotr (x w bits)
  (logior (logand (ash x (- (mod bits w)))
                  (1- (ash 1 w)))
          (logand (ash x (- w (mod bits w)))
                  (1- (ash 1 w)))))

(deftest-fun-args test-rotl-t1 arith-rotl (3 4 1) 6)
(deftest-fun-args test-rotl-t2 arith-rotl (3 4 2) 12)
(deftest-fun-args test-rotl-t3 arith-rotl (3 4 3) 9)
(deftest-fun-args test-rotl-t4 arith-rotl (3 4 4) 3)
(deftest-fun-args test-rotr-t1 arith-rotr (123 8 1) 189)
(deftest-fun-args test-rotr-t2 arith-rotr (123 8 2) 222)
(deftest-fun-args test-rotr-t3 arith-rotr (123 8 3) 111)
(deftest-fun-args test-rotr-t4 arith-rotr (123 8 4) 183)

(deftest test-arith-rot ()
  (combine-results
   (test-rotl-t1)
   (test-rotl-t2)
   (test-rotl-t3)
   (test-rotl-t4)
   (test-rotr-t1)
   (test-rotr-t2)
   (test-rotr-t3)
   (test-rotr-t4)))

;; SHL and SHR given amount of bits on x of width w
(defun arith-shl (x w bits)
  (logand (ash x bits)
          (1- (ash 1 w))))

(defun arith-shr (x w bits)
  (arith-shl x w (- bits)))

(deftest-fun-args test-shl-t1 arith-shl (3 4 1) 6)
(deftest-fun-args test-shl-t2 arith-shl (3 4 2) 12)
(deftest-fun-args test-shl-t3 arith-shl (3 4 3) 8)
(deftest-fun-args test-shl-t4 arith-shl (3 4 4) 0)
(deftest-fun-args test-shr-t1 arith-shr (123 8 1) 61)
(deftest-fun-args test-shr-t2 arith-shr (123 8 2) 30)
(deftest-fun-args test-shr-t3 arith-shr (123 8 3) 15)
(deftest-fun-args test-shr-t4 arith-shr (123 8 4) 7)

(deftest test-arith-shift ()
  (combine-results
   (test-shl-t1)
   (test-shl-t2)
   (test-shl-t3)
   (test-shl-t4)
   (test-shr-t1)
   (test-shr-t2)
   (test-shr-t3)
   (test-shr-t4)))


;; SHA functions
;; BSIG0(x) = ROTR^2(x) XOR ROTR^13(x) XOR ROTR^22(x)
(defun arith-bsig0 (x)
  (logxor (arith-rotr x 32 2)
          (arith-rotr x 32 13)
          (arith-rotr x 32 22)))

;; BSIG1(x) = ROTR^6(x) XOR ROTR^11(x) XOR ROTR^25(x)
(defun arith-bsig1 (x)
  (logxor (arith-rotr x 32 6)
          (arith-rotr x 32 11)
          (arith-rotr x 32 25)))

;; SSIG0(x) = ROTR^7(x) XOR ROTR^18(x) XOR SHR^3(x)
(defun arith-ssig0 (x)
  (logxor (arith-rotr x 32 7)
          (arith-rotr x 32 18)
          (arith-shr x 32 3)))

;; SSIG1(x) = ROTR^17(x) XOR ROTR^19(x) XOR SHR^10(x)
(defun arith-ssig1 (x)
  (logxor (arith-rotr x 32 17)
          (arith-rotr x 32 19)
          (arith-shr x 32 10)))

;; T1 = h + BSIG1(e) + CH(e,f,g) + Kt + Wt
(defun arith-t1 (e f g h k w)
  (arith-add (arith-add h
                        (arith-add (arith-bsig1 e)
                                   (arith-ch e f g)
                                   32)
                        32)
             (arith-add k w 32)
             32))

;; T2 = BSIG0(a) + MAJ(a,b,c)
(defun arith-t2 (a b c)
  (arith-add (arith-bsig0 a)
             (arith-maj a b c)
             32))

;; Ai = T1 + T2
;; Ei = D(i-1) + T1
(defun arith-sha-row (az1 az2 az3 az4 ez1 ez2 ez3 ez4 k w)
  (let* ((t1 (arith-t1 ez1 ez2 ez3 ez4 k w))
         (t2 (arith-t2 az1 az2 az3))
         (a (arith-add t1 t2 32))
         (e (arith-add az4 t1 32)))
    (+ (ash t1 96)
       (ash t2 64)
       (ash a 32)
       e)))

(deftest test-arith ()
  (combine-results
   (test-arith-add)
   (test-arith-ch)
   (test-arith-maj)
   (test-arith-rot)
   (test-arith-shift)))
