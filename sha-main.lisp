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

;; set up the SHA256 problem constraints

(def-global-groups (h 8) (k 64) (w 64) (t1 64) (t2 64) (a 64) (e 64) (sha 8))

(sha-const-k)
(const-groups (h-0 "6a09e667") (h-1 "bb67ae85") (h-2 "3c6ef372") (h-3 "a54ff53a")
              (h-4 "510e527f") (h-5 "9b05688c") (h-6 "1f83d9ab") (h-7 "5be0cd19"))

;; abcd efgh ijkl mnop
;; qrst uvwx 0123 4567
(const-groups (w-0 "61626364") (w-1 "65666768") (w-2 "696a6b6c") (w-3 "6d6e6f70")
              (w-4 "71727374") (w-5 "75767778") (w-6 "30313233") (w-7 "34353637")
              (w-8 "80000000") (w-9 "00000000") (w-10 "00000000") (w-11 "00000000")
              (w-12 "00000000") (w-13 "00000000") (w-14 "00000000") (w-15 "00000100"))

;(const-groups (sha-0 "00000000") (sha-1 "00000000") (sha-2 "00000000") (sha-3 "00000000")
;              (sha-4 "00000000") (sha-5 "00000000") (sha-6 "00000000") (sha-7 "000000ff"))

;(const-groups (sha-0 "4018A290") (sha-1 "7717832B") (sha-2 "F7C8D1DC") (sha-3 "112CBFF0")
;              (sha-4 "1F71ABCC") (sha-5 "C7BB73B7") (sha-6 "3C111D91") (sha-7 "9444E5C1"))

(sha-logic-constraints)

; (printg w-0 w-1 w-2 w-3 w-4 w-5 w-6 w-7)
; (printg w-8 w-9 w-10 w-11 w-12 w-13 w-14 w-15)
; (printg w-56 w-57 w-58 w-59 w-60 w-61 w-62 w-63)

; (printg h-0 h-1 h-2 h-3 h-4 h-5 h-6 h-7)
; (format t "   W    :   T1   :   T2   :   Ai   :   Ai1  :   Ai2  :  Ai3   :   Ei   :   Ei1  :   Ei2  :   Ei3  ~%")
; (sha-unroll print-sha-row 0 63)
; (printg sha-0 sha-1 sha-2 sha-3 sha-4 sha-5 sha-6 sha-7)

;; create a solver on the problem bits
(vector->hexstr
 (one-value
  (solution
   (list-global-groups sha-0 sha-1 sha-2 sha-3 sha-4 sha-5 sha-6 sha-7)
   (static-ordering #'divide-and-conquer-force))))
;;;;;;;;;;
;   (reorder #'domain-size
;            #'(lambda (x) (declare (ignore x)) nil)
;            #'<
;            #'linear-force))))


;; ("4018A290:7717832B:F7C8D1DC:112CBFF0:1F71ABCC:C7BB73B7:3C111D91:9444E5C1")

;; $ echo -n 'abcdefghijklmnopqrstuvwx01234567' | sha256sum
;; 4018a2907717832bf7c8d1dc112cbff01f71abccc7bb73b73c111d919444e5c1  -
