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

(sha-logic-constraints)

;; create a solver on the problem bits
(sort
 (mapcar #'vector->hexstr
         (all-values
          (solution
           (list-global-groups sha-0 sha-1 sha-2 sha-3 sha-4 sha-5 sha-6 sha-7)
           #'linear-force)))
 #'string<)

;; ("4018A290:7717832B:F7C8D1DC:112CBFF0:1F71ABCC:C7BB73B7:3C111D91:9444E5C1")

;; $ echo -n 'abcdefghijklmnopqrstuvwx01234567' | sha256sum
;; 4018a2907717832bf7c8d1dc112cbff01f71abccc7bb73b73c111d919444e5c1  -
