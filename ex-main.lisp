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

;; set up the problem constraints

;; invert some functions y = f(x)
(def-global-groups x-bsig0 x-bsig1 x-ssig0 x-ssig1 y)

(const-groups (y "00000001"))
(bsig0 (:name y :width 32 :start 31 :inc -1)
       (:name x-bsig0 :width 32 :start 31 :inc -1))
(bsig1 (:name y :width 32 :start 31 :inc -1)
       (:name x-bsig1 :width 32 :start 31 :inc -1))
(ssig0 (:name y :width 32 :start 31 :inc -1)
       (:name x-ssig0 :width 32 :start 31 :inc -1))
(ssig1 (:name y :width 32 :start 31 :inc -1)
       (:name x-ssig1 :width 32 :start 31 :inc -1))

;; create a solver on the problem bits
(sort (mapcar #'vector->hexstr
 (all-values
  (solution
   (list-global-groups x-bsig0 x-bsig1 x-ssig0 x-ssig1)
   (static-ordering #'divide-and-conquer-force))))
      #'string<)

; (printg y x-bsig0 x-bsig1 x-ssig0 x-ssig1)

; (value->hexstr (arith-bsig0 (hexstr->value "BA2EE7E3")) 32)
; (value->hexstr (arith-bsig1 (hexstr->value "586686E1")) 32)
; (value->hexstr (arith-ssig0 (hexstr->value "B2D724DA")) 32)
; (value->hexstr (arith-ssig1 (hexstr->value "42BDBA97")) 32)
