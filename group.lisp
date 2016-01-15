(require :screamer)
(in-package :screamer-user)

;; create a signal symbol from a group symbol and a bit number
(defun mk-signal-sym (group bit)
  (intern (strcat (symbol-name group) "-" (format nil "~d" bit))))

(deftest-fun-args test-mk-signal-sym mk-signal-sym ('mux 3) 'mux-3)

;; group spec proplist getter functions
(defun group-name (g) (getf g :name))
(defun group-var (g) (getf g :var (group-name g)))
(defun group-width (g) (getf g :width 1))
(defun group-start (g) (getf g :start 0))
(defun group-inc (g) (getf g :inc 1))
(defun group-mod (g) (getf g :mod))

(deftest-fun-args test-group-name group-name ( '(:name mux) ) 'mux)
(deftest-fun-args test-group-name-empty group-name ( '(:width 8) ) nil)
(deftest-fun-args test-group-var group-var ( '(:name mux :var mi) ) 'mi)
(deftest-fun-args test-group-var-empty group-var ( '(:name mux) ) 'mux)
(deftest-fun-args test-group-width group-width ( '(:name mux :width 8) ) 8)
(deftest-fun-args test-group-width-empty group-width ( '(:name mux) ) 1)
(deftest-fun-args test-group-start group-start ( '(:name mux :start 7) ) 7)
(deftest-fun-args test-group-start-empty group-start ( '(:name mux) ) 0)
(deftest-fun-args test-group-inc group-inc ( '(:name mux :inc -1) ) -1)
(deftest-fun-args test-group-inc-empty group-inc ( '(:name mux) ) 1)
(deftest-fun-args test-group-mod group-mod ( '(:name mux :mod 16) ) 16)
(deftest-fun-args test-group-mod-empty group-mod ( '(:name mux) ) nil)

(deftest test-group-getters ()
  (combine-results
   (test-group-name) (test-group-name-empty)
   (test-group-var) (test-group-var-empty)
   (test-group-width) (test-group-width-empty)
   (test-group-start) (test-group-start-empty)
   (test-group-inc) (test-group-inc-empty)
   (test-group-mod) (test-group-mod-empty)))

(defun group-var! (g v) (update-plist g :var v))
(defun group-start! (g v) (update-plist g :start v))
(defun group-mod! (g v) (update-plist g :mod v))

;; rotate group b with respect to group a
;; by given bits on width w
(defun rotate-group (w bits a b)
  (let ((b-shifted (mod (+ (group-start a) bits) w)))
    (group-mod! (group-start! b b-shifted) w)))


;; unroll-groups helper function
(defun unroll-groups-do-body (groups)
  (append
   `((body-out nil)
     (idx 0 (incf idx)))
   (mapcar #'(lambda (g)
               (let ((var   (group-var g))
                     (start (group-start g))
                     (inc   (group-inc g))
                     (mod   (group-mod g)))
                 `(,var ,start ,(if (null mod)
                                    `(+ ,var ,inc)
                                    `(mod (+ ,var ,inc) ,mod)))))
           groups)))

(deftest-fun-args test-unroll-groups-do-body
  unroll-groups-do-body ( '((:name a :start 0 :width 7)
                            (:name b :start 7 :inc -1 :mod 8)) )
  '((body-out nil)
    (idx 0 (incf idx))
    (a 0 (+ a 1))
    (b 7 (mod (+ b -1) 8))) )

;; Function to define macros or functions that operate on logic vectors
;; called groups. The function generates code that needs to be evaluated
;; to generate the desired form expansion and substitution.
;;
;; Syntax:
;;   - groups is a list of group specifications, where
;;     each group spec is a plist with the following keys:
;;
;;     :name   signal group name
;;     :var    variable to substitute in forms [default: name]
;;     :width  number of bits [default: 1]
;;     :start  index of first bit [default: 0]
;;     :inc    index increment (may be negative) [default: 1]
;;     :mod    index modulo [default: nil]
;;
;;   - forms is a body containing references to group vars.
;;
;; Notes:
;;   - A non-nil :mod is useful to generate bit rotation patterns.
;;   - Group widths don't have to be the same; the smallest one
;;     will be used.
;;   - If the evaluation of forms yields nil for any index, that
;;     will be omitted from the output. So the number of expansions
;;     may be less than the minimum group width.
;;
(defun unroll-groups (groups &rest forms)
  (let ((names (mapcar #'group-name groups))
        (vars  (mapcar #'group-var groups))
        (width (apply #'min (mapcar #'group-width groups)))
        (do-body (unroll-groups-do-body groups)))
    (let ((body `(do ,do-body
                     ((>= idx ,width)
                      (nreverse body-out))
                   (let ,(mapcar #'(lambda (n v)
                                     (list v `(mk-signal-sym ',n ,v)))
                                 names vars)
                     (let ((body-form ,@forms))
                       (if (not (null body-form))
                           (setf body-out (cons body-form body-out))))))))
      (eval body))))

(defun test-unroll-groups-f ()
  (unroll-groups '((:name a :var ai :start 7 :width 8 :inc -1)
                   (:name b :start 15 :width 8 :inc -1)
                   (:name c :start 5 :width 8 :mod 8))
                 '`(assert! (equalv ,ai (xorv ,b ,c)))))

(deftest-fun-args test-unroll-groups
  test-unroll-groups-f ()
  '((assert! (equalv a-7 (xorv b-15 c-5)))
    (assert! (equalv a-6 (xorv b-14 c-6)))
    (assert! (equalv a-5 (xorv b-13 c-7)))
    (assert! (equalv a-4 (xorv b-12 c-0)))
    (assert! (equalv a-3 (xorv b-11 c-1)))
    (assert! (equalv a-2 (xorv b-10 c-2)))
    (assert! (equalv a-1 (xorv b-9 c-3)))
    (assert! (equalv a-0 (xorv b-8 c-4)))))


(defmacro vectorize (groups body)
  `(progn ,@(unroll-groups groups body)))

(deftest-fun-args test-vectorize
  macroexpand-1 ('(vectorize ((:name a :width 4 :start 3 :inc -1)
                              (:name b :width 4 :start 3 :inc -1)
                              (:name c :width 4 :start 3 :inc -1))
                     `(assert! (equalv ,a (xor2v ,b ,c)))) )
  '(progn
    (assert! (equalv a-3 (xor2v b-3 c-3)))
    (assert! (equalv a-2 (xor2v b-2 c-2)))
    (assert! (equalv a-1 (xor2v b-1 c-1)))
    (assert! (equalv a-0 (xor2v b-0 c-0)))))


;; let-groups
;; Macro to wrap body in locally defined signal groups

(defun mk-index-seq (width start inc mod)
  (do ((idx 0 (1+ idx))
       (seq nil (cons i seq))
       (i start (if (null mod)
                    (+ i inc)
                    (mod (+ i inc) mod))))
      ((>= idx width) (nreverse seq))))

(defun group-syms (g)
  (let ((name (group-name g))
        (seq  (mk-index-seq (group-width g)
                            (group-start g)
                            (group-inc g)
                            (group-mod g))))
    (mapcar #'(lambda (idx) (mk-signal-sym name idx)) seq)))

(deftest-fun-args test-group-syms
  group-syms ('(:name d :width 3 :start 2 :inc -1))
  '(d-2 d-1 d-0))


(defun group-defs (g)
  (let ((syms (group-syms g)))
    (mapcar #'(lambda (s) (list s '(a-booleanv))) syms)))

(deftest-fun-args test-group-defs
  group-defs ('(:name d :width 3 :start 2 :inc -1))
  '((d-2 (a-booleanv)) (d-1 (a-booleanv)) (d-0 (a-booleanv))))


(defmacro let-groups (groups &body body)
  `(let ,(mapcan #'group-defs groups)
     ,@body))

(defmacro list-groups (&rest groups)
  `(list ,@(mapcan #'group-syms groups)))

(defun def-groups (&rest group-specs)
  (mapcar #'(lambda (g)
              (if (symbolp g)
                  (list :name g :width 32 :start 31 :inc -1)
                  (let ((name (car g))
                        (width (cadr g)))
                    (list :name name :width width
                          :start (1- width) :inc -1))))
          group-specs))

(defmacro std-logic-vector (name width)
  `(list :name ,name :width ,width :start ,(1- width) :inc -1))

(deftest test-group ()
  (combine-results
   (test-mk-signal-sym)
   (test-group-getters)
   (test-unroll-groups-do-body)
   (test-unroll-groups)
   (test-vectorize)
   (test-group-syms)
   (test-group-defs)))
