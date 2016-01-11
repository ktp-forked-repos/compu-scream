(require :screamer)
(in-package :screamer-user)

(load "test.lisp")
(load "macros.lisp")
(load "utils.lisp")
(load "group.lisp")

;; extensions to allow gate-style logic computations

(defmacro nandv (&rest v)
  `(notv (andv ,@v)))

(defmacro norv (&rest v)
  `(notv (orv ,@v)))

(defmacro xor2v (x y)
  `(orv (andv ,x (notv ,y)) (andv (notv ,x) ,y)))

(defmacro xor3v (x y z)
  `(xor2v ,x (xor2v ,y ,z)))

(defmacro xor4v (x y z w)
  `(xor2v (xor2v ,x ,y) (xor2v ,z ,w)))

(defmacro chv (x y z)
  `(xor2v (andv ,x ,y)
          (andv (notv ,x) ,z)))

(defmacro majv (x y z)
  `(xor3v (andv ,x ,y)
          (andv ,x ,z)
          (andv ,y ,z)))

;; test function generators

(defun mk-vars (arity)
  (let ((vars nil))
    (dotimes (i arity)
      (push (gensym) vars))
    vars))

(defmacro mk-testfun-body (fun arity)
  (let ((vars (mk-vars arity)))
    `(let ,(mapcar #'(lambda (v) (list v '(a-booleanv))) vars)
       (assert! (,fun ,@vars))
       (list ,@vars))))

(defmacro mk-testfun (name fun arity)
  `(defun ,name ()
     (sort (mapcar #'vector->binstr
                   (all-values
                    (solution
                     (mk-testfun-body ,fun ,arity)
                     (reorder #'domain-size
                              #'(lambda (x) (declare (ignore x)) nil)
                              #'<
                              #'linear-force))))
           #'string<)))

(mk-testfun test-not-f notv 1)
(mk-testfun test-and-f andv 2)
(mk-testfun test-nand-f nandv 2)
(mk-testfun test-nand3-f nandv 3)
(mk-testfun test-or-f orv 2)
(mk-testfun test-nor-f norv 2)
(mk-testfun test-nor3-f norv 3)
(mk-testfun test-xor2-f xor2v 2)
(mk-testfun test-xor3-f xor3v 3)
(mk-testfun test-xor4-f xor4v 4)
(mk-testfun test-chv-f chv 3)
(mk-testfun test-majv-f majv 3)

(deftest-fun test-not '("0"))
(deftest-fun test-and '("11"))
(deftest-fun test-nand '("00" "01" "10"))
(deftest-fun test-nand3 '("000" "001" "010" "011" "100" "101" "110"))
(deftest-fun test-or '("01" "10" "11"))
(deftest-fun test-nor '("00"))
(deftest-fun test-nor3 '("000"))
(deftest-fun test-xor2 '("01" "10") )
(deftest-fun test-xor3 '("001" "010" "100" "111"))
(deftest-fun test-xor4 '("0001" "0010" "0100" "0111"
                         "1000" "1011" "1101" "1110"))
(deftest-fun test-chv '("001" "011" "110" "111"))
(deftest-fun test-majv '("011" "101" "110" "111"))

(deftest test-logic-gates ()
  (combine-results
   (test-not)
   (test-and) (test-nand) (test-nand3)
   (test-or) (test-nor) (test-nor3)
   (test-xor2) (test-xor3) (test-xor4)
   (test-chv) (test-majv)))

;; some basic circuits for digital computations

(defmacro half-adder (a b c s)
  `(progn (assert! (equalv ,s (xor2v ,a ,b)))
          (assert! (equalv ,c (andv ,a ,b)))))

(defmacro full-adder-s (a b ci s)
  `(assert! (equalv ,s (xor3v ,a ,b ,ci))))

(defmacro full-adder (a b ci co s)
  `(progn (full-adder-s ,a ,b ,ci ,s)
          (assert! (equalv ,co (orv (andv ,a ,b)
                                    (andv ,ci
                                          (xor2v ,a ,b)))))))

;; generic ripple-carry adder structure
(defun mk-rc-adder-body (w a b c s)
  (let ((groups `((:name ,a :width ,w)
                  (:name ,b :width ,w)
                  (:name ,s :width ,w)
                  (:name ,c :var c0 :width ,w)
                  (:name ,c :var c1 :width ,w :start 1))))
    (unroll-groups groups
       `(cond ((= idx 0)       `(half-adder   ,,a ,,b ,c1     ,,s))
              ((< idx (1- ,w)) `(full-adder   ,,a ,,b ,c0 ,c1 ,,s))
              (t               `(full-adder-s ,,a ,,b ,c0     ,,s))))))

(defmacro mk-rc-adder (name a b s)
  (let* ((w (apply #'min (mapcar #'group-width (list a b s))))
         (c (gensym)))
    `(defun ,name ,(mapcan #'group-syms (list a b s))
       (let ,(group-defs `(:name ,c :start 1 :width ,(1- w)))
          ,@(eval (mk-rc-adder-body w
                                    (group-name a)
                                    (group-name b)
                                    c
                                    (group-name s)))))))

;; create some example functions that create adders over given groups
(mk-rc-adder rc-adder-3
             (:name x :width 3 :start 2 :inc -1)
             (:name y :width 3 :start 2 :inc -1)
             (:name r :width 3 :start 2 :inc -1))

(mk-rc-adder rc-adder-4
             (:name a :width 4 :start 3 :inc -1)
             (:name b :width 4 :start 3 :inc -1)
             (:name s :width 4 :start 3 :inc -1))

(mk-rc-adder rc-adder-5
             (:name a :width 5 :start 4 :inc -1)
             (:name b :width 5 :start 4 :inc -1)
             (:name s :width 5 :start 4 :inc -1))

(mk-rc-adder rc-adder-6
             (:name a :width 6 :start 5 :inc -1)
             (:name b :width 6 :start 5 :inc -1)
             (:name s :width 6 :start 5 :inc -1))

(mk-rc-adder rc-adder-7
             (:name a :width 7 :start 6 :inc -1)
             (:name b :width 7 :start 6 :inc -1)
             (:name s :width 7 :start 6 :inc -1))

(mk-rc-adder rc-adder-8
             (:name a :width 8 :start 7 :inc -1)
             (:name b :width 8 :start 7 :inc -1)
             (:name s :width 8 :start 7 :inc -1))

;; generic bit rotate operations
(defun mk-rotate-body (a b)
  (let ((groups (list a b)))
    (unroll-groups groups
       ``(assert! (equalv ,,(group-name a) ,,(group-name b))))))

;; a = ROTL^bits(b) for MSB-first vectors
(defmacro mk-rotate-left (name bits a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-shifted (mod (- (group-start a) bits) w))
         (b-rotated (group-mod! (group-start! b b-shifted) w)))
    `(defun ,name ,(mapcan #'group-syms (list a b))
       ,@(eval (mk-rotate-body a b-rotated)))))

;; a = ROTR^bits(b) for MSB-first vectors
(defmacro mk-rotate-right (name bits a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-shifted (mod (+ (group-start a) bits) w))
         (b-rotated (group-mod! (group-start! b b-shifted) w)))
    `(defun ,name ,(mapcan #'group-syms (list a b))
       ,@(eval (mk-rotate-body a b-rotated)))))

;; create some example functions that create adders over given groups

(mk-rotate-left rotl1-8
                1
                (:name a :width 8 :start 7 :inc -1)
                (:name b :width 8 :start 7 :inc -1))

(mk-rotate-right rotr1-8
                 1
                 (:name a :width 8 :start 7 :inc -1)
                 (:name b :width 8 :start 7 :inc -1))

(mk-rotate-right rotr9-16
                 9
                 (:name a :width 16 :start 15 :inc -1)
                 (:name b :width 16 :start 15 :inc -1))


;; generic bit shift operations
(defun mk-shift-left-body (bits a b)
  (let ((groups (list a b)))
    (unroll-groups groups
       `(cond ((>= idx ,bits) `(assert! (equalv ,,(group-var a) nil)))
              (t              `(assert! (equalv ,,(group-var a)
                                                ,,(group-var b))))))))

(defun mk-shift-right-body (bits a b)
  (let ((groups (list a b)))
    (unroll-groups groups
      `(cond ((< idx ,bits) `(assert! (equalv ,,(group-var a) nil)))
             (t             `(assert! (equalv ,,(group-var a)
                                              ,,(group-var b))))))))

;; a = b << bits for MSB-first vectors
(defmacro mk-shift-left (name bits a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-shifted (mod (- (group-start a) bits) w))
         (b-rotated (group-mod! (group-start! b b-shifted) w)))
    `(defun ,name ,(mapcan #'group-syms (list a b))
       ,@(eval (mk-shift-left-body (- w bits) a b-rotated)))))

;; a = b >> bits for MSB-first vectors
(defmacro mk-shift-right (name bits a b)
  (let* ((w (apply #'min (mapcar #'group-width (list a b))))
         (b-shifted (mod (+ (group-start a) bits) w))
         (b-rotated (group-mod! (group-start! b b-shifted) w)))
    `(defun ,name ,(mapcan #'group-syms (list a b))
       ,@(eval (mk-shift-right-body bits a b-rotated)))))

(mk-shift-left shl1-4
               1
               (:name y :width 4 :start 3 :inc -1)
               (:name x :width 4 :start 3 :inc -1))

(mk-shift-left shl1-8
               1
               (:name y :width 8 :start 7 :inc -1)
               (:name x :width 8 :start 7 :inc -1))

(mk-shift-right shr1-8
                1
                (:name y :width 8 :start 7 :inc -1)
                (:name x :width 8 :start 7 :inc -1))

(mk-shift-left shl7-16
               7
               (:name y :width 16 :start 15 :inc -1)
               (:name x :width 16 :start 15 :inc -1))

(mk-shift-right shr9-16
                9
                (:name y :width 16 :start 15 :inc -1)
                (:name x :width 16 :start 15 :inc -1))

;; test function generators

(defmacro mk-testcirc-body (fun arity)
  (let ((vars (mk-vars arity)))
    `(let ,(mapcar #'(lambda (v) (list v '(a-booleanv))) vars)
       (,fun ,@vars)
       (list ,@vars))))

(defmacro mk-testcirc (name fun arity)
  `(defun ,name ()
     (sort (mapcar #'vector->binstr
                   (all-values
                    (solution
                     (mk-testcirc-body ,fun ,arity)
                     (reorder #'domain-size
                              #'(lambda (x) (declare (ignore x)) nil)
                              #'<
                              #'linear-force))))
           #'string<)))

(mk-testcirc test-half-adder-f half-adder 4)
(mk-testcirc test-full-adder-f full-adder 5)

(deftest-fun test-half-adder '("0000" "0101" "1001" "1110"))
(deftest-fun test-full-adder '("00000" "00101" "01001" "01110"
                               "10001" "10110" "11010" "11111"))

;; compute what the output of an adder test function should be
(defun rc-adder-output (width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (dotimes (b n)
        (let* ((s (mod (+ a b) n))
               (str (strcat (value->binstr a width)
                            (value->binstr b width)
                            (value->binstr s width))))
          (setf out (cons str out)))))
    (nreverse out)))

;; compute what the output of a rotate test function should be
(defun rotl-output (bits width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (let* ((s (rotl a width bits))
             (str (strcat (value->binstr s width)
                          (value->binstr a width))))
          (setf out (cons str out))))
    (sort out #'string<)))

(defun rotr-output (bits width)
  (rotl-output (- bits) width))

;; compute what the output of a shift test function should be
(defun shl-output (bits width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (let* ((s (shl a width bits))
             (str (strcat (value->binstr s width)
                          (value->binstr a width))))
          (setf out (cons str out))))
    (sort out #'string<)))

(defun shr-output (bits width)
  (shl-output (- bits) width))


(mk-testcirc test-rc-adder3-f rc-adder-3 9)
(mk-testcirc test-rc-adder4-f rc-adder-4 12)
(mk-testcirc test-rc-adder5-f rc-adder-5 15)
(mk-testcirc test-rc-adder6-f rc-adder-6 18)
(mk-testcirc test-rc-adder7-f rc-adder-7 21)
(mk-testcirc test-rc-adder8-f rc-adder-8 24)

(deftest-fun test-rc-adder3 (rc-adder-output 3))
(deftest-fun test-rc-adder4 (rc-adder-output 4))
(deftest-fun test-rc-adder5 (rc-adder-output 5))
(deftest-fun test-rc-adder6 (rc-adder-output 6))
(deftest-fun test-rc-adder7 (rc-adder-output 7))
(deftest-fun test-rc-adder8 (rc-adder-output 8))

(mk-testcirc test-rotl1-8-f rotl1-8 16)
(mk-testcirc test-rotr1-8-f rotr1-8 16)
(mk-testcirc test-rotr9-16-f rotr9-16 32)

(deftest-fun test-rotl1-8 (rotl-output 1 8))
(deftest-fun test-rotr1-8 (rotr-output 1 8))
(deftest-fun test-rotr9-16 (rotr-output 9 16))


(mk-testcirc test-shl1-4-f shl1-4 8)
(mk-testcirc test-shl1-8-f shl1-8 16)
(mk-testcirc test-shr1-8-f shr1-8 16)
(mk-testcirc test-shl7-16-f shl7-16 32)
(mk-testcirc test-shr9-16-f shr9-16 32)

(deftest-fun test-shl1-4 (shl-output 1 4))
(deftest-fun test-shl1-8 (shl-output 1 8))
(deftest-fun test-shr1-8 (shr-output 1 8))
(deftest-fun test-shl7-16 (shl-output 7 16))
(deftest-fun test-shr9-16 (shr-output 9 16))


(deftest test-rc-adder ()
  (combine-results
   (test-rc-adder3)
   (test-rc-adder4)
   (test-rc-adder5)
   (test-rc-adder6)
   (test-rc-adder7)
   (test-rc-adder8)))

(deftest test-rotlr ()
  (combine-results
   (test-rotl1-8)
   (test-rotr1-8)
   (test-rotr9-16)))

(deftest test-shlr ()
  (combine-results
   (test-shl1-4)
   (test-shl1-8)
   (test-shr1-8)
   (test-shl7-16)
   (test-shr9-16)))

(deftest test-basic-circuits ()
  (combine-results
   (test-half-adder)
   (test-full-adder)
   (test-rc-adder)
   (test-rotlr)
   (test-shlr)))

(deftest test ()
  (combine-results
   (test-macros)
   (test-utils)
   (test-group)
   (test-logic-gates)
   (test-basic-circuits)))

(test)

;;;;;
;;
;; Now we need to build infrastructure to specify constraints on
;; vectors of boolean variables
;;  - allow them to be constrained as hex string "dead:beef:1234"
;;  - allow their results to be printed as hex string
;;  - need support for ROTR, SHR, + (integer addition) and friends
;;
;;;;;


;; mk-binding: bind a vector to certain bit values.
;;
;; Each bit can have three values: T, NIL and 'don't care'. Bits
;; marked as 'don't care' do not receive any binding.
;;
;; When specifying the binding value as hex or bin, separator
;; characters ':' or '-' may be arbitrarily added to make the string
;; easier to comprehend. These separators are skipped and ignored.
;;
;; In a binary binding, 'x' means 'don't care'.  In a hex binding, 'x'
;; means don't care for all the corresponding four bits.

(defun format-bindstr (bindstr)
  (remove #\- (remove #\: bindstr)))

(defun bindstr-hex-to-bin (hex)
  (with-output-to-string (s)
     (dolist (c (coerce (string-downcase hex) 'list))
       (cond ((eq c #\0) (princ "0000" s))
             ((eq c #\1) (princ "0001" s))
             ((eq c #\2) (princ "0010" s))
             ((eq c #\3) (princ "0011" s))
             ((eq c #\4) (princ "0100" s))
             ((eq c #\5) (princ "0101" s))
             ((eq c #\6) (princ "0110" s))
             ((eq c #\7) (princ "0111" s))
             ((eq c #\8) (princ "1000" s))
             ((eq c #\9) (princ "1001" s))
             ((eq c #\a) (princ "1010" s))
             ((eq c #\b) (princ "1011" s))
             ((eq c #\c) (princ "1100" s))
             ((eq c #\d) (princ "1101" s))
             ((eq c #\e) (princ "1110" s))
             ((eq c #\f) (princ "1111" s))
             (t          (princ "xxxx" s))))))

(defun mk-binding-body (a bindstr)
  (unroll-groups (list a)
    `(cond ((eq #\0 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) nil)))
           ((eq #\1 (char ,bindstr idx)) `(assert! (equalv ,,(group-name a) t)))
           (t nil))))

(defmacro mk-binding-bin (name a bindstr)
  (let ((bindctl (format-bindstr bindstr)))
    `(defun ,name ,(group-syms a)
       ,@(eval (mk-binding-body a bindctl)))))

(defmacro mk-binding-hex (name a bindstr)
  (let ((bindctl (bindstr-hex-to-bin (format-bindstr bindstr))))
    `(defun ,name ,(group-syms a)
       ,@(eval (mk-binding-body a bindctl)))))

;; example
(mac (mk-binding-bin my-bind-fun
                     (:name a :width 8 :start 7 :inc -1)
                     "01:01-x1:x1"))

(mac (mk-binding-hex my-bind-fun
                     (:name a :width 8 :start 7 :inc -1)
                     "A:D"))

;; should expand into:
(defun my-bind-fun (a-7 a-6 a-5 a-4 a-3 a-2 a-1 a-0)
  (assert! (equalv a-7 nil))
  (assert! (equalv a-6 t))
  (assert! (equalv a-5 nil))
  (assert! (equalv a-4 t))
  (assert! (equalv a-2 t))
  (assert! (equalv a-0 t)))



;; An example where we
;; - declare two groups,
;; - constrain them to a certain logical relation,
;; - bind one or both of them fully or partially,
;; - evaluate the results.

(defmacro mk-rc-adder-w/o-defun (a b s)
  (let* ((w (apply #'min (mapcar #'group-width (list a b s))))
         (c (gensym)))
    `(let ,(group-defs `(:name ,c :start 1 :width ,(1- w)))
       ,@(eval (mk-rc-adder-body w
                                 (group-name a)
                                 (group-name b)
                                 c
                                 (group-name s))))))

(mac (mk-rc-adder-w/o-defun
      (:name a :width 4 :start 3 :inc -1)
      (:name b :width 4 :start 3 :inc -1)
      (:name s :width 4 :start 3 :inc -1)))

(defmacro mk-binding-bin-w/o (a bindstr)
  (let ((bindctl (format-bindstr bindstr)))
    `(progn ,@(eval (mk-binding-body a bindctl)))))

(mac (mk-binding-bin-w/o
      (:name a :width 8 :start 7 :inc -1)
      "01:01-x1:x1"))

(defmacro mk-binding-hex-w/o (a bindstr)
  (let ((bindctl (bindstr-hex-to-bin (format-bindstr bindstr))))
    `(progn ,@(eval (mk-binding-body a bindctl)))))

(mac (mk-binding-hex-w/o
      (:name c :width 4 :start 3 :inc -1)
      "a"))

(defun example-body ()
  (let-groups ((:name a :width 4 :start 3 :inc -1)
               (:name b :width 4 :start 3 :inc -1)
               (:name s :width 4 :start 3 :inc -1))
     (mk-rc-adder-w/o-defun (:name a :width 4 :start 3 :inc -1)
                            (:name b :width 4 :start 3 :inc -1)
                            (:name s :width 4 :start 3 :inc -1))
     (mk-binding-bin-w/o (:name a :width 4 :start 3 :inc -1) "x1:01")
     (mk-binding-hex-w/o (:name b :width 4 :start 3 :inc -1) "A")
     (mk-binding-bin-w/o (:name s :width 4 :start 3 :inc -1) "xx:xx")

     ;; TODO this should be generated by a macro
     (list a-3 a-2 a-1 a-0
           b-3 b-2 b-1 b-0
           s-3 s-2 s-1 s-0)))

(defun example ()
  (sort (mapcar #'vector->binstr
                (all-values
                 (solution
                  (example-body)
                  (reorder #'domain-size
                           #'(lambda (x) (declare (ignore x)) nil)
                           #'<
                           #'linear-force))))
        #'string<))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-half-adder-generator ()
  (all-values
   (let ((a (a-boolean))
         (b (a-boolean))
         (c (a-boolean))
         (s (a-boolean)))
     (if (and (equalv s (xor2v a b))
              (equalv c (andv a b)))
         (vector->binstr (list a b s c))
       (fail)))))

(defun test-half-adder-constraint ()
  (mapcar #'vector->binstr
          (all-values
           (solution
            (let ((a (a-booleanv))
                  (b (a-booleanv))
                  (s (a-booleanv))
                  (c (a-booleanv)))
              (half-adder a b s c)
              (list a b s c))
            (reorder #'domain-size
                     #'(lambda (x) (declare (ignore x)) nil)
                     #'<
                     #'linear-force)))))
