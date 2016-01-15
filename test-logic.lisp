(require :screamer)
(in-package :screamer-user)

;; test function generators

(defun mock-group-symbols (n)
  (let ((symbols nil))
    (dotimes (i n)
      (let* ((char (code-char (+ (char-code #\A) i)))
             (str (coerce (list char) 'string)))
        (setf symbols (cons (intern str) symbols))))
    (nreverse symbols)))

(defun mock-group-list (arity-list)
  (let ((symbols (mock-group-symbols (length arity-list))))
    (mapcar #'(lambda (sym arity)
                (car (def-groups (list sym arity))))
            symbols arity-list)))

(defmacro mk-testfun (name fun arity)
  (let ((group (car (def-groups (list 'x arity)))))
    `(def-solver ,name
       all-values
       vector->binstr
       (,group)
       (assert! (,fun ,@(group-syms group))))))

(defmacro mk-testcirc (name fun arity)
  (let ((group (car (def-groups (list 'x arity)))))
    `(def-solver ,name
       all-values
       vector->binstr
       (,group)
       (,fun ,@(group-syms group)))))

(defmacro mk-testcirc/groups (name fun args &rest arity-list)
  (let ((groups (mock-group-list arity-list)))
    `(def-solver ,name
       all-values
       vector->binstr
       ,groups
       (,fun ,@args ,@groups))))

;; partially test a = fun(b) on 32 bits width, allowing only the
;; bottom-most free-bits of b to change; thus generating the
;; corresponding results of applying fun to 0 .. 2^free-bits - 1
(defmacro partial-test-gen (fun free-bits a b)
  (let ((bindstr (strcat (make-array (- 32 free-bits)
                                     :element-type 'character
                                     :initial-element #\0)
                         (make-array free-bits
                                     :element-type 'character
                                     :initial-element #\x))))
    `(progn (,fun ,a ,b)
            (binding-bin ,b ,bindstr))))

;; compute reference output for adder
(defun rc-adder-output (width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (dotimes (b n)
        (let* ((s (arith-add a b width))
               (str (strcat (value->binstr a width :slice 0)
                            (value->binstr b width :slice 0)
                            (value->binstr s width :slice 0)))
               (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out)))))
    (nreverse out)))

;; compute reference output for bit rotate
(defun rotl-output (bits width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-rotl a width bits))
             (str (strcat (value->binstr s width :slice 0)
                          (value->binstr a width :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

(defun rotr-output (bits width)
  (rotl-output (- bits) width))

;; compute reference output for bit shift
(defun shl-output (bits width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-shl a width bits))
             (str (strcat (value->binstr s width :slice 0)
                          (value->binstr a width :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

(defun shr-output (bits width)
  (shl-output (- bits) width))

;; compute reference output for bsig0
(defun bsig0-output (free-bits)
  (let ((n (ash 1 free-bits))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-bsig0 a))
             (str (strcat (value->binstr s 32 :slice 0)
                          (value->binstr a 32 :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

;; compute reference output for bsig1
(defun bsig1-output (free-bits)
  (let ((n (ash 1 free-bits))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-bsig1 a))
             (str (strcat (value->binstr s 32 :slice 0)
                          (value->binstr a 32 :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

;; compute reference output for ssig0
(defun ssig0-output (free-bits)
  (let ((n (ash 1 free-bits))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-ssig0 a))
             (str (strcat (value->binstr s 32 :slice 0)
                          (value->binstr a 32 :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

;; compute reference output for ssig1
(defun ssig1-output (free-bits)
  (let ((n (ash 1 free-bits))
        (out nil))
    (dotimes (a n)
      (let* ((s (arith-ssig1 a))
             (str (strcat (value->binstr s 32 :slice 0)
                          (value->binstr a 32 :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))
