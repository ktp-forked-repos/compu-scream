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
                `(:name ,sym :width ,arity :start ,(1- arity) :inc -1))
            symbols arity-list)))

(defmacro mk-testfun (name fun arity)
  (let ((group `(:name x :width ,arity :start ,(1- arity) :inc -1)))
    `(def-solver ,name
       all-values
       vector->binstr
       (,group)
       (assert! (,fun ,@(group-syms group))))))

(defmacro mk-testcirc (name fun arity)
  (let ((group `(:name x :width ,arity :start ,(1- arity) :inc -1)))
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

;; compute reference output for adder
(defun rc-adder-output (width)
  (let ((n (ash 1 width))
        (out nil))
    (dotimes (a n)
      (dotimes (b n)
        (let* ((s (add a b width))
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
      (let* ((s (rotl a width bits))
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
      (let* ((s (shl a width bits))
             (str (strcat (value->binstr s width :slice 0)
                          (value->binstr a width :slice 0)))
             (fmt-str (slice-string str 4 #\_)))
          (setf out (cons fmt-str out))))
    (sort out #'string<)))

(defun shr-output (bits width)
  (shl-output (- bits) width))
