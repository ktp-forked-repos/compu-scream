(require :screamer)
(in-package :screamer)

;; extensions to allow gate-style logic computations

(defun xorv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (let* ((xs (remove nil xs :test #'eq))
           (count-nonnil (length xs))
           (xs (remove t xs :test #'eq))
           (n-unc (length xs))
           (n-true (- count-nonnil n-unc)))
      (cond
       ((zerop count-nonnil) nil) ; all inputs were nil -- return nil
       ((zerop n-unc) (oddp n-true)) ; no unbound input -- return parity of inputs
       ((and (= n-unc 1) (oddp n-true)) (notv (first xs))) ; one unbound input -- gives the output
       ((= n-unc 1) (first xs))
       (t (let* ((z (a-booleanv))
                 (xs (remove t xs :test #'eq))
                 (args (cons z xs)))
            ;; attach noticer to all unconstrained vars, including z
            (dolist (a args)
              (attach-noticer!-internal
               #'(lambda ()
                   (cond ((variable-true? a)
                          (local (decf n-unc)
                                 (incf n-true)
                                 (remove a args :test #'eq)))
                         ((variable-false? a)
                          (local (decf n-unc)
                                 (remove a args :test #'eq))))
                   ;; if only two unconstrained vars remain, setup constraint between them
                   (cond ((and (= 2 n-unc) (oddp n-true))
                          (assert! (=v ((notv (first args)) (second args)))))
                         ((= 2 n-unc)
                          (assert! (=v (first args) (second args))))
                         ;; if only one unconstrained var remains, determine its value
                         ((and (= 1 n-unc) (oddp n-true))
                          (restrict-true! (first args)))
                         ((= 1 n-unc)
                          (restrict-false! (first args)))))
               a))))))))

(defun xorv (&rest xs)
  "Restricts each argument to be boolean.

Returns NIL if called with no arguments. If all arguments are known,
returns T if an odd number of them is T, NIL if an even number of them
is T.

Otherwise returns a boolean variable V. The values of all
unconstrained arguments and V are mutually constrained so that
together they will contain an even number of T values.

 * The number of unconstrained arguments is maintained as n-unc.
 * The number of constrained to T arguments is maintained as n-true.

 * If V or any argument later becomes constrained to
   -   T: n-unc is decremented and n-true incremented;
   - NIL: n-unc is decremented.

   If n_unc is now

   - 2: the two remaining unconstrained arguments (or V) are
        constrained to be opposites (if n-true is odd) or the same (if
        n-true is even).

   - 1: the single remaining argument is constrained to T (if n-true
     is odd) or NIL (if n-true is even).

Note that unlike CL:XOR, XORV is a function and always evaluates all its
arguments. Secondly, any non-boolean argument causes it to fail."
  (xorv-internal xs))

(defun assert!-xorv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (member t xs :test #'eq)
      (let* ((xs (remove nil xs :test #'eq))
             (count (length xs)))
        (cond ((zerop count) (fail))
              ((= count 1) (restrict-true! (first xs)))
              (t (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-true? x))
                                ((variable-false? x)
                                 (local (decf count))
                                 (cond ((zerop count) (fail))
                                       ((= count 1)
                                        (dolist (x xs)
                                          (unless (variable-false? x)
                                            (restrict-true! x))))))))
                      x)))))))))

(defun assert!-xorv (&rest xs) (assert!-xorv-internal xs))
