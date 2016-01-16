(require :screamer)
(in-package :screamer-user)


(all-values
 (let ((a (a-boolean))
       (b (a-boolean)))
   (if (andv a b)
       (list a b)
       (fail))))


(all-values
 (let ((x (either 1 2 3 4 5 6 7 8 9 10))
       (y (either 1 2 3 4 5 6 7 8 9 10)))
   (if (= 0 (logxor x y))
       (list x y)
       (fail))))


(defun logvals ()
  (let ((a (an-integer-betweenv 0 255))
        (b (an-integer-betweenv 0 255)))
    (assert! (=v 128 (+v a b))) ;; +v should be logandv / logxorv / etc.
    (all-values                 ;; but those are not (yet) implemented.
     (solution
      (list a b)
      (reorder #'domain-size
               #'(lambda (x) (declare (ignore x)) nil)
               #'<
               #'linear-force)))))


(defun triples (n)
  (let ((a (an-integer-betweenv 1 n))
        (b (an-integer-betweenv 1 n))
        (c (an-integer-betweenv 1 n)))
    (format t "assertions start~%")
    (assert! (=v (+v (*v a a) (*v b b)) (*v c c)))
    (format t "assertions finish~%")
    (all-values
     (solution
      (list a b c)
      (reorder #'range-size
               #'(lambda (x) (< x 1e-6))
               #'>
               #'divide-and-conquer-force)))))

(time (triples 500))
