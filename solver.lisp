(require :screamer)
(in-package :screamer-user)

;; def-solver
;;
;; define a function to solve a constraint problem on given groups
;; constrained by given constraint-forms
(defun def-solver-inner (solver-extent groups constraints)
  `(,solver-extent
    (solution
     (let-groups ,groups
        ,@constraints
        (list-groups ,@groups))
     (reorder #'domain-size
              #'(lambda (x) (declare (ignore x)) nil)
              #'<
              #'linear-force))))

(defmacro def-solver (name solver-extent
                           groups
                           &body constraints)
  (let ((solver-inner (def-solver-inner solver-extent groups constraints)))
    `(defun ,name ()
       (sort
        ,(if (eq 'all-values solver-extent)
             `(mapcar #'vector->binstr ,solver-inner)
             `(list (vector->binstr ,solver-inner)))
       #'string<))))
