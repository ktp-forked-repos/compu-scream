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
                           vector-formatter
                           groups
                           &body constraints)
  (let* ((groups (if (symbolp (car groups)) (eval groups) groups))
         (solver-inner (def-solver-inner solver-extent groups constraints)))
    `(defun ,name ()
       (sort
        ,(if (eq 'all-values solver-extent)
             `(mapcar #',vector-formatter ,solver-inner)
             `(list (,vector-formatter ,solver-inner)))
       #'string<))))
