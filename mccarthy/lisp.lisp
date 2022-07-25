#|
"...Lisp in itself. These were “Maxwell’s Equations of Software!” This is the whole world of programming in a few lines that I can put my hand over."
|#

(cl:defpackage :lisp
  (:export
   :evalquote)
  (:import-from
   :cl
   :in-package
   :defun :defmacro
   :t :nil))

(in-package "LISP")

(defmacro evalquote (fn x)
  `(apply ',fn ',x nil))

(defun apply (fn x a)
  (cl:cond ((cl:atom fn)
            (cl:cond ((cl:eq fn 'car) (cl:caar x))
                     ((cl:eq fn 'cdr) (cl:cdar x))
                     ((cl:eq fn 'cons) (cl:cons (cl:car x) (cl:cadr x)))
                     ((cl:eq fn 'atom) (cl:atom (cl:car x)))
                     ((cl:eq fn 'eq) (cl:eq (cl:car x) (cl:cadr x)))
                     (t (apply (eval fn a) x a))))
           ((cl:eq (cl:car fn) 'lambda)
            (eval (cl:caddr fn) (pairlist (cl:cadr fn) x a)))
           ((cl:eq (cl:car fn) 'label)
            (apply (cl:caddr fn) x (cl:cons (cl:cons (cl:cadr fn)
                                                     (cl:caddr fn))
                                            a)))))

(defun eval (e a)
  (cl:cond ((cl:atom e)
            (cl:cdr (assoc e a)))
           ((cl:atom (cl:car e))
            (cl:cond ((cl:eq (cl:car e) 'quote)
                      (cl:cadr e))
                     ((cl:eq (cl:car e) 'cond)
                      (eval-cons (cl:cdr e) a))
                     (t (apply (cl:car e) (eval-list (cl:cdr e) a) a))))
           (t (apply (cl:car e) (eval-list (cl:cdr e) a) a))))

(defun eval-cons (c a)
  (cl:cond ((eval (cl:caar c) a)
            (eval (cl:cadar c) a))
           (t (eval-cons (cl:cdr c) a))))

(defun eval-list (m a)
  (cl:cond ((cl:null m) nil)
           (t (cl:cons (eval (cl:car m) a)
                       (eval-list (cl:cdr m) a)))))
