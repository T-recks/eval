;;; File: lisp.lisp
;;; Description: This program uses Common Lisp to implement a LISP-1.5-like language (henceforth "LISP").

#|
"...Lisp in itself. These were “Maxwell’s Equations of Software!” This is the whole world of programming in a few lines that I can put my hand over." - Kay

On page 13 of the LISP 1.5 Programmer's Manual, McCarthy implements a complete programming language using only two primitive data types, five primitive functions, and one special form. The two primitive data types are the Symbol and the Cons. The five primitive functions operating on these data types are CAR, CDR, CONS, ATOM, and EQ. The primitive form is COND.

Wherever the six functions/forms are used to implement LISP, they are prefixed with the CL package to emphasize their status as imported primitives. For readability, instances of primitive data types--such as T, NIL, '(), or 'FOO--are conventionally not prefixed, either in the implementation of LISP or in LISP example programs.
|#

(cl:defpackage :lisp
  (:export
   :eval
   :evalquote)
  (:import-from
   :cl
   :in-package
   :defun :defmacro
   :t :nil))

(in-package "LISP")

(defun eval (e a)
  (cl:cond ((cl:atom e)
            (cl:cdr (assoc e a)))
           ((cl:atom (cl:car e))
            (cl:cond ((cl:eq (cl:car e) 'quote)
                      (cl:cadr e))
                     ((cl:eq (cl:car e) 'cond)
                      (eval-cond (cl:cdr e) a))
                     (t (apply (cl:car e) (eval-list (cl:cdr e) a) a))))
           (t (apply (cl:car e) (eval-list (cl:cdr e) a) a))))

(defun eval-cond (c a)
  (cl:cond ((eval (cl:caar c) a)
            (eval (cl:cadar c) a))
           (t (eval-cond (cl:cdr c) a))))

(defun eval-list (m a)
  (cl:cond ((null m) nil)
           (t (cl:cons (eval (cl:car m) a)
                       (eval-list (cl:cdr m) a)))))

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

#|
Q: Is not Cons a compound data type constructed from Symbols, rather than a primitive data type?
A: I'd argue that while the Cons type could be implemented as a compound data type, in LISP 1.5 it is treated as primitive (so we treat it as such in LISP).
Indeed, the original LISP 1.5 implementation for the PDP-10 used the underlying machine code equivalents of CAR and CDR as primitive in order to implement Cons types.
There's at least one SICP exercise on this topic (e.g. defining CONS, CAR, and CDR as higher order functions)... I'll reference it here later once I bother to find it.

Q: Is not DEFUN a special form? It looks like implementing LISP requires two special forms, not one.
A: Sort of. DEFUN is used rather like DEFINE would be in LISP 1.5. In the standard LISP 1.5 implementation, there was a default global environment and also a "pseudo-function" DEFINE that added definitions to the global environment. Functions could be DEFINE'd by associating a symbol with a lambda in the global environment. Environments, as you can also see in this LISP implementation, were merely association "lists", constructed by CONS and deconstructed by CAR and CDR.

Thus, environments and definitions in LISP 1.5 were simple abstractions implemented on top of the CONS, CAR, and CDR primitives, and DEFINE is syntax sugar for manipulating the global/default environment. You can think of DEFUN in this LISP implementation in a similar light. A complete model of LISP could show how to extend the evaluator to support something equivalent to DEFUN, but this would obscure somewhat the important features of the model.

In sum, DEFUN is a convenience making LISP nicer to implement and use in the CL programming environment, but not significant or essential enough to model in LISP:EVAL. This point is demonstrated by the implementation of LISP2 in the file lisp.lisp.lisp, which uses only an environment expression and no special forms from LISP (although it does store this expression in a CL variable for convenient use).
|#
