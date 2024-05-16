;;; File: lisp.lisp
;;; Description: This program uses Common Lisp to implement a LISP-1.5-like language (henceforth "LISP").

#|
"...Lisp in itself. These were “Maxwell’s Equations of Software!” This is the whole world of programming in a few lines that I can put my hand over."

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

;; "Evalquote is available to the programmer as a LISP function -- thus, one may now write
;; "(EVALQUOTE APPEND ((A) (B C D)))" rather than
;; "(EVAL (QUOTE (APPEND (A) (B C D))) NIL)" should one desire so." (96).
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
           ;; Note: this effectively completes the definition of APPLY. The only thing left is handling of LABEL forms.
           ;; From here, skip to the definition of EVAL to continue understanding LISP. Or read on if you dare...
           ;;
           ;; LABEL is useless in a practical LISP programming system but included below for completeness.
           ;; It is essentially a holdover from McCarthy's original 1960 paper defining LISP 1.0, but
           ;; all sources on LISP 1.5 state that in practice it was not used in user programs (TODO: insert sources here).
           ;; This was because (in the author's opinion) LABEL is redundant with the addition of mutable environments in LISP 1.5,
           ;; denoted by the parameter A in this implementation and in the Manual.
           ;; Environments in turn are made more usable in LISP 1.5 by the DEFINE "pseudo-function"
           ;; (see the end of this file for more commentary on DEFINE).
           ;; 
           ;; Furthermore, LABEL is redundant (in the author's opinion) with Scheme's LET, which is not present in LISP 1.5.
           ;;
           ;; LISP 1.0 included no mutation, so environments were treated only as expressions, not mutable variables.
           ;; Thus, LABEL was useful for constructing new environment expressions from other environment expressions
           ;; in a purely functional, expression-oriented style.
           ;;
           ;; The original 1960 LISP was a purely functional language in the modern sense, but as LISP grew in
           ;; adoption and relevance it rapidly transformed into a mutable, multi-paradigm language, as we can start to see already
           ;; in the 1.5 version.
           ;;
           ;; This difference between the 1.0 and 1.5 versions of LISP highlights a tension between the two main camps in early language design.
           ;; The former took its primary inspiration from LISP 1.0 and the Lambda Calculus, focusing on principled, purely functional
           ;; formalisms while 
           ;; the latter took its primary inspiration from LISP 1.5, focusing on practical, human-friendly programming systems.
           ;; Each camp extended various olive branches to the other.
           ;; Notice the stereotypical LISP binary tree style of this historical development ;)
           ;; 
           ;; These two camps constitute the main axis of early programming language design.
           ;; The first branch in this tree brought us Scheme, ML, Miranda, and Haskell, which embodied more sophisticated versions of LISP 1.0
           ;; purely functional style, while attempting to meet the other camp (i.e. the practical programmer) in the middle with principled and
           ;; constrained approaches to mutability, culminating in Monadic IO.
           ;; 
           ;; The second brought us Scheme, Common LISP, Smalltalk, and then CLOS.
           ;; TODO: elaborate on what these languages contributed and how they embody the second leaf... I'm too tired to do it right now.
           ;;
           ;; Yes, Scheme is in both camps. Much like LISP itself.
           ;;
           ;; Almost everything interesting about programming language design in the 20th century can be easily understood as
           ;; a precursor to LISP or a reaction to LISP, as various programming researchers sought to answer either the question
           ;; "How can we do what LISP 1.0 did but better?" or "How can we do what LISP 1.5 did but better?"
           ;; Where "better" equals more parsimoniously, in a more principled fashion, more practically, providing more expressive
           ;; power to the programmer, more efficiently, etc. etc...
           ;;
           ;; Here ends the author's diatribe on the profundity of the symbol known as LABEL and its obsolescence in LISP.
           ;; Here begins the evaluator's definition of LABEL, the elder fossil of LISP formalisms.
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
  (cl:cond ((null m) nil)
           (t (cl:cons (eval (cl:car m) a)
                       (eval-list (cl:cdr m) a)))))

#|
Q: Is not Cons a compound data type constructed from Symbols, rather than a primitive data type?
A: I'd argue that while the Cons type could be implemented as a compound data type, in LISP 1.5 it is treated as primitive (so we treat it as such in LISP).
Indeed, the original LISP 1.5 implementation for the PDP-10 used the underlying machine code equivalents of CAR and CDR as primitive in order to implement Cons types.
There's at least one SICP exercise on this topic (e.g. defining CONS, CAR, and CDR as higher order functions)... I'll reference it here later once I bother to find it.

Q: Is not DEFUN a special form? It looks like implementing LISP requires two special forms, not one.
A: Sort of. DEFUN is used rather like DEFINE would be in LISP 1.5. In the standard LISP 1.5 implementation, there was a default global environment and also a "pseudo-function" DEFINE
that added definitions to the global environment. Environments, as you can also see in this LISP implementation, were merely association "lists", constructed by CONS and deconstructed by CAR and CDR.
Thus, environments and definitions in LISP 1.5 were simple abstractions implemented on top of the CONS, CAR, and CDR primitives, and DEFINE is syntax sugar for manipulating the global/default environment.
You can think of DEFUN in this LISP implementation in a similar light.
|#
