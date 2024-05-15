;;; File: programs.lisp
;;; Description: examples LISP programs

(in-package "LISP")

;; Example 1
;; Demonstrates: variable definition
;; => BAR
(eval 'FOO
      '((FOO . BAR)))

;; Example 2
;; Demonstrates: function definition,
;; list syntax
;; => FIRST
(eval '(head (quote (first second)))
      '((head . (lambda (c) (car c)))))

;; Example 3a
;; Demonstrates: recursive function definition,
;; recursive symbol definition,
;; => (A B C D)
(eval '(append (quote (A)) (quote (B C D)))
      '((append . (lambda (x y)
                    (cond ((eq x nil) y)
                          ((eq (quote t) (quote t))
                           (cons (car x)
                                 (append (cdr x) y))))))))

;; Example 3b
;; Demonstrates: definition of T
;; => (A B C D)
(eval '(append (quote (A)) (quote (B C D)))
      '((t . t)                   ; T defined self-referentially here.
                                        ; It could also be handled specially by EVAL when it identifies
                                        ; a COND form, but that would be stylistically inferior;
                                        ; especially since T is effectively syntactic sugar for any
                                        ; tautologically true expression, as evident from  Example 3a.
        (append . (lambda (x y)
                    (cond ((eq x nil) y)
                          (t (cons (car x) ; APPEND definition abbreviated by levereging T
                                   (append (cdr x) y))))))))

;; TODO: LISP definition of aux functions NULL and EQUAL
;; TODO: LISP recursive definition of MEMBER using NULL and EQUAL
;; TODO: LISP definition of a new programming language
