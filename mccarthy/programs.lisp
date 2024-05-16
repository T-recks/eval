;;; File: programs.lisp
;;; Description: example LISP and LISP2 programs.

(in-package "LISP")

;;;;;;;;;;
;; LISP ;;
;;;;;;;;;;

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
;; Demonstrates: recursive function definition
;; => (A B C D)
(eval '(append (quote (A)) (quote (B C D)))
      '((append . (lambda (x y)
                    (cond ((eq x nil) y)
                          ((eq (quote a) (quote a))
                           (cons (car x)
                                 (append (cdr x) y))))))))

;; Example 3b
;; Demonstrates: recursive symbol definition,
;; definition of T
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

;; TODO: LISP recursive definition of MEMBER using NULL and EQUAL

;;;;;;;;;;;
;; LISP2 ;;
;;;;;;;;;;;

(cl:let ((env (cl:with-open-file (f "lisp.lisp.lisp")
                (cl:read f))))
  (cl:values
   (lisp:eval '(eval
                (quote foo)
                (quote ((foo . bar))))
              env) ; => BAR
   (lisp:eval '(eval
                (quote (dot (quote a) (quote b)))
                nil)
              env) ; => (A . B)
   (lisp:eval '(eval
                (quote (head (dot (quote a) (quote b))))
                nil)
              env) ; => A
   (lisp:eval '(eval
                (quote (tail (dot (quote a) (quote b))))
                nil)
              env) ; => B
   (lisp:eval '(eval
                (quote (= (quote b) (tail (dot (quote a) (quote b)))))
                nil)
              env) ; => T
   (lisp:eval '(eval
                (quote (symbol? (tail (dot (quote a) (quote b)))))
                nil)
              env) ; => T
   (lisp:eval '(eval
                (quote ((lambda (x y) (dot x y)) (quote a) (quote b)))
                nil)
              env)
   ))

