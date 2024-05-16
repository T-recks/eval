;;; File: lisp.lisp.lisp
;;; Description: This expression defines LISP2 (pronounced "LISP squared").
;;; It can be passed as an environment to LISP:EVAL in order to interperet
;;; LISP2 programs within LISP. As such, it effectively constitutes a LISP
;;; program implementing the LISP2 language (a language rather similar to LISP).

((null . (lambda (x) (eq x nil)))
 (equal . (lambda (x y)
            (cond ((atom x)
                   (cond ((atom y)
                          (eq x y))
                         (t nil)))
                  ((equal (car x) (car y))
                   (equal (cdr x) (cdr y)))
                  (t nil))))
 (pairlist . (lambda (x y a)
               (cond ((null x) a)
                     (t (cons (cons (car x) (car y))
                              (pairlist (cdr x) (cdr y) a))))))
 (assoc . (lambda (x a)
            (cond ((equal (car (car a)) x)
                   (car a))
                  (t (assoc x (cdr a))))))
 (eval . (lambda (e a)
           (cond ((atom e)
                  (cdr (assoc e a)))
                 ((atom (car e))
                  (cond ((eq (car e) (quote quote))
                         (car (cdr e)))
                        ((eq (car e) (quote cond))
                         (eval-cons (cdr e) a))
                        (t (apply (car e) (eval-list (cdr e) a) a))))
                 (t (apply (car e) (eval-list (cdr e) a) a)))))
 (eval-cond . (lambda (c a)
                (cond ((eval (car (car c)) a)
                       (eval (car (cdr (car c))) a))
                      (t (eval-cond (cdr c) a)))))
 (eval-list . (lambda (m a)
                (cond ((null m) nil)
                      (t (cons (eval (car m) a)
                               (eval-list (cdr m) a))))))
 (apply . (lambda (fn x a)
            (cond ((atom fn)
                   (cond ((eq fn (quote head)) (car (car x)))
                         ((eq fn (quote tail)) (cdr (car x)))
                         ((eq fn (quote dot)) (cons (car x) (car (cdr x))))
                         ((eq fn (quote symbol?)) (atom (car x)))
                         ((eq fn (quote =)) (eq (car x) (car (cdr x))))
                         (t (apply (eval fn a) x a))))
                  ((eq (car fn) (quote lambda))
                   (eval (car (cdr (cdr fn))) (pairlist (car (cdr fn)) x a)))
                  ((eq (car fn) (quote label))
                   (apply (car (cdr (cdr fn))) x (cons (cons (car (cdr fn))
                                                             (car (cdr (cdr fn))))
                                                       a))))))
 (t . t))
