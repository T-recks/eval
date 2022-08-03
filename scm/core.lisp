#|
Core features for any evaluator to use, e.g. primitive procedures, syntax definitions, and data context abstractions.

Presently no control context abstractions are explicitly defined, i.e. the interpreter inherits the control mechanisms of the host language (whatever CL implementation we compile it in).
|#

;;;;;;;;;;;;;
;; package ;;
;;;;;;;;;;;;;
(cl:defpackage :scheme
  (:nicknames :scm)
  (:export
   :eval
   :apply)
  (:import-from
   :cl
   :quote
   :&optional :&body :&rest :&key :in-package
   :defun :funcall :lambda :labels :defparameter :defmacro :defstruct :defconstant :defmethod :setf :defvar :error ; functions, variables, errors
   :t :nil :equalp :numberp :listp :consp :symbolp :stringp :eq :null ; truth, predicates
   :cons :car :cdr :cadr :caadr :cdadr :cddr :caddr :cdddr :cadddr ; cons cells
   :mapcar :progn
   :+ :* :- :/ := :<
   :make-hash-table :gethash
   ))

(in-package "SCHEME")

;;;;;;;;;;;;;;;;;;;;;
;; data structures ;;
;;;;;;;;;;;;;;;;;;;;;
;; begin data structures
(defconstant true 'true)
(defconstant false 'false)

(defun booleanp (exp)
  (cl:if (eq exp true)
         t
         (eq exp false)))

(defun truep (x)
  (cl:not (eq x false)))

(defun falsep (x)
  (eq x false))

(defstruct procedure
  parameters
  body
  environment)

(defun compound-procedure-p (p) (procedure-p p))

(defconstant +the-empty-environment+ nil)

(defstruct environment
  first-frame
  enclosing)

(defun make-frame (vars vals)
  (cl:let ((frame (make-hash-table)))
    (cl:labels ((iter (vrs vls)
                  (cl:cond ((null vrs) frame)
                           (t (add-binding-to-frame! (car vrs) (car vls) frame)
                              (iter (cdr vrs) (cdr vls))))))
      (iter vars vals))))

(defun add-binding-to-frame! (var val frame)
  (setf (gethash var frame) val))

(defun extend-environment (vars vals base-env)
  (cl:if (cl:= (cl:length vars) (cl:length vals))
         (make-environment :first-frame (make-frame vars vals)
                           :enclosing base-env)
         (cl:if (cl:< (cl:length vars) (cl:length vals))
                (error "Too many arguments supplied ~S ~S" vars vals)
                (error "Too few arguments supplied ~S ~S" vars vals))))

(defun lookup-variable-value (var env)
  (labels ((env-loop (env)
             (cl:if (eq env +the-empty-environment+)
                    (error "Unbound variable ~S" var)
                    (cl:let ((val (gethash var (environment-first-frame env))))
                      (cl:if val
                             val
                             (env-loop (environment-enclosing env)))))))
    (env-loop env)))

(defun set-variable-value! (var val env)
  (labels ((env-loop (env)
             (cl:if (eq env +the-empty-environment+)
                    (error "Unbound variable -- SET! ~S" var)
                    (cl:let ((old-val (gethash var (environment-first-frame env))))
                      (cl:if old-val
                             (setf (gethash var (environment-first-frame env)) val)
                             (env-loop (environment-enclosing env)))))))
    (env-loop env)))

(defun define-variable! (var val env)
  (add-binding-to-frame! var val (environment-first-frame env)))
;; end data structures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment and primitive setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin run-program
(defun setup-environment ()
  (cl:let ((initial-env
             (extend-environment (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 +the-empty-environment+)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(defstruct primitive
  implementation)

(defun cl-pred->scm-pred (pred)
  "Return a predicate procedure that is like PRED but returns TRUE instead of T and FALSE instead of NIL."
  (lambda (&rest args)
    (cl:if (cl:apply pred args)
           true
           false)))

(defparameter *primitive-predicate-names*
  '((= =)
    (< <)
    (pair? consp)
    (null? null)
    (eq? eq)
    (number? numberp))
  "In each name-pair, the first symbol is what the name should be in the Scheme evaluator and the second symbol is the name for the Common Lisp predicate to use.")

(defparameter *primitive-predicates*
  (mapcar (lambda (name-pair)
            (cl:list (car name-pair)
                     (cl-pred->scm-pred (cl:symbol-function (cadr name-pair)))))
          *primitive-predicate-names*))

(defvar *primitive-procedures*
  `((car ,#'car)
    (cdr ,#'cdr)
    (cons ,#'cons)
    (+ ,#'+)
    (- ,#'-)
    (* ,#'*)
    (/ ,#'/)
    ,@*primitive-predicates*))

(defun primitive-procedure-names ()
  (mapcar #'car *primitive-procedures*))

(defun primitive-procedure-objects ()
  (mapcar (lambda (proc) (make-primitive :implementation (cadr proc)))
          *primitive-procedures*))

(defun apply-primitive-procedure (proc args)
  (cl:apply (primitive-implementation proc) args))

(defvar *the-global-environment* (setup-environment))

(defun reset-global-environment! () (setf *the-global-environment* (setup-environment)))

(defun define-primitive! (var pval env)
  (setf (cadr (cl:assoc var *primitive-procedures*))
        pval)
  (define-variable! var (make-primitive :implementation pval) env))
;; end run-program

;;;;;;;;;;;;
;; syntax ;;
;;;;;;;;;;;;
;; begin syntax
(defun self-evaluating-p (exp)
  (cl:cond ((numberp exp) t)
           ((stringp exp) t)
           ((booleanp exp) t)
           (t nil)))

(defun variablep (exp) (symbolp exp))

(defun quotedp (exp)
  (tagged-list-p exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list-p (exp tag)
  (cl:if (consp exp)
         (eq (car exp) tag)
         nil))

(defun assignmentp (exp)
  (tagged-list-p exp 'set!))

(defun assignment-variable (exp) (cadr exp))

(defun assignment-value (exp) (caddr exp))

(defun definitionp (exp)
  (tagged-list-p exp 'define))

(defun definition-variable (exp)
  (cl:if (symbolp (cadr exp))
         (cadr exp)                     ; e.g. (define x ...)
         (caadr exp)                    ; e.g. (define (x ...) ...)
         ))

(defun definition-value (exp)
  (cl:if (symbolp (cadr exp))
         (caddr exp)
         (make-lambda (cdadr exp)       ; formal parameters
                      (cddr exp))       ; body
         ))

(defun lambdap (exp) (tagged-list-p exp 'lambda))

(defun lambda-parameters (exp) (cadr exp))

(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun ifp (exp) (tagged-list-p exp 'if))

(defun if-predicate (exp) (cadr exp))

(defun if-consequent (exp) (caddr exp))

(defun if-alternative (exp)
  (cl:if (cl:not (null (cdddr exp)))
         (cadddr exp)
         false))

(defun make-if (predicate consequent alternative)
  (cl:list 'if predicate consequent alternative))

(defun beginp (exp) (tagged-list-p exp 'begin))

(defun begin-actions (exp) (cdr exp))

(defun last-exp-p (seq) (null (cdr seq)))

(defun first-exp (seq) (car seq))

(defun rest-exps (seq) (cdr seq))

(defun sequence->exp (seq)
  (cl:cond ((null seq) seq)
           ((last-exp-p seq) (first-exp seq))
           (t (make-begin seq))))

(defun make-begin (seq) (cons 'begin seq))

(defun applicationp (exp) (consp exp))

(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

(defun no-operands-p (ops) (null ops))

(defun first-operand (ops) (car ops))

(defun rest-operands (ops) (cdr ops))
;; end syntax

;;;;;;;;;;;;;;;;;;;;;;;;;
;; derived expressions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin cond->if
(defun condp (exp) (tagged-list-p exp 'cond))

(defun cond-clauses (exp) (cdr exp))

(defun cond-else-clause-p (clause)
  (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause) (car clause))

(defun cond-actions (clause) (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (cl:if (null clauses)
         false                         ; no else clause
         (cl:let ((first (car clauses))
                  (rest (cdr clauses)))
           (cl:if (cond-else-clause-p first)
                  (cl:if (null rest)
                         (sequence->exp (cond-actions first))
                         (error "ELSE clause isn't last -- COND->IF ~S" clauses))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest))))))
;; end cond->if
