;; package definition
(cl:defpackage :scheme
  (:nicknames :scm)
  (:export
   :eval
   :apply)
  (:import-from
   :cl
   :quote
   :&optional :&body :&rest :&key :in-package
   :defun :funcall :lambda :labels :defparameter :defmacro :setf :defvar :error ; functions, variables, errors
   :t :nil :equalp :numberp :listp :consp :symbolp :stringp :eq :null ; truth, predicates
   :cons :car :cdr :cadr :caadr :cdadr :cddr :caddr :cdddr :cadddr ; cons cells
   :mapcar :progn
   :+ :* :- :/ :=
   ))

(in-package "SCHEME")
;; begin data structures
(defparameter true 'true)
(defparameter false 'false)

(defun boolean? (exp)
  (cl:if (eq exp true)
         t
         (eq exp false)))

(defun true? (x)
  (cl:not (eq x false)))

(defun false? (x)
  (eq x false))
;; begin syntax
(defun self-evaluating? (exp)
  (cl:cond ((numberp exp) t)
           ((stringp exp) t)
           ((boolean? exp) t)
           (t nil)))
(defun variable? (exp) (symbolp exp))
(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list? (exp tag)
  (cl:if (consp exp)
         (eq (car exp) tag)
         nil))
(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp) (cadr exp))

(defun assignment-value (exp) (caddr exp))
(defun definition? (exp)
  (tagged-list? exp 'define))

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
(defun lambda? (exp) (tagged-list? exp 'lambda))

(defun lambda-parameters (exp) (cadr exp))

(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))
(defun if? (exp) (tagged-list? exp 'if))

(defun if-predicate (exp) (cadr exp))

(defun if-consequent (exp) (caddr exp))

(defun if-alternative (exp)
  (cl:if (cl:not (null (cdddr exp)))
         (cadddr exp)
         false))

(defun make-if (predicate consequent alternative)
  (cl:list 'if predicate consequent alternative))
(defun begin? (exp) (tagged-list? exp 'begin))

(defun begin-actions (exp) (cdr exp))

(defun last-exp? (seq) (null (cdr seq)))

(defun first-exp (seq) (car seq))

(defun rest-exps (seq) (cdr seq))

(defun sequence->exp (seq)
  (cl:cond ((null seq) seq)
           ((last-exp? seq) (first-exp seq))
           (t (make-begin seq))))

(defun make-begin (seq) (cons 'begin seq))
(defun application? (exp) (consp exp))

(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

(defun no-operands? (ops) (null ops))

(defun first-operand (ops) (car ops))

(defun rest-operands (ops) (cdr ops))
;; end syntax
;; begin cond->if
(defun cond? (exp) (tagged-list? exp 'cond))

(defun cond-clauses (exp) (cdr exp))

(defun cond-else-clause? (clause)
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
           (cl:if (cond-else-clause? first)
                  (cl:if (null rest)
                         (sequence->exp (cond-actions first))
                         (error "ELSE clause isn't last -- COND->IF ~S" clauses))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest))))))
;; end cond->if
(defun make-procedure (parameters body env)
  (cl:list 'procedure parameters body env))

(defun compound-procedure? (p) (tagged-list? p 'procedure))

(defun procedure-parameters (p) (cadr p))

(defun procedure-body (p) (caddr p))

(defun procedure-environment (p) (cadddr p))
(defun enclosing-environment (env) (cdr env))

(defun first-frame (env) (car env))

(defparameter *the-empty-environment* nil)

(defun make-frame (variables values) (cons variables values))

(defun frame-variables (frame) (car frame))

(defun frame-values (frame) (cdr frame))

(defun add-binding-to-frame! (var val frame)
  (setf (car frame) (cons var (car frame))
        (cdr frame) (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
  (cl:if (cl:= (cl:length vars) (cl:length vals))
         (cons (make-frame vars vals) base-env)
         (cl:if (cl:< (cl:length vars) (cl:length vals))
                (error "Too many arguments supplied ~S ~S" vars vals)
                (error "Too few arguments supplied ~S ~S" vars vals))))

(defun lookup-variable-value (var env)
  (labels ((env-loop (env)
             (labels ((scan (vars vals)
                        (cl:cond ((null vars)
                                  (env-loop (enclosing-environment env)))
                                 ((eq var (car vars))
                                  (car vals))
                                 (t (scan (cdr vars) (cdr vals))))))
               (cl:if (eq env *the-empty-environment*)
                      (error "Unbound variable ~S" var)
                      (cl:let ((frame (first-frame env)))
                        (scan (frame-variables frame)
                              (frame-values frame)))))))
    (env-loop env)))

(defun set-variable-value! (var val env)
  (labels ((env-loop (env)
             (labels ((scan (vars vals)
                        (cl:cond ((null vars)
                                  (env-loop (enclosing-environment env)))
                                 ((eq var (car vars))
                                  (setf (car vals) val))
                                 (t (scan (cdr vars) (cdr vals))))))
               (cl:if (eq env *the-empty-environment*)
                      (error "Unbound variable -- SET! ~S" var)
                      (cl:let ((frame (first-frame env)))
                        (scan (frame-variables frame)
                              (frame-values frame)))))))
    (env-loop env)))

(defun define-variable! (var val env)
  (cl:let ((frame (first-frame env)))
    (labels ((scan (vars vals)
               (cl:cond ((null vars)
                         (add-binding-to-frame! var val frame))
                        ((eq var (car vars))
                         (setf (car vals) val))
                        (t (scan (cdr vars) (cdr vals))))))
      (scan (frame-variables frame)
            (frame-values frame)))))
;; end data structures
(defun eval (exp env)
  (cl:cond ((self-evaluating? exp) exp)
           ((variable? exp) (lookup-variable-value exp env))
           ((quoted? exp) (text-of-quotation exp))
           ((assignment? exp) (eval-assignment exp env))
           ((definition? exp) (eval-definition exp env))
           ((if? exp) (eval-if exp env))
           ((lambda? exp)
            (make-procedure (lambda-parameters exp)
                            (lambda-body exp)
                            env))
           ((begin? exp)
            (eval-sequence (begin-actions exp) env))
           ((cond? exp) (eval (cond->if exp) env))
           ((application? exp)
            (apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
           (t
            (error "Unknown expression type -- EVAL ~S" exp))))
(defun apply (procedure arguments)
  (cl:cond ((primitive-procedure? procedure)
            (apply-primitive-procedure procedure arguments))
           ((compound-procedure? procedure)
            (eval-sequence
             (procedure-body procedure)
             (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
           (t
            (error "Unknown procedure type -- APPLY ~S" procedure))))
(defun list-of-values (exps env)
  (cl:if (no-operands? exps)
         '()
         (cons (eval (first-operand exps) env)
               (list-of-values (rest-operands exps) env))))
(defun eval-if (exp env)
  (cl:if (true? (eval (if-predicate exp) env))
         (eval (if-consequent exp) env)
         (eval (if-alternative exp) env)))
(defun eval-sequence (exps env)
  (cl:cond ((last-exp? exps) (eval (first-exp exps) env))
           (t (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
    env)
  'ok)
(defun primitive-procedure? (proc) (tagged-list? proc 'primitive))

(defun primitive-implementation (proc) (cadr proc))

(defun cl-pred->scm-pred (pred)
  "Return a predicate procedure that is like PRED but returns TRUE instead of T and FALSE instead of NIL."
  (lambda (&rest args)
    (cl:if (cl:apply pred args)
           true
           false)))

(defparameter *primitive-predicate-names*
  '((= cl:=)
    (< cl:<)
    (pair? cl:consp)
    (null? cl:null)
    (eq? cl:eq)
    (number? cl:numberp)
    )
  "In each name-pair, the first symbol is what the name should be in the Scheme evaluator and the second symbol is the name for the Common Lisp predicate to use.")

(defparameter *primitive-predicates*
  (mapcar (lambda (name-pair)
            (cl:list (car name-pair)
                     (cl-pred->scm-pred (cl:symbol-function (cadr name-pair)))))
          *primitive-predicate-names*))

(defparameter *primitive-procedures*
  `((car ,#'car)
    (cdr ,#'cdr)
    (cons ,#'cons)
    (+ ,#'cl:+)
    (- ,#'cl:-)
    (* ,#'cl:*)
    (/ ,#'cl:/)
    ,@*primitive-predicates*
    ))

(defun primitive-procedure-names ()
  (mapcar #'car *primitive-procedures*))

(defun primitive-procedure-objects ()
  (mapcar (lambda (proc) (cl:list 'primitive (cadr proc)))
          *primitive-procedures*))

(defun apply-primitive-procedure (proc args)
  (cl:apply (primitive-implementation proc) args))
;; begin run-program
(defun setup-environment ()
  (cl:let ((initial-env
             (extend-environment (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 *the-empty-environment*)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(defvar *the-global-environment* (setup-environment))

(defun reset-global-environment! () (setf *the-global-environment* (setup-environment)))
(defparameter *input-prompt* "> ")
(defparameter *output-prompt* "")

(defun prompt-for-input (string)
  (cl:format t "~&~A" string))

(defun display-output-prompt (string)
    (cl:format t "~&~A" string))

(defun announce-output (string)
  (cl:format t "~&~A" string))

(defun newline ()
  (cl:format t "~&"))

(defun driver-loop ()
  (cl:loop
    (prompt-for-input *input-prompt*)
    (cl:let ((input (cl:read)))
      (cl:if (equalp input '(quit))
             'goodbye
             (user-print (eval input *the-global-environment*))))))

(defun user-print (object)
  (cl:if (compound-procedure? object)
         (cl:format t "~A" (cl:list 'compound-procedure
                                     (procedure-parameters object)
                                     (procedure-body object)
                                     '<procedure-env>))
         (cl:format t "~A" object)))
;; end run-program
