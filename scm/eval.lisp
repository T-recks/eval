#|
A basic Scheme evaluator.
|#
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
