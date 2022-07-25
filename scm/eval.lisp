#|
A basic Scheme evaluator.
|#

(defun eval (exp &optional (env *the-global-environment*))
  (cl:cond ((self-evaluating-p exp) exp)
           ((variablep exp) (lookup-variable-value exp env))
           ((quotedp exp) (text-of-quotation exp))
           ((assignmentp exp) (eval-assignment exp env))
           ((definitionp exp) (eval-definition exp env))
           ((ifp exp) (eval-if exp env))
           ((lambdap exp)
            (make-procedure :parameters (lambda-parameters exp)
                            :body (lambda-body exp)
                            :environment env))
           ((beginp exp) (eval-sequence (begin-actions exp) env))
           ((condp exp) (eval (cond->if exp) env))
           ((applicationp exp)
            (apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
           (t
            (error "Unknown expression type -- EVAL ~S" exp))))

(defun apply (procedure arguments)
  (cl:cond ((primitive-p procedure)
            (apply-primitive-procedure procedure arguments))
           ((compound-procedure-p procedure)
            (eval-sequence
             (procedure-body procedure)
             (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
           (t
            (error "Unknown procedure type -- APPLY ~S" procedure))))

(defun list-of-values (exps env)
  (cl:if (no-operands-p exps)
         '()
         (cons (eval (first-operand exps) env)
               (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (cl:if (truep (eval (if-predicate exp) env))
         (eval (if-consequent exp) env)
         (eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cl:cond ((last-exp-p exps) (eval (first-exp exps) env))
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
