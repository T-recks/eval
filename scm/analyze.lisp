#|
A syntax analyzing evaluator for Scheme.
|#

(defun eval-analyze (exp env)
  (funcall (analyze exp) env))

(defun analyze (exp)
  (cl:cond ((self-evaluating? exp)
            (analyze-self-evaluating exp))
           ((quoted? exp) (analyze-quoted exp))
           ((variable? exp) (analyze-variable exp))
           ((assignment? exp) (analyze-assignment exp))
           ((definition? exp) (analyze-definition exp))
           ((if? exp) (analyze-if exp))
           ((lambda? exp) (analyze-lambda exp))
           ((begin? exp) (analyze-sequence (begin-actions exp)))
           ((cond? exp) (analyze (cond->if exp)))
           ((application? exp) (analyze-application exp))
           (t
            (error "Unknown expression type -- ANALYZE ~S" exp))))

(defun analyze-self-evaluating (exp)
  (lambda (env) exp))

(defun analyze-quoted (exp)
  (cl:let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(defun analyze-variable (exp)
  (lambda (env) (lookup-variable-value exp env)))

(defun analyze-assignment (exp)
  (cl:let ((var (assignment-variable exp))
           (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (funcall vproc env) env)
      'ok)))

(defun analyze-definition (exp)
  (cl:let ((var (definition-variable exp))
           (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (funcall vproc env) env)
      'ok
      ;; var
      )))

(defun analyze-if (exp)
  (cl:let ((pproc (analyze (if-predicate exp)))
           (cproc (analyze (if-consequent exp)))
           (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (cl:if (true? (funcall pproc env))
             (funcall cproc env)
             (funcall aproc env)))))

(defun analyze-lambda (exp)
  (cl:let ((vars (lambda-parameters exp))
           (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(defun analyze-sequence (exps)
  (labels ((sequentially (proc1 proc2)
             (lambda (env) (funcall proc1 env) (funcall proc2 env)))
           (iter (first-proc rest-procs)
             (cl:if (null rest-procs)
                    first-proc
                    (iter (sequentially first-proc (car rest-procs))
                          (cdr rest-procs)))))
    (cl:let ((procs (mapcar #'analyze exps)))
      (cl:if (null procs)
             (error "Empty sequence -- ANALYZE")
             (iter (car procs) (cdr procs))))))

(defun analyze-application (exp)
  (cl:let ((fproc (analyze (operator exp)))
           (aprocs (mapcar #'analyze (operands exp))))
    (lambda (env)
      (execute-application (funcall fproc env)
                           (mapcar (lambda (aproc) (funcall aproc env))
                                   aprocs)))))

(defun execute-application (proc args)
  (cl:cond ((primitive-procedure? proc)
            (apply-primitive-procedure proc args))
           ((compound-procedure? proc)
            (funcall (procedure-body proc)
                     (extend-environment (procedure-parameters proc)
                                         args
                                         (procedure-environment proc))))
           (t (error "Unknown procedure type -- EXECUTE-APPLICATION ~S" proc))))
