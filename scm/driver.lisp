#|
A repl for Scheme.
|#

(defparameter *input-prompt* "> ")

(defun prompt-for-input (string)
  (cl:format t "~&~A" string))

(defun display-output-prompt (string)
    (cl:format t "~&~A" string))

(defun announce-output (string)
  (cl:format t "~&~A" string))

(defun newline ()
  (cl:format t "~&"))

(defparameter *default-evaluator* #'eval)

(define-primitive! 'eval *default-evaluator* *the-global-environment*)

(defun driver-loop (&key (evaluator +default-evaluator+) (env *the-global-environment*))
  (cl:let ((active-eval (lookup-variable-value 'eval env)))
    (cl:if (cl:not (eq evaluator active-eval))
           (error "Environment error: evaluator passed to driver-loop not compatible with existing definitions")
           (cl:loop
            (prompt-for-input *input-prompt*)
            (cl:let ((input (cl:read)))
              (cl:if (equalp input '(quit))
                     (cl:return 'goodbye)
                     (print (funcall evaluator input env))))))))

(defmethod print (object)
  (cl:format t "~A" object))

(defmethod print ((object procedure))
  (cl:format t "~A" (cl:list 'compound-procedure
                             (procedure-parameters object)
                             (procedure-body object)
                             '<procedure-env>)))
