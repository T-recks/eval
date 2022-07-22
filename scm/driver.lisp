#|
A repl for Scheme.
|#

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

(defun driver-loop (&key (evaluator #'eval) (env *the-global-environment*))
  (cl:loop
   (prompt-for-input *input-prompt*)
   (cl:let ((input (cl:read)))
     (cl:if (equalp input '(quit))
            (cl:return 'goodbye)
            (user-print (funcall evaluator input env))))))

(defun user-print (object)
  (cl:if (compound-procedure? object)
         (cl:format t "~A" (cl:list 'compound-procedure
                                     (procedure-parameters object)
                                     (procedure-body object)
                                     '<procedure-env>))
         (cl:format t "~A" object)))
