(defun append (x y)
  (cl:cond ((cl:null x) y)
           (t (cl:cons (cl:car x)
                       (append (cl:cdr x) y)))))

(defun member (x y)
  (cl:cond ((cl:null y) nil)
           ((equal x (cl:car y)) t)
           (t (member x (cl:cdr y)))))
