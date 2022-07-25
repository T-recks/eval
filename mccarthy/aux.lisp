(defun equal (x y)
  (cl:cond ((cl:atom x)
            (cl:cond ((cl:atom y)
                      (cl:eq x y))
                     (t nil)))
           ((equal (cl:car x) (cl:car y))
            (equal (cl:cdr x) (cl:cdr y)))
           (t nil)))

(defun pairlist (x y a)
  (cl:cond ((cl:null x) a)
           (t (cl:cons (cl:cons (cl:car x) (cl:car y))
                       (pairlist (cl:cdr x) (cl:cdr y) a)))))

(defun assoc (x a)
  (cl:cond ((equal (cl:caar a) x)
            (cl:car a))
           (t (assoc x (cl:cdr a)))))
