(defun foo (s) (caar (p s)))

(defun p (s)
  (let ((r (p2 s))) (if (equal s r) s (p r))))

(defun p2 (s)
  (if (null s)
      nil
    (let ((m (p3 s)))
      (if (null m)
          (cons (car s) (p2 (cdr s)))
        (cons m (nthcdr (length (cdr m)) s))))))

(defun p3 (s)
  (some (lambda (ph) (gp (cdr ph) s (list (car ph))))
        g))

(defun gp (ph s r)
  (cond ((null ph) (values (reverse r) s))
        ((null s) nil)
        ((gp= (car ph) (car s))
         (gp (cdr ph) (cdr s) (cons (car s) r)))
        (t nil)))

(defun gp= (x y) (eql x (if (atom y) y (car y))))

(defparameter g
    (quote
     ((:np i) (:v see) (:det the) (:n ship)
      (:np :det :n) (:vp :v :np) (:s :np :vp))))