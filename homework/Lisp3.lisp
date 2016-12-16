(in-package :cs325-user)

(defun make-balance (balance)
  #'(lambda (&optional incre)
      (if (null incre)
          balance
        (setq balance (+ incre balance)))))

(defun make-balance (balance)
  #'(lambda (&optional income)
      (if (null income)
          balance
        (incf balance income))))

(defun make-balance (balance)
  #'(lambda (&optional (income 0))
      (incf balance income)))
      (if (null income)
          balance
        (incf balance income))))

(defun foo (n &optional (a 1) (b 0))
  (if (= n 0) b (foo (1- n) (+ a b) a)))

(defun baz (m n)
  (cond ((= m 0) (1+ n))
        ((= n 0) (baz (1- m) 1))
        (t (baz (1- m) (baz m (1- n))))))