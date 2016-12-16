(in-package :cs325-user)

(defun has-number-p (s-exp)
  (catch 'founded
    (dfs s-exp)
    nil
    ))

(defun dfs (s-exp)
  (if (atom s-exp)
      (if (numberp s-exp)
          (throw 'founded t)
        nil)
    (if (numberp (car s-exp))
        (throw 'founded t)
      (dfs (cdr s-exp)))))

(defun has-number-p (s-exp)
  (if (null s-exp)
      nil
    (if (numberp s-exp)
        t
      (if (listp s-exp)
          (some #'has-number-p s-exp)
        nil))))

(defun has-number-p (s-exp)
  (if (atom s-exp)
    (numberp s-exp)
    (some #'has-number-p s-exp)))
    