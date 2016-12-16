(in-package :cs325-user)

(define-test unifies-p
  (assert-true (unifies-p '?x '?x))
  (assert-true (unifies-p '(?x a) '(a ?x)))
  (assert-false (unifies-p '(?x b) '(a ?x)))
  (assert-true (unifies-p '(a ?x) '(?x ?x)))
  (assert-true (unifies-p '(?x ?y) '(?y a)))
  (assert-false (unifies-p '?x '(f ?x)))
  (assert-false (unifies-p '(f ?x) '?x))
  (assert-false (unifies-p '(?x ?y) '(?y (f ?x))))
  (assert-true (unifies-p '(?x a) '(?x ?x)))
  )

(defun unifies-p (x y) (not (null (unify x y '(nil)))))

(defun unify (pat form blists)
  (cond ((var-p pat) (var-match-p pat form blists))
        ((var-p form) (var-match-p form pat blists))
        ((eql pat form) blists)
        ((or (atom pat) (atom form)) nil)
        (t
         (unify (cdr pat) (cdr form)
                (unify (car pat) (car form) blists)))))

(defun var-match-p (var form blists)
  (if (eql var form) blists
    (let ((binding (assoc var (car blists))))
      (cond ((and (null binding) (can-bind-p var form blists))
             (list (cons (cons var form) (car blists))))
            (t (unify (cdr binding) form blists))))))

(defun can-bind-p (var form blists)
  (or (atom form)
      (not (inside-p var form blists))))

(defun inside-p (x container blists)
  (if (atom container)
      (unify x container blists)
    (or (inside-p x (car container) blists)
        (inside-p x (cdr container) blists))))

(defun var-p (pat)
  (and (symbolp pat) (eql (char (symbol-name pat) 0) #\?)))