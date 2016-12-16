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
  (assert-true (unifies-p '?x '(a)))
  (assert-false (unifies-p '(?y ?x) '((f ?x) ?y)))
  (assert-true (unifies-p '(?x a) '((f ?y) ?y)))
  (assert-false (unifies-p '(a ?x) '(b c)))
  (assert-true (unifies-p '(?y ?x a) '(?x ?y ?x)))
  )

(define-test ask
  (assert-true (ask '(mortal socrates)))
  (assert-true (ask '(mortal pluto)))
  (assert-false (ask '(mortal shakey)))
  (assert-true (ask '(mortal robby)))
  (assert-true (some (lambda (blist)
                       (member 'robby blist :key 'cdr))
                     (ask '(mortal ?x))))
  (assert-true (ask '(descendant-of jon mackenzie)))
  (assert-true (ask '(descendant-of max jon)))
  (assert-true (ask '(descendant-of chris jon)))
  (assert-true (ask '(descendant-of chris mackenzie)))
  (assert-true (ask '(descendant-of max mackenzie)))
  )

(defparameter *kb*
    (quote
     ((<- (mortal ?x) (robot ?x) (made-by us-robotics ?x))
      (<- (robot shakey)) (<- (robot robby))
      (<- (made-by us-robotics robby))))) 
;                 ^
;                 |
; Why is there a space?

; (ask '(mortal robby))
; (((?X . ROBBY)))



(defun ask (x &optional (blists (list nil)))
  (mapcan (lambda (r) (bc (cddr r) (unify x (cadr r) blists)))
          *kb*))

; ?
(defun bc (qs blists)
  (and blists
       (if (null qs)
           blists
         (bc (cdr qs) (ask (car qs) blists)))))

; Try to match things, return matched result: (unifies-p '(?x ?y) '(?y a))
(defun unify (pat form blists)
  (cond ((null blists) nil)
        ((var-p pat) (var-match-p pat form blists))
        ((var-p form) (var-match-p form pat blists))
        ((eql pat form) blists)
        ((or (atom pat) (atom form)) nil)
        (t
         (unify (cdr pat) (cdr form)
                (unify (car pat) (car form) blists)))))

; ? try to match var, return a list of blists containing all possible blist
(defun var-match-p (var form blists)
  (if (eql var form)
      blists
    (let ((binding (assoc var (car blists)))) ;??
      (cond ((and (null binding) (can-bind-p var form blists))
             (list (cons (cons var form) (car blists))))
            (t (unify (cdr binding) form blists))))))

; See if it's okey to bind p
; atom form is okey
; no looping var in form is okey
#|
(defun can-bind-p (var form blists)
  (or (atom form)
      (not (inside-p var form blists))))
|#
(defun can-bind-p (var form blists)
  (or (and (atom form) (not (var-p form)))
      (not (inside-p var form blists))))

; see can we find x in containter
#|
(defun inside-p (x container blists)
  (if (atom container)
      (or (eql x container)
          (and (var-p container)
               (inside-p x (cdr (assoc container (car blists)))
                         blists)))
    (or (inside-p x (car container) blists)
        (inside-p x (cdr container) blists))))
|#

(defun inside-p (x container blists)
  (cond ((eql x container) t)
        ((var-p container)
         (inside-p x (cdr (var-binding container blists)) blists))
        ((atom container) nil)
        (t
         (or (inside-p x (car container) blists)
             (inside-p x (cdr container) blists)))))

(defun var-binding (var blists)
  (assoc var (car blists)))

; Check if pat is var (start with ?)
(defun var-p (pat)
  (and (symbolp pat) (eql (char (symbol-name pat) 0) #\?)))