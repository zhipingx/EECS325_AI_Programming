(in-package :cs325-user)

;;; TEST CASES

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

;;; BACKWARD CHAINING RULES

(defparameter *kb*
    (quote
     ((<- (mortal ?x) (animal ?x))
      (<- (animal ?x) (human ?x))
      (<- (animal ?x) (dog ?x))
      (<- (mortal ?x) (robot ?x) (made-by us-robotics ?x))
      (<- (human socrates))
      (<- (human plato)) 
      (<- (dog pluto))
      (<- (robot shakey))
      (<- (robot robby))
      (<- (made-by us-robotics robby))
      (<- (married john mary))
      (<- (married chris max))
      (<- (child-of chris jon))
      (<- (child-of jon mackenzie))
      (<- (descendant-of ?x ?y) (child-of ?x ?y))
      (<- (descendant-of ?x ?y) (married ?z ?x) (child-of ?z ?y))
      (<- (descendant-of ?x ?y) (child-of ?x ?z) (descendant-of ?z ?y))
      )))

;;; BACKWARD CHAINER

(defun true-p (x) (not (null (ask x))))

(defun ask (x &optional (blists (list nil)))
  (mapcan (lambda (r)
            (bc (cddr r) (unify x (cadr r) blists)))
          *kb*))

(defun bc (qs blists)
  (and blists
       (if (null qs)
           blists
         (bc (cdr qs) (ask (car qs) blists)))))

;;; UNIFIER

(defun unifies-p (x y) (not (null (unify x y '(nil)))))

(defun unify (pat form blists)
  (cond ((null blists) nil)
        ((var-p pat) (var-match-p pat form blists))
        ((var-p form) (var-match-p form pat blists))
        ((eql pat form) blists)
        ((or (atom pat) (atom form)) nil)
        (t
         (unify (cdr pat) (cdr form)
                (unify (car pat) (car form) blists)))))

(defun var-match-p (var form blists)
  (mapcan (lambda (blist)
            (var-bind-p var form blist))
          blists))

(defun var-bind-p (var form blist)
  (if (var-equalp var form blist) (list blist)
    (let ((binding (var-binding var blist)))
      (cond (binding (unify (cdr binding) form (list blist)))
            ((can-bind-p var form blist)
             (list (cons (cons var form) blist)))
            (t nil)))))

(defun var-equalp (var1 var2 blist)
  (and (var-p var2)
       (or (eql var1 var2)
           (var-equalp var1 (cdr (var-binding var2 blist)) blist))))

(defun can-bind-p (var form blist)
  (or (and (atom form) (not (var-p form)))
      (not (inside-p var form blist))))

(defun inside-p (x container blist)
  (cond ((eql x container) t)
        ((var-p container)
         (inside-p x (cdr (var-binding container blist)) blist))
        ((atom container) nil)
        (t
         (or (inside-p x (car container) blist)
             (inside-p x (cdr container) blist)))))

(defun var-binding (var blist)
  (assoc var blist))

(defun var-p (pat)
  (and (symbolp pat) (eql (char (symbol-name pat) 0) #\?)))