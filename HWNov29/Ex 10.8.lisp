(in-package :cs325-user)

(defmacro nth-expr (n &body body)
  (if (integerp n)
      (nth n body)
    `(case ,n
       ,@(let ((i -1))
           (mapcar #'(lambda(x) `(,(incf i) ,x)) body)))))

(defmacro nth-expr (m &rest exprs)
  (list 'nth (list '1- m) (quote `(list ,@exprs))))

(defmacro nth-expr (m &rest exprs)
  (list 'nth (list '1- m) exprs))



(defmacro nth-expr (m &rest exprs)
  (list 'nth m exprs))

(defmacro nth-expr (m &rest exprs)
  (list 'nth `(1- ,m) `(list ,@exprs)))

(run-tests nth-expr)

(macroexpand-1 '(nth-expr 2 (- 1 0) (+ 1 2) (/ 1 0)))

;;;
(defmacro nth-expr (n &rest exprssions)
  (let ((i (gensym)) (exprs (gensym)))
    `(do ((,i (1- ,n) (1- ,i))
          (,exprs (quote ,exprssions) (cdr ,exprs)))
         ((= ,i 0) (car ,exprs)))))

(defmacro nth-expr (n &rest exprssions)
  (let ((i (gensym)) (exprs (gensym)))
    `(do ((,i ,n (1- ,i))
         (,exprs (list ,exprssions) (cdr ,exprs)))
        ((= ,i 0) (car ,exprs)))))


(defmacro nth-expr (n &rest expressions)
    `(case ,n
       ,@(loop for i in expressions
               for n from 1
               collect `((,n) ,i))))


(defmacro n-of (n expr)
  (let ((number (gensym)) (i (gensym)) (lst (gensym)))
    `(do ((,number ,n)
          (,i 0 (1+ ,i))
          (,lst nil (cons ,expr ,lst)))
         ((= ,i ,number) (nreverse ,lst)))))

(assert-true (functionp (compile nil (lambda (n) (n-of n 'a)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((x 'a)) (print (macroexpand-1 (preserve (x) (setq x 'b)) x)))
(macroexpand-1 '(preserve 2 (- 1 0) (+ 1 2) (/ 1 0)))

(defmacro preserve (params &rest body)
  `((lambda ,params ,@body) ,@params))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; puofei
(define-modify-macro doublef (&optional (y 2)) *)

(defmacro doublef (n)
  (list 'let (list (list 'var n))
        (list 'setf 'var (list '* 'var '2))))

(defmacro doublef (n)
  (list 'setf n (list '* n '2)))


(let ((var n))
  (setf var (* var 2)))

(defmacro br (fn &rest lst)
  (let ((var (gensym)))
    (lx var (cons fn (cons var lst)))))

(let ((q 2)) (macroexpand-1 (doublef q)) q)
