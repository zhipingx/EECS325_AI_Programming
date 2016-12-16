(in-package :cs325-user)


;;; 2-4 ~
(defun greater (x y)
  (if (> x y) x y))


;;; 2-7 ~
(defun  has-list-p (lst)
  (if arr
      (or (listp (car lst)) (has-list-p (cdr lst)))
    nil))


;;; 2-8 ~
(defun print-dots (n)
  (do ((i 0 (1+ i)))
      ((= i n))
    (format t ".")))


;;; Cheat
;;; (defun get-a-count (lst)
;;;  (count 'a lst))

;;; This one passed critique
(defun get-a-count (lst)
  (if (null lst)
      0
    (+ (get-a-count (cdr lst)) (if (eql (car lst) 'a) 1 0))))
;;; This one didn't
(defun get-a-count (lst)
  (let ((len 0))
    (dolist (obj lst)
      (when (eql obj 'a) (incf len)))
    len))

(defun get-a-count (lst)
  (do ((ll lst (cdr lst))
       (len 0 ((if (eql (car ll) 'a) (1+ len) (len)))))
      ((null ll) (len))))




;;; 2-9
; a
;(defun summit (lst)
;  (apply #'+ (remove nil lst)))
; b
(defun summit (lst)
  (if (null lst)
      0
    (let ((x (car lst)))
      (if (null x)
          (summit (cdr lst))
        (+ x (summit (cdr lst)))))))


; I'm not sure about the has-list-p
; Are you suggesting to not use do or dolist in this case?
; What I learned from the reading is:
; We should use do to iterative and cumulate. At the same time.
; Otherwise, use something else, like this:
(defun get-a-count (lst)
  (do ((ll lst (cdr ll))
       (len 0 (if (eql (car ll) 'a) (1+ len) len)))
      ((null ll) len)))
; Did I got the point?

; ll is used to stepping through the lst
; len is used to cumulate the count of 'a
; The thing is: cumulate 'a require a judgement to see if car ll is eql to 'a
; In such case, should we keep useing 


; And about the explaination of summit
; a) Remove is not destructive.
;    Which means it do not change the actual value of lst
; b) A clause to stop the recursion is missed.
;    We need something to break the circle.
