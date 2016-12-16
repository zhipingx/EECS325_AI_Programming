(in-package :cs325-user)
#|
(defun show-dots (lst)
  (cond
   ((atom lst) (format t "~A" lst))
   (T
    (format t "(")
    (if (listp (car lst))
        (show-dots (car lst))
      (format t "~A" (car lst)))
    (format t " . ")
    (show-dots (cdr lst))
    (format t ")"))))
|#
(defun show-dots (lst)
  (cond
   ((atom lst) (format t "~a" lst))
   (t
    (format t "(")
    (show-dots (car lst))
    (format t " . ")
    (show-dots (cdr lst))
    (format t ")"))))


(defun show-list (lst)
  (cond
   ((atom lst)
    (format t "~a" lst))
   (t
    (format t "[")
    (do ((ll lst (cdr ll)))
        ((atom ll) (unless (null ll) (format t ". ~a" ll)))
        (show-list (car ll))
        (unless (null (cdr ll))
          (format t " ")))
    (format t "]"))))


(defun show-list (lst)
  (cond
   ((atom lst)
    (format t "~a" lst))
   (t
    (format t "[")
    (show-list (car lst))
    (do ((ll (cdr lst) (cdr ll)))
        ((atom ll) (unless (null ll) (format t " . ~a" ll)))
      (format t " ")
      (show-list (car ll)))
    (format t "]"))))

(if (atom (car lst))
        (format t "~a" (car lst))
      (show-list (car lst)))






