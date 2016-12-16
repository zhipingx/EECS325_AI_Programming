(in-package :cs325-user)





(defun find-range (fn fr to)
  (do ((step (if (> to fr) 1 -1))
       (i fr (+ step i)))
      ((or (= i to) (funcall fn i)) (if (= i to) nil i))))

(defun every-range (fn fr to)
  (do ((step (if (> to fr) 1 -1))
      (i fr (+ step i)))
      ((or (= i to) (not (funcall fn i))) (= i to))))

(defun map-range (fn fr to)
  (do ((step (if (> to fr) 1 -1))
       (i fr (+ step i))
       (lst nil (cons (funcall fn i) lst)))
      ((= i to) (reverse lst))))





(defun map-range (fn fr to)
  (mapcar fn (range fr to (if (> to fr) 1 -1))))

(defun find-range (fn fr to)
  (find-if fn (range fr to (if (> to fr) 1 -1))))

(defun every-range (fn fr to)
  (every fn (range fr to (if (> to fr) 1 -1))))

; range from PYTHON!
(defun range (fr to &optional (step 1))
  (if (= fr to)
      nil
    (cons fr (range (+ fr step) to step))))

find-range and every-range should not need any CONSing. map-range should not do more CONSes than the length of the output list.