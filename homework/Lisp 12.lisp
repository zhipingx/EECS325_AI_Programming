(in-package :cs325-user)

(defun reduce-tree (fn tree &optional (init nil))
  (cond
   ((null tree) init)
   ((atom tree) (funcall fn init tree))
   (t (do ((l tree (cdr l))
             (result init (reduce-tree fn (car l) result)))
           ((null l) result)))))