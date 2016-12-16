(in-package :cs325-user)
#|
(defun longest-path (start end net)
  (reverse (dfs (list start)
                (if (eql start end)
                    (list start)
                  nil)
                end net)))

(defun dfs (path good-path end net)
  (cond
   ((and (eql (car path) end) (cdr path))
    (better-path path good-path))
   ((member (car path) (cdr path))
    good-path)
   (t
    (let ((neighbours (cdr (assoc (car path) net))))
      (do ((nodes neighbours (cdr nodes))
           (best-path
            good-path 
            (better-path good-path 
                         (dfs (cons (car nodes) path) best-path end net))))
          ((null nodes) best-path))))))
|#
(defun dfs (path end net)
  (cond
   ((and (eql (car path) end) (cdr path))
    path)
   ((member (car path) (cdr path))
    nil)
   (t
    (let ((neighbours (cdr (assoc (car path) net))))
      (do ((nodes neighbours (cdr nodes))
           (best-path nil 
            (better-path best-path (dfs (cons (car nodes) path) end net))))
          ((null nodes) best-path))))))

(defun longest-path (start end net)
  (or
   (reverse (dfs (list start) end net))
   (if (eql start end) (list start) nil)))

(defun better-path (path1 path2)
  (if (> (length path1) (length path2))
      path1
    path2))

#|
(defun dfs (path end net)
  (cond
   ((and (eql (car path) end) (cdr path))
    (list (list end)))
   ((member (car path) (cdr path))
    nil)
   (t
    (mapcan #'(lambda (node)
                (mapcar #'(lambda (p)
                                  (cons (car path) p))
                              (dfs (cons node path) end net)))
            (cdr (assoc (car path) net))))))
|#
#|
(defun dfs (start end net origin passed)
  (when (cdr (assoc start net))
  (mapcar #'(lambda (n)
              (format t "~a, ~a, ~a~%" n start (append n (list start)))
              (append n (list start)))
          (mapcar #'(lambda (node)
                      (dfs node end paths net))
                  (cdr (assoc start net))))))
|#        
    




(setf min '((a b c) (b c) (c d)))
