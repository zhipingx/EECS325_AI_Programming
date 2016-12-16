(in-package :cs325-user)

(setf min '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun shortest-path (start end net)
  (let ((l (wtf start end net)))
    (format t "???????????????????????????? ~a" l)
    (reverse l)))


(defun shortest-path (start end net)
  (let ((path (bfs (list (list start))
                   (lambda (s) (member end (get-neighbours s net)))
                   (lambda (s) (get-neighbours s net)))))
    (if (null path)
        nil
      (reverse (cons end path)))))

(defun shortest-path (start end net)
  (let ((path (bfs (list (list start))
                   (lambda (s) (eql end s)) 
                   (lambda (s) (get-neighbours s net)))))
    (if (null path)
        nil
      (reverse (cons end path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Iterative Deepening Search
(defun shortest-path (start end net)
  (reverse (ids (list start)
                (lambda (s) (eql end s)) 
                (lambda (s) (get-neighbours s net)))))

(defun ids (path pred gen)
  (do ((depth 0 (1+ depth))
       (found nil (dls path pred gen depth)))
      ((or found (= depth 7)) found)))


(defun dls (path pred gen depth)
  (cond
   ((funcall pred (car path)) path)
   ((= depth 0) nil)
   (t (let ((neighbours (valid-neighbours path (funcall gen (car path)))))
      (do ((rest neighbours (cdr rest))
           (found nil (dls (cons (car rest) path) pred gen (1- depth))))
          ((or (null rest) found) found))))))

(defun valid-neighbours (path neighbours)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (list n)))
          neighbours))

(defun get-neighbours (s net)
  (cdr (assoc s net)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Generalized BFS
(defun get-neighbours (s net)
  (cdr (assoc s net)))

(defun shortest-path (start end net)
  (reverse (bfs (list (list start))
                (lambda (s) (eql end s)) 
                (lambda (s) (get-neighbours s net)))))

(defun valid-neighbours (path neighbours)
  (mapcan #'(lambda (n)
              (if (member n path)
                  nil
                (list (cons n path))))
          neighbours))

(defun bfs (paths pred gen)
  (let* ((path (car paths))
         (node (car path))
         (neighbours (funcall gen node)))
    (cond ((null paths) nil)
          ((funcall pred node) path)
          (t (bfs (cdr (append paths (valid-neighbours path neighbours))) pred gen)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun bfs (end queue net)
  (let* ((path (car queue)) (node (car path)) (neighbours (cdr (assoc node net))))
    (cond ((empty-queue-p queue) nil)
          ((member end neighbours)
           (reverse (cons end (car queue))))
          (t (bfs end (cdr (append queue (valid-neighbours path neighbours))) net)))))


















; With throw-catch
(defun bfs (end queue net)
  (catch 'found
    (if (empty-queue-p queue)
        nil
      (let* ((path (car queue)) (node (car path)))
        (if (eql node end)
            ((reverse path))
          (bfs end
               (append (cdr queue)
                       (new-paths path node net end))
               net))))))




(defun new-paths (path node net end)
  (let ((paths
         (mapcan #'(lambda (n)
                     (cond
                      ((eql n end)
                       (throw 'found (reverse (cons n path))))
                      ((null (member n path))
                       (list (cons n path)))
                      (t nil)))
                 (cdr (assoc node net)))))
    paths))






; Without throw-catch
(defun bfs (end queue net)
    (if (empty-queue-p queue)
        nil
      (let* ((path (car queue)) (node (car path)))
        (if (eql node end)
            (reverse path)
          (bfs end
               (append
                (cdr queue)
                (let ((paths
                       (mapcan
                        #'(lambda (n)
                            (cond
                             ((eql n end)
                              (return-from bfs (reverse (cons n path))))
                             ((null (member n path))
                              (list (cons n path)))
                             (t nil)))
                        (cdr (assoc node net)))))
                  paths))
               net)))))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
      nil
    (let* ((path (car queue)) (node (car path)))
      (bfs end (cdr (append queue (mapcan #'(lambda (n)
                  (cond
                   ((eql n end)
                    (return-from bfs (reverse (cons n path))))
                   ((member n path) nil)
                   (t (list (cons n path)))))
              (cdr (assoc node net))))) net))))



(defun bfs (end queue net)
  (cond ((member end (cdr (assoc (car (car queue)) net)))
         (reverse (cons end (car queue))))
        ((empty-queue-p queue) nil)
        (t (bfs end (update-queue queue net) net))))



(defun update-queue (queue net)
  (let* ((path (car queue)) (node (car path)))
    (cdr (append queue 
                 (mapcan #'(lambda (n)
                             (unless (member n path)
                               (list (cons n path))))
                         (cdr (assoc node net)))))))


(defun update-queue (queue net)
  (let* ((path (car queue)) (node (car path)))
    (cdr (append queue 
                 (mapcan #'(lambda (n)
                             (unless (member n path)
                               (list (cons n path))))
                         (cdr (assoc node net)))))))




#|
(defun new-paths (path node net end)
  (let ((paths
         (mapcar #'(lambda (n)
                     (if (eql n end)
                         (throw 'founded (reverse (cons n path)))
                       (unless (member n path)
                         (cons n path))))
                 (cdr (assoc node net)))))
    (remove nil paths)))
|#
