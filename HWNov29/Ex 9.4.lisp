(in-package :cs325-user)

(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
   ((> x1 x2) (intersect-segments x2 y1 x1 y2 x3 y3 x4 y4))
   ((> y1 y2) (intersect-segments x1 y2 x2 y1 x3 y3 x4 y4))
   ((> x3 x4) (intersect-segments x1 y1 x2 y2 x4 y3 x3 y4))
   ((> y3 y4) (intersect-segments x1 y1 x2 y2 x3 y4 x4 y3))
   (t (regular-intersect-segments x1 y1 x2 y2 x3 y3 x4 y4))))


(defun regular-intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
   ((no-intersect x1 y1 x2 y2 x3 y3 x4 y4) nil)
   (t (values-list (list (max x1 x3) (max y1 y3) (min x2 x4) (min y2 y4))))))

(defun no-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (or (> x1 x4) (< x2 x3) (> y1 y4) (< y2 y3)))


;;;; This is box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((> x1 x2) (intersect-segments x2 y2 x1 y1 x3 y3 x4 y4))
        ((> x3 x4) (intersect-segments x1 y1 x2 y2 x4 y4 x3 y3))
        ((> x1 x3) (intersect-segments x3 y3 x4 y4 x1 y1 x2 y2))
        (t (regular-intersect-segments x1 y1 x2 y2 x3 y3 x4 y4))))

(defun regular-intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
   ((is-point x1 y1 x2 y2) (point-intersect x1 y1 x3 y3 x4 y4))
   ((is-point x3 y3 x4 y4) (point-intersect x3 y3 x1 y1 x2 y2))
   ((= x1 x2) (y-intersect x1 y1 y2 x3 y3 x4 y4))
   ((= x3 x4) (y-intersect x3 y3 y4 x1 y1 x2 y2))
   ((is-parallel x1 y1 x2 y2 x3 y3 x4 y4)
    (parallel-intersect x1 y1 x2 y2 x3 y3 x4 y4))
   (t (regular-intersect x1 y1 x2 y2 x3 y3 x4 y4))))
         

(defun y-intersect (x y1 y2 x3 y3 x4 y4)
  (cond 
   ((= x3 x4) (if (= x x3) (y-parallel-intersect x y1 y2 y3 y4) nil))
   ((<= x3 x x4)
    (let* ((k (get-k x3 y3 x4 y4))
           (b (get-b x3 y3 x4 y4))
           (y (get-y x k b)))
      (if (<= y1 y y2)
          (values-list (x y))
        nil)))
   (t nil)))


(defun y-parallel-intersect (x y1 y2 y3 y4)
  (cond 
   ((> y1 y2) (y-parallel-intersect x y2 y1 y3 y4))
   ((> y3 y4) (y-parallel-intersect x y1 y2 y4 y3))
   ((> y1 y3) (y-parallel-intersect x y3 y4 y1 y2))
   ((< y2 y3) nil)
   (t (values-list (list x (max y1 y3) x (min y2 y4))))))


(defun get-b (x1 y1 x2 y2)
  (/ (- (* y1 (- x2 x1)) (* x1 (- y2 y1))) (- x2 x1)))


(defun get-y (x k b)
 (+ (* k x) b))


(defun is-point (x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))


(defun point-intersect (x y x1 y1 x2 y2)
  (if (or (is-point x y x1 y1)
          (is-point x y x2 y2)
          (and (= (get-k x1 y1 x y)
                  (get-k x y x2 y2))
               (<= x1 x x2)
               (or (<= y1 y y2) (<= y2 y y1))))
      (values-list (list x y x y))
    nil))


(defun is-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (= (get-k x1 y1 x2 y2)
     (get-k x3 y3 x4 y4)))


(defun get-k (x1 y1 x2 y2)
  (if (= x1 x2)
      most-positive-fixnum
    (/ (- y2 y1) (- x2 x1))))


(defun parallel-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (if (is-on-same-line x1 y1 x2 y2 x3 y3 x4 y4)
      (same-line-intersect x1 y1 x2 y2 x3 y3 x4 y4)
    nil))


(defun same-line-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond 
   ((< x2 x3) nil)
   ((< x4 x2) (values-list (list x3 y3 x4 y4)))
   (t (values-list (list x3 y3 x2 y2)))))


(defun is-on-same-line (x1 y1 x2 y2 x3 y3 x4 y4)
  (is-parallel x1 y1 x3 y3 x2 y2 x4 y4))

  
(defun regular-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((cp (get-cross-point x1 y1 x2 y2 x3 y3 x4 y4)))
    (if (cp-at-lines cp x1 y1 x2 y2 x3 y3 x4 y4)
        (values-list cp)
      nil)))


(defun cp-at-lines (cp x1 y1 x2 y2 x3 y3 x4 y4)
  (and (<= x1 (car cp) x2)
       (<= x3 (car cp) x4)
       (or (<= y1 (cadr cp) y2) (<= y2 (cadr cp) y1))
       (or (<= y3 (cadr cp) y4) (<= y4 (cadr cp) y3))))


(defun get-cross-point (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((k1 (get-k x1 y1 x2 y2))
         (b1 (get-b x1 y1 x2 y2))
         (k2 (get-k x3 y3 x4 y4))
         (b2 (get-b x3 y3 x4 y4)))
    (get-cross-point-by-k-b k1 b1 k2 b2)))


(defun get-cross-point-by-k-b (k1 b1 k2 b2)
  (let* ((x (/ (- b1 b2) (- k2 k1)))
         (y (get-y x k1 b1)))
    (list x y)))
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((k1 (slope x1 y1 x2 y2))
        (k2 (slope x3 y3 x4 y4)))
    (cond ((is-point x1 y1 x2 y2)
           (point-intersect x1 y1 x3 y3 x4 y4))
          ((is-point x3 y3 x4 y4)
           (point-intersect x3 y3 x1 y1 x2 y2))
          ((judge-parallel x1 y1 x2 y2 x3 y3 x4 y4)
           (parallel-intersect k1 x1 y1 x2 y2 x3 y3 x4 y4))
          (t (nonparallel-intersect k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)))))

(defun slope (x1 y1 x2 y2)
  (if (= x1 x2) most-positive-fixnum
    (/ (- y2 y1) (- x2 x1))))

(defun judge-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (if (= (slope x1 y1 x2 y2) (slope x3 y3 x4 y4))
      t
    nil))

(defun work-not-parallel (k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
   ((and (/= k1 most-positive-fixnum)
         (/= k2 most-positive-fixnum))
    (find-point k1 k2 x1 y1 x2 y2 x3 y3 x4 y4))
   ((= k1 most-positive-fixnum)
    (report x1 x3 y3 x4 y4))
   ((= k2 most-positive-fixnum)
    (report x3 x1 y1 x2 y2))
   (t nil)))
  
(defun work-parallel (k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
   ((and (= k1 most-positive-fixnum)
         (= x1 x3))
    (intersect-x x1 y1 y2 y3 y4))
   ((and (zerop k1) (= y1 y3))
    (intersect-y y1 x1 x2 x3 x4))
   ((eql (getb x1 y1 x2 y2)
         (getb x3 y3 x4 y4))
    (intersect k1 x1 y1 x2 y2 x3 y3 x4 y4))
   (t nil)))

(defun is-point (x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))
    
(defun report-point (x y x1 y1 x2 y2)
  (when (point-on-segment x y x1 y1 x2 y2)
    (values x y x y)))

(defun find-point(k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (let*((b1 (getb x1 y1 x2 y2))
        (b2 (getb x3 y3 x4 y4))
        (x (/ (- b2 b1) (- k1 k2)))
        (y (/ (- (* b1 k2) (* b2 k1)) (- k2 k1))))
    (when (and (point-on-segment x y x1 y1 x2 y2)
               (point-on-segment x y x3 y3 x4 y4))
      (values x y))))

(defun report(x x1 y1 x2 y2)
  (let ((y (+ (* (/ (- x x1) (- x2 x1)) (- y2 y1)) y1)))
    (when (and (>= y (min y1 y2))
               (<= y (max y1 y2)))
      (values x y))))
  
                            
(defun intersect-x (x a1 a2 b1 b2)
  (when (and (>= (max b1 b2) (min a1 a2))
             (>= (max a1 a2) (min b1 b2)))
    (values x (min (max a1 a2) (max b1 b2))
            x (max (min a1 a2) (min b1 b2)))))

(defun intersect-y (y a1 a2 b1 b2)
  (when (and (>= (max b1 b2) (min a1 a2))
           (>= (max a1 a2) (min b1 b2)))
    (values (min (max a1 a2) (max b1 b2)) y
            (max (min a1 a2) (min b1 b2)) y)))

(defun intersect (k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (when (and (>= (max x1 x2) (min x3 x4))
             (>= (max x3 x4) (min x1 x2)))
    (if (> k1 0)
        (values1 x1 y1 x2 y2 x3 y3 x4 y4)
      (values2 x1 y1 x2 y2 x3 y3 x4 y4))))

(defun values1 (x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max (min x1 x2) (min x3 x4))
          (max (min y1 y2) (min y3 y4))
          (min (max x1 x2) (max x3 x4))
          (min (max y1 y2) (max y3 y4))))

(defun values2 (x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max (min x1 x2) (min x3 x4))
          (min (max y1 y2) (max y3 y4))
          (min (max x1 x2) (max x3 x4))
          (max (min y1 y2) (min y3 y4))))

(defun getb (x1 y1 x2 y2)
  (if(= x1 x2)
      most-negative-fixnum
    (- y1 (/ (* x1 (- y2 y1)) (- x2 x1)))))

(defun point-on-segment (x y x1 y1 x2 y2)
  (and (eql y
            (+ (* x (getk x1 y1 x2 y2))
               (getb x1 y1 x2 y2)))
       (>= x (min x1 x2))
       (<= x (max x1 x2))))












