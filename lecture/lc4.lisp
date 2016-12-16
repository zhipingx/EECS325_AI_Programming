(in-package :cs325-user)

(define-test get-value
  (init-test)
  (assert-equal nil (get-value 'chilly 'locomotion))
  (add-isa 'chilly 'animal)
  (assert-equal 'walk (get-value 'chilly 'locomotion))
  (add-isa 'chilly 'bird)
  (assert-equal 'fly (get-value 'chilly 'locomotion))
  (add-isa 'chilly 'penguin)
  (assert-equal 'walk (get-value 'chilly 'locomotion))
  (add-isa 'chilly 'volatis)
  (assert-equal 'fly (get-value 'chilly 'locomotion))
  )


(defparameter *triples* ())

(defun add-isa (obj isa)
  (setq *triples*
        (append *triples*
                (list (list obj 'is-a isa)))))

(defun get-value (obj prop)
  (or (some (lambda
             (triple)
             (get-triple-value obj prop triple))
            *triples*)
      (some (lambda (x) (get-value x prop))
            (get-isas obj)
            )
      ))

(defun get-triple-value (obj prop triple)
  (and (eql obj (car triple))
       (eql prop (cadr triple)) (caddr triple)))

(defun get-isas (obj)
  (let ((isas (raw-isas obj)))
    (remove-if (lambda (isa)
                 (some (lambda (other)
                         (and (not (eql other isa))
                              (isa-p other isa)))
                       isas))
               isas)))

(defun raw-isas (obj)
  (filter (lambda (triple)
            (get-triple-value obj 'is-a triple))
          *triples*))

;; true if x is a y
(defun isa-p (x y)
  (or (eql x y)
      (some (lambda (isa) (isa-p isa y))
            (raw-isas x))))

(defun filter (fn lst)
  (mapcan (lambda (x)
           (let ((v (funcall fn x)))
            (and v (list v))))
          lst))

(defun init-test ()
  (setq *triples*
        '((bird is-a animal)
          (penguin is-a bird)
          (volatis is-a penguin)
          (animal locomotion walk)
          (bird locomotion fly)
          (penguin locomotion walk)
          (volatis locomotion fly))))