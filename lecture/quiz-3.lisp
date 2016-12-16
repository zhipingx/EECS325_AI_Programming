(in-package :cs325-user)
(defparameter *triples* ())

(defun add-isa (obj isa)
  (setq *triples*
        (append *triples*
                (list (list obj 'is-a isa)))))

;; why lambda here
(defun get-value (obj prop)
  (or (some (lambda (triple)
              (get-triple-value obj prop triple))
            *triples*)
      (some (lambda (x) (get-value x prop))
            (get-isas obj))))

(defun get-triple-value (obj prop triple)
  (and (eql obj (car triple))
       (eql prop (cadr triple)) (caddr triple)))

(defun get-isas (obj)
  (filter (lambda
           (triple)
           (get-triple-value obj 'is-a triple))
          *triples*))

(defun filter (fn lst)
  (mapcan (lambda (x)
            (let
                ((v (funcall fn x)))
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
          (bird volatis fly))))