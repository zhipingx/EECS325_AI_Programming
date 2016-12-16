(in-package :cs325-user)

(defparameter *triples*
    (quote
     ((boat is-a thing)
      (day-boat is-a boat)
      (wheel-boat is-a boat)
      (engineless-boat is-a day-boat)
      (small-multi-hull-boat is-a day-boat)
      (pedal-wheel-boat is-a engineless-boat)
      (pedal-wheel-boat is-a wheel-boat)
      (small-catamaran is-a small-multi-hull-boat)
      (pedalo is-a pedal-wheel-boat)
      (pedalo is-a small-catamaran)
      (day-boat navzone 5)
      (wheel-boat navzone 100))))

#|

|#

(defun get-value (obj prop)
  (some (lambda (isa)
          (some
           (lambda (triple)
            (get-triple-value isa prop triple))
           *triples*))
        (all-isas obj)))

(defun all-isas (obj)
  (remove-duplicates
     (cons obj
           (mapcan 'all-isas (raw-isas obj)))))

(defun raw-isas (obj)
  (filter (lambda
           (triple)
           (get-triple-value obj 'is-a triple))
          *triples*))

(defun filter (fn lst)
  (mapcan (lambda
           (x)
           (let
            ((v (funcall fn x)))
            (and v (list v))))
          lst))

(defun get-triple-value (obj prop triple)
  (and (eql obj (car triple))
       (eql prop (cadr triple)) (caddr triple)))