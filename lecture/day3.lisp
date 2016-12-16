(in-package :cs325-user)

(defparameter *animal-triples*
  '((mammal isa animal)
    (mammal has vertebra)
    (cat isa mammal)
    (cat has fur)
    (bear isa mammal)
    (bear has fur)
    (fish isa animal)
    (fish lives-in water)
    (whale isa mammal)
    (whale lives-in water)
    (animal isa physical-object)
    (physical-object has mass)
    (cat isa pet)
    (pet has owner)
    ))

(define-test has-p 
  (assert-true (has-p 'cat 'fur))
  (assert-true (has-p 'bear 'fur))
  (assert-false (has-p 'whale 'fur))
  (assert-false (has-p 'fish 'fur))
  (assert-true (has-p 'cat 'vertebra))
  (assert-true (has-p 'bear 'vertebra))
  (assert-true (has-p 'whale 'vertebra))
  (assert-false (has-p 'fish 'vertebra))
  (assert-true (has-p 'cat 'mass))
  (assert-true (has-p 'fish 'mass))
  (assert-true (has-p 'cat 'owner))
  )

(define-test get-isa 
  (assert-equal '(mammal pet) (get-isa 'cat))
  (assert-equal '(animal) (get-isa 'fish))
  (assert-equal nil (get-isa 'physical-object))
  )

(defun get-isa (obj &optional (triples *animal-triples*))
  (if (null triples) nil
    (if (and (eql (cadar triples) 'isa) (eql (caar triples) obj))
        (cons (caddar triples)
              (get-isa obj (cdr triples)))
      (get-isa obj (cdr triples)))))
  
;;; NOT FINISHED
(defun has-p (obj value)
  (or (member (list obj 'has value) *animal-triples* :test 'equal)))