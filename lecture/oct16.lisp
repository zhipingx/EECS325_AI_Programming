(in-package :cs325-user)

(defparameter *triples*
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

(define-test query
  (assert-equal '(((?x fish)) ((?x whale)))
    (query '((?x lives-in water))))
  (assert-equal '(((?x whale)))
    (query '((?x lives-in water) (?x isa mammal))))
  (assert-equal '(((?y mammal) (?x whale)))
    (query '((?x lives-in water) (?x isa ?y) (?y has vertebra))))
  )

;;; NEW
(defun read-triples (file)
  (with-open-file (in file)
    ;; first expression in file is a list of package names
    (dolist (name (read in))
      (or (find-package name) (make-package name)))

    ;; remaining expressions are triples to load
    (do ((triple (read in nil) (read in nil))
         (triples nil (cons triple triples)))
        ((null triple)
         (when triples 
           (setq *triples* (nreverse triples)))
         (length triples)))))

;;; FIXED
;;; query '((?x isa mammal) (?x has fur))
;;; (((?X CAT)) ((?X BEAR)))
(defun query (lst &optional bl)
  (if (null lst)
      (list bl)
    (query-collect *triples* (car lst) (cdr lst) bl)))

;;; query-collect *triples* '(?x isa mammal) '((?x has fur)) nil
;;; (((?X CAT)) ((?X BEAR)))
(defun query-collect (triples item lst bl)
  (mapcan (lambda (triple)
            (let ((bl2 (match-p item triple bl)))
              (and bl2 (query lst bl2))))
          triples))

;;; MATCH-P from before, with a stub added for filters
;;; match-p '(?x isa mammal) '(bear isa mammal) nil
;;; ((?X BEAR))
;;;
(defun match-p (pat form &optional blist)
  (cond ((var-p pat) (var-match-p pat form blist))
        ((eql pat form) (or blist '(nil)))
        ((atom pat) nil)
        ((atom form) nil)
        ((functional-rel-p pat) (match-functional pat form blist))
        (t 
         (let ((new-blist (match-p (car pat) (car form) blist)))
           (and new-blist
                (match-p (cdr pat) (cdr form) new-blist))))))

;;; To be defined in next class
(defun functional-rel-p (pat) nil)
(defun match-functional (pat form blist) nil)

(defun var-p (pat)
  (and (symbolp pat)
       (eql (char (symbol-name pat) 0) #\?)))

(defun var-match-p (pat form blist)
  (let ((binding (assoc pat blist)))
    (cond ((null binding) (cons (list pat form) blist))
          ((equal form (cadr binding)) blist)
          (t nil))))
