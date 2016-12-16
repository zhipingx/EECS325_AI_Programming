;;; Oct-25-2016 File released

;;; Code defined in class, put into the TRIPLES package
;;;
;;; Has bug handling filter patterns like (?w (>) 100)
;;; See triple-tests.lisp

(defpackage #:triples
  (:use #:common-lisp)
  (:export #:query #:read-triples #:read-nt #:*triples*)
  )

(in-package :triples)

(defun read-triples (file)
  (with-open-file (in file)
    ;; first expression in file is a list of any packages
    ;; that must be defined first
    ;; may be just a name or (name URI) pair
    (dolist (ns (read in))
      (let ((name (if (atom ns) ns (car ns))))
        (or (find-package name) (make-package name))))

    ;; remaining expressions are triples to load
    (do ((triple (read in nil) (read in nil))
         (triples nil (cons triple triples)))
        ((null triple) (nreverse triples)))))

;;; Following code does not pass the filter test cases

(defun query (lst &optional bl)
  (if (null lst)
      (list bl)
    (query-collect *triples* (car lst) (cdr lst) bl)))

(defun query-collect (triples item lst bl)
  (mapcan (lambda (triple)
            (let ((bl2 (match-p item triple bl)))
              (and bl2 (query lst bl2))))
          triples))
;; (query-collect *triples* '(?W (>) 100) nil '((?W 66000) (?A WHALE)))
;; (query-collect *triples* '(?Y HAS VERTEBRA) nil '((?Y MAMMAL) (?X WHALE)))
(defun query-collect (triples item lst bl)
  (if (functional-rel-p item)
      (let ((bl2 (match-functional item nil blist)))
        (and bl2 (query lst bl2)))
    (mapcan (lambda (triple)
              (let ((bl2 (match-p item triple bl)))
                (and bl2 (query lst bl2))))
            triples)))

;;; MATCH-P with function-based matching
;; match-p '(?Y HAS VERTEBRA) '(MAMMAL ISA ANIMAL) '((?Y MAMMAL) (?X WHALE))
;; match-p '(?Y HAS VERTEBRA) '(MAMMAL HAS VERTEBRA) '((?Y MAMMAL) (?X WHALE))
;; match-p '(?W (>) 100) '(MAMMAL ISA ANIMAL) '((?W 66000) (?A WHALE))
;; match-p '(?W (>) 100) '(WHALE WEIGHT 66000) '((?W 66000) (?A WHALE))
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

;;; NEW CODE

;;; True for triple like ((?a weight ?w) (?w (>) 100))
(defun functional-rel-p (pat) (consp (cadr pat)))

;;; True for values like this
;;; pat = (?w (>) 100)
;;; form = ? first triple
;;; blist = ((?w 240) (?a bear))
(defun match-functional (pat form blist)
  (let ((value1 (pat-value (car pat) blist))
        (value2 (pat-value (caddr pat) blist)))
    (if (funcall (caadr pat) value1 value2)
        blist
      nil)))

;;; We need a method, to see if blist can be formed by form
;;; '(WHALE WEIGHT 66000) '((?W 66000) (?A WHALE))
;;; (match-p '(?contains (?and (?? numberp) ?x)) '((a 12) c (((5)))))
;;; (((?X . 5)) ((?X . 12)))

(defun pat-value (pat blist)
  (let ((binding (assoc pat blist)))
    (if binding (cadr binding) pat)))

(defun var-p (pat)
  (and (symbolp pat)
       (eql (char (symbol-name pat) 0) #\?)))

(defun var-match-p (pat form blist)
  (let ((binding (assoc pat blist)))
    (cond ((null binding) (cons (list pat form) blist))
          ((equal form (cadr binding)) blist)
          (t nil))))