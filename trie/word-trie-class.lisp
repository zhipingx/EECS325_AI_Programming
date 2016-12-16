(in-package :cs325-user)


(defpackage trie
  (:export make-trie trie-word trie-count subtrie add-word read-words mapc-trie)
  (:use :common-lisp))


(in-package trie)


(defclass trie ()
  ((word  :accessor trie-word
          :initform nil)
   (count :accessor trie-count
          :initform 0)
   (chars :accessor trie-chars
          :initform nil)))


(defun make-trie ()
  (make-instance 'trie))


(defun add-chars (chars trie word)
  (cond 
   ((null chars)
    (setf (trie-word trie) word)
    (incf (trie-count trie)))
   (t
    (let* ((char (car chars))
           (sub-trie (get-son trie char)))
      (incf (trie-count trie))
      (when (null sub-trie)
        (setf sub-trie (make-trie))
        (push (cons char sub-trie) (trie-chars trie)))
      (add-chars (cdr chars) sub-trie word)))))

      
(defun get-son (trie char)
  (cdr (assoc char (trie-chars trie))))


(defun add-word (word trie)
  (let ((chars (coerce word 'list)))
    (add-chars chars trie word)
    trie))


(defun read-words (file trie)
  (with-open-file (stream file)
    (do ((word (read-line stream nil) (read-line stream nil)))
        ((null word) trie)
      (add-word word trie))))


(defun list-subtrie (trie chars)
  (cond
   ((null trie) nil)
   ((null (car chars)) trie)
   (t (list-subtrie (get-son trie (car chars)) (cdr chars)))))


(defun subtrie (trie &rest chars)
  (list-subtrie trie chars))


(defun mapc-trie (fn trie)
  (mapcar (lambda (c-s) (funcall fn (car c-s) (cdr c-s)))
          (trie-chars trie)))

#|
(run-tests trie-word)
(run-tests trie-count)
(run-tests mapc-trie)
(run-tests subtrie)
(run-tests mapc-trie)
(run-tests read-words)
|#
; (fn char subtrie)
