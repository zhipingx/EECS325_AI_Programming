(in-package :cs325-user)
; Define a structure to represent a tree where each node 
; contains some data and has up to three children. 
; Define:
;   (a) a function to copy such a tree 
;     (so that no node in the copy is eql to a node in the original)
;   (b) a function that takes an object and such a tree, 
;     and returns true if the object is eql to the data field of one of the nodes

; (top (a (b nil (c nil nil) (d nil nil)) nil nil) nil (e nil nil (f nil (g nil nil) nil)))

(defstruct 3tree
  data
  left
  middle
  right)


(defun 3tree-clone (tree)
  (if (null tree)
      nil
    (make-3tree :data (3tree-data tree)
                :left (3tree-clone (3tree-left tree))
                :middle (3tree-clone (3tree-middle tree))
                :right (3tree-clone (3tree-right tree)))))


(defun 3tree-member (obj tree)
  (cond 
   ((null tree) nil)
   ((eql obj (3tree-data tree)) t)
   (t (or (3tree-member obj (3tree-left tree))
          (3tree-member obj (3tree-middle tree))
          (3tree-member obj (3tree-right tree))))))
