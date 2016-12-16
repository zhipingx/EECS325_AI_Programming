(in-package :cs325-user)
; Define a function that takes a filename and returns a list of the expres- sions in the file.

; Instead of making a list of the expressions in a file, define (map-stream function stream) to apply function to every Lisp expression in stream. It should return nil. Define (map-file function pathname) to apply function to every Lisp expression in the file named by pathname, and return nil. These functions can be very useful for checking code files, collecting the names of functions defined in a file, and so on.
; There are test cases only for map-stream.


(defun map-file (function pathname)
  (with-open-file (stream pathname :direction :input)
    (map-stream function stream)))

(defun map-stream (function stream)
  (let ((eof (gensym)))
    (do ((exp (read stream nil eof) (read stream nil eof)))
         ((eql exp eof))
      (funcall function exp))))

(defun map-stream (function stream)
  (do ((exps (read-stream stream) (cdr exps)))
      ((null exps))
    (funcall function (car exps))))

(defun read-stream (stream)
  (let ((eof (gensym)))
    (do* ((exp nil (read stream nil eof))
          (exps nil (if (not (eql exp eof))
                        (append exps (list exp))
                      exps)))
         ((not (listen stream)) exps))))
  


