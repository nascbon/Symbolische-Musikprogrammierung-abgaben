(defun arithm-ser (begin end step)
  (mapcar (lambda (i) (+ begin (* i step)))
          (loop for i from 0 below end collect i)))


(arithm-ser 0 10 1)
