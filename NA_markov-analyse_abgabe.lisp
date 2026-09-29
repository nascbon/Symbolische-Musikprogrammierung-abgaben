(defun markov-analysis (sequence)
  (let* ((data-space (remove-duplicates sequence :test #'equal))
         (size (length data-space))
         (transition-counts (make-hash-table :test #'equal)))
    
    (loop for a on sequence while (rest a) do
      (let ((from (first a))
            (to (second a)))
        (let ((inner (gethash from transition-counts)))
          (unless inner
            (setf inner (make-hash-table :test #'equal))
            (setf (gethash from transition-counts) inner))
          (incf (gethash to inner 0)))))
   
    (let ((matrix
           (mapcar (lambda (from)
                     (let ((row (make-list size :initial-element 0.0))
                           (inner (gethash from transition-counts)))
                       (when inner
                         (let ((total (reduce #'+ (loop for x being the hash-values of inner collect x))))
                           (loop for to in data-space
                                 for i from 0
                                 do (setf (nth i row)
                                          (if (> total 0)
                                              (/ (gethash to inner 0) total)
                                              0.0)))))
                       row))
                   data-space)))
      (list data-space matrix))))
