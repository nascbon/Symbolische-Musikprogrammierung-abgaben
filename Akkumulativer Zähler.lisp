
(defparameter *init* (loop for i from 0 to 11 collect (cons i 0)))


(defun mc-to-pitch (mc)
  (mod (floor mc) 12))


(defun accum-notes-counter (note-list)
  (dolist (note note-list)
    (let* ((pc (mc-to-pitch note))
           (entry (assoc pc *init*)))
      (when entry
        (rplacd entry (1+ (cdr entry))))))
  *init*)


(accum-notes-counter '(20 37 26 38 47 56 15 28 15 57 68 36 57 86 73 54 36))



