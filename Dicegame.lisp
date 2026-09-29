
(defun dice1()
  (+ 1 (random 6))
  )

(dice1)

(defun dice2()
  (+ 1 (random 6))
  )

(dice2)

(defun snake-eyes-p(dice1 dice2)
  (and (= dice1 1) (= dice2 1)))
  


(snake-eyes-p (print (dice1)) (print (dice2)))

(defun double-sixes-p(dice1 dice2)
  (and (= dice1 6) (= dice2 6)))


(defun dicegame ()
  (or
    (snake-eyes-p (dice1) (dice2))
    (double-sixes-p (dice1) (dice2))
    )
  )

(dicegame)
