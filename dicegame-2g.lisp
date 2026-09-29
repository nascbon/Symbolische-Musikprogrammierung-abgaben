( setf *random-state* ( make-random-state t ) )

( defun rangedice (min-p max-p)
	( + ( random ( + ( - max-p min-p ) 1 )) min-p))





( defun within-p (testn-p min-p max-p)
	( and ( >= testn-p min-p) ( <= testn-p max-p)))





( defun dicegame-2g ( min-p max-p testn-p0 testn-p1 testn-p2 )
	( and
		( within-p testn-p0 min-p max-p )
		( within-p testn-p1 min-p max-p )
		( within-p testn-p2 min-p max-p )))

( print ( dicegame-2g
	3
	8
	( print ( rangedice 0 12 ))
	( print ( rangedice 0 12 ))
	( print ( rangedice 0 12 ))))
