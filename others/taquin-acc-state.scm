(define (interval a b)
	(if (> a b)
		'()
		(cons
			a
			(interval (+ a 1) b)
		)
	)
)

(define (taquin-acc-state N)
	(map
		(lambda (x) (interval (* (- x 1) N) (- (* x N) 1) ))
		(interval 1 N)
	)
)