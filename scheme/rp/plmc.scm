;; PUZZLE

(define Sigma
	'(
		(1 . 2) (1 . 3) (1 . 4)
        (2 . 5) (3 . 5) (3 . 6)
        (4 . 7) (5 . 8) (6 . 8)
        (7 . 8)
	)
)

(define F
	'(8)
)

(define plmc-s 1)

(define (plmc-delta q sigma)
	(if (= (car sigma) q)
		(cdr sigma)
		#f
	)
)

(define (plmc-adj q)
	(filter
		(lambda (x) (cdr x))
		(map
			(lambda (sigma)
				(cons sigma (plmc-delta q sigma))
			)
			Sigma
		)
	)
)

(define (plmc-acc-state? q)
	(member	q F)
)

;; HEURISTIQUE

(define H
	'(
		(1 . 10) (2 . 4) (3 . 3)
		(4 . 5) (5 . 1) (6 . 6)
		(7 . 2) (8 . 0)
	)
)

(define (plmc-h q)
	(plmc-h-acc q H)
)

(define (plmc-h-acc q H)
	(if	(= (caar H) q)
		(cdar H)
		(plmc-h-acc q (cdr H))
	)
)

;; SOLVE

;; ((rp-solve plmc-s plmc-adj plmc-acc-state?))
;; ((rp-solve-heuristic plmc-s plmc-adj plmc-acc-state? plmc-h))