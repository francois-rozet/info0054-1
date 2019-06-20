#lang racket

(provide s)
(provide adj)
(provide acc-state?)
(provide (rename-out (h heuristic)))

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

(define s 1)

(define (delta q sigma)
	(if (= (car sigma) q)
		(cdr sigma)
		#f
	)
)

(define (adj q)
	(filter
		(lambda (x) (cdr x))
		(map
			(lambda (sigma)
				(cons sigma (delta q sigma))
			)
			Sigma
		)
	)
)

(define (acc-state? q)
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

(define (h q)
	(h-acc q H)
)

(define (h-acc q H)
	(if	(= (caar H) q)
		(cdar H)
		(h-acc q (cdr H))
	)
)