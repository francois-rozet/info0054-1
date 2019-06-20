#lang racket

(provide s)
(provide adj)
(provide acc-state?)

;; UTILS

(define (loop? l)
	(cond
		((null? l) #f)
		((member (car l) (cdr l)) #t)
		(else (loop? (cdr l)))
	)
)

;; PUZZLE

(define Sigma
	'(
		(1 . 6) (1 . 8) (2 . 7) (2 . 9)
		(3 . 8) (4 . 5) (4 . 9) (5 . 4)
		(6 . 1) (6 . 0) (7 . 2) (7 . 8)
		(8 . 1) (8 . 3) (8 . 7) (9 . 2)
		(9 . 4) (0 . 6)
	)
)

(define F
	'(
		(0 1 3 6) (0 6 1 3) (0 6 3 1)
		(3 0 1 6) (3 1 0 6) (3 6 0 1)
		(6 0 1 3) (6 0 3 1) (6 1 0 3)
	)
)

(define s '(1 3 6 0))

(define (delta q sigma)
	(let
		(
			(q*
				(map
					(lambda (x)
						(if (= x (car sigma))
							(cdr sigma)
							x
						)
					)
					q
				)
			)
		)
		(if (or (equal? q* q) (loop? q*))
			#f
			q*
		)
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