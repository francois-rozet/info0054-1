#lang racket

(provide s)
(provide adj)
(provide acc-state?)

;; UTILS

(define (enumerate n)
	(if (zero? n)
		'()
		(cons n (enumerate (- n 1)))
	)
)

(define (n-copy n object)
	(if (zero? n)
		'()
		(cons object (n-copy (- n 1) object))
	)
)

;; PUZZLE

(define (make-sate n m)
	(cons
		(append (enumerate n) '(0))
		(map
			(lambda (x)	(list (- x)))
			(reverse (enumerate (- (max m 2) 1)))
		)
	)
)

(define s (make-sate 4 3))

(define (delta q sigma)
	(delta* q (car sigma) (cdr sigma) 0)
)

(define (delta* q up down mod)
	(if (= mod 2)
		q
		(let*
			(
				(tower (car q))
				(top (car tower))
			)
			(cond
				(
					(= top up)
					(cons
						(cdr tower)
						(delta* (cdr q) up down (+ mod 1))
					)
				)
				(
					(= top down)
					(cons
						(cons up tower)
						(delta* (cdr q) up -1 (+ mod 1))
					)
				)
				(else
					(cons
						tower
						(delta* (cdr q) up down mod)
					)
				)
			)
		)
	)
)

(define (Sigma q)
	(Sigma*
		(sort
			(map car q)
			>
		)
	)
)

(define (Sigma* l)
	(if (or (null? l) (< (car l) 1))
		'()
		(append
			(map
				cons
				(n-copy (length (cdr l)) (car l))
				(cdr l)
			)
			(Sigma* (cdr l))
		)
	)
)

(define (adj q)
	(map
		(lambda (sigma) (cons sigma (delta q sigma)))
		(Sigma q)
	)
)

(define (acc-state? q)
	(let
		((l (sort (map car q) >)))
		(or (zero? (car l)) (zero? (cadr l)))
	)
)