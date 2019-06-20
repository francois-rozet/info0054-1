#lang racket

(provide s)
(provide adj)
(provide acc-state?)

;; UTILS

;; PUZZLE

(define Sigma '(u d l r))

(define (make-state p E)
	(cons p (make-state* E))
)

(define (make-state* E)
	(if (null? E)
		'()
		(cons
			(if (list? (car E))
				(make-state* (car E))
				(not (zero? (car E)))
			)
			(make-state* (cdr E))
		)
	)
)

(define s (make-state '(2 . 1) '((1 0 0) (0 0 0) (1 0 1))))

(define (delta q sigma)
	(let*
		(
			(p (car q))
			(x (car p))
			(y (cdr p))
			(E (cdr q))
			(N (length E))
		)
		(cond
			(
				(equal? sigma 'u)
				(if (zero? x)
					#f
					(cons (cons (- x 1) y) (delta* (- x 1) y E))
				)
			)
			(
				(equal? sigma 'd)
				(if (= x (- N 1))
					#f
					(cons (cons (+ x 1) y) (delta* (+ x 1) y E))
				)
			)
			(
				(equal? sigma 'l)
				(if (zero? y)
					#f
					(cons (cons x (- y 1)) (delta* x (- y 1) E))
				)
			)
			(
				(equal? sigma 'r)
				(if (= y (- N 1))
					#f
					(cons (cons x (+ y 1)) (delta* x (+ y 1) E))
				)
			)
			(else #f)
		)
	)
)

(define (delta* x y E)
	(cond 
		((null? E) '())
		(
			(list? (car E))
			(cons
				(delta* x y (car E))
				(delta* (- x 1) y (cdr E))
			)
		)
		(else
			(cons
				(if (< (+ (abs x) (abs y)) 2)
					(not (car E))
					(car E)
				)
				(delta* x (- y 1) (cdr E))
			)
		)
	)
)

(define (adj q)
	(filter
		(lambda (x) (cdr x))
		(map
			(lambda (sigma) (cons sigma (delta q sigma)))
			Sigma
		)
	)
)

(define (acc-state? q)
	(acc-state?* (cdr q))
)

(define (acc-state?* E)
	(if (null? E)
		#t
		(and
			(if (list? (car E))
				(acc-state?* (car E))
				(car E)
			)
			(acc-state?* (cdr E))
		)
	)
)