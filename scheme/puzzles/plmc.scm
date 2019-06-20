#lang racket

(provide s)
(provide adj)
(provide acc-state?)

;; UTILS

(define (xand a b)
	(or (and a b) (and (not a) (not b)))
)

(define (xor a b)
	(and (or a b) (or (not a) (not b)))
)

;; PUZZLE

(define Sigma '(p l m c))

(define F
	(list
		(list #f #f #f #f)
	)
)

(define s
	(list #t #t #t #t)
)

(define p car)
(define p! cdr)
(define l cadr)
(define l! cddr)
(define m caddr)
(define m! cdddr)
(define c cadddr)
(define c! cddddr)

(define (delta q sigma)
	(cons
		(not (p q))
		(if (equal? sigma 'l)
			(cons (not (l q)) (l! q))
			(cons (l q)
				(if (equal? sigma 'm)
					(cons (not (m q)) (m! q))
					(cons (m q)
						(cons
							(if (equal? sigma 'c)
								(not (c q))
								(c q)
							)
							(c! q)
						)
					)
				)
			)
		)
	)
)

(define (valid? q)
	(not
		(and
			(xor (p q) (m q))
			(or
				(xand (m q) (l q))
				(xand (m q) (c q))
			)
		)
	)
)

(define (adj q)
	(filter
		(lambda (x) (valid? (cdr x)))
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