;; PUZZLE

(define Sigma '(1 2))

(define F '(6))

(define s 0)

(define (delta q sigma)
	(min (+ q sigma) 10)
)

(define (adj q)
	(map
		(lambda (sigma) (cons sigma (delta q sigma)))
		Sigma
	)
)

(define (acc-state? q)
	(member q F)
)

;; SOLVE

;; ((rp-solve s adj acc-state?))