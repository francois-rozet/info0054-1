#lang racket

(require "solver.scm")
(require "taquin.scm")

;; UTILS

(define n-solutions
	(lambda (iterator n)
		(if (or (zero? n) (null? iterator))
			'()
			(let*
				(
					(pair (iterator))
					(sol (car pair))
					(iterator (cdr pair))
				)
				(cons sol (n-solutions iterator (- n 1)))
			)
		)
	)
)

;; SOLVE

(define taquin-init-state (taquin-make-state '((2 3 6) (1 x 5) (7 8 4))))

;; (n-solutions (rp-solve taquin-init-state taquin-adj-states taquin-acc-state?) 1)
;; (n-solutions (rp-solve-heuristic taquin-init-state taquin-adj-states taquin-acc-state? taquin-heuristic) 10)