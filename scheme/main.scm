#lang racket

(require "solver.scm")
(require "puzzles/taquin.scm")

;; UTILS

(define (n-lazy f n)
	(if (zero? n)
		'()
		(let
			((g (f)))
			(if (null? g)
				'()
				(cons (car g) (n-lazy (cdr g) (- n 1)))
			)
		)
	)
)

;; SOLVE

(n-lazy (rp-solve s adj acc-state?) 1)
;; (n-lazy (rp-solve-heuristic s adj acc-state? heuristic) 10)