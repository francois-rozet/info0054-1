#lang racket

(provide rp-solve)
(provide rp-solve-heuristic)

;; UTILS

;; Pour le comparateur comp et les listes l et r triées selon comp,
;; merge retourne la liste triée des éléments de l et r réunis.
(define (merge l r comp)
	(cond
		((null? l) r)
		((null? r) l)
		((comp (car l) (car r))
			(cons
				(car l)
				(merge (cdr l) r comp)
			)
		)
		(else
			(cons
				(car r)
				(merge l (cdr r) comp)
			)
		)
	)
)

;; Pour les paires l et r dont les car sont des nombres,
;; car<? retourne #t si (car l) est plus petit que (car r), #f sinon.
(define (car<? l r)
	(< (car l) (car r))
)

;; SOLVER

;; N.B. Un chemin est une structure de la forme (word . states) où
;; word est la liste de coups du dernier au premier joués et
;; states la liste des états visités du dernier au premier.

(define word car)
(define states cdr)
(define last-state cadr)

;; Pour un état s, start initialise un chemin à l'état s.
(define (start s)
	(cons '() (list s))
)

;; Pour un chemin path et une fonction d'adjacence adj,
;; child retourne la liste des chemins valides enfants de path.
(define (child adj path)
	(map
		(lambda (x)
			(cons
				(cons (car x) (word path))
				(cons (cdr x) (states path))
			)
		)
		(filter
			(lambda (y) (not (member (cdr y) (states path))))
			(adj (last-state path))
		)
	)
)

;; Pour les fonctions d'adjacence et d'acceptation adj et acc-state?,
;; une liste de chemins de longueur N stack et
;; une liste de chemins de longueur N + 1 acc,
;; fun est un itérateur paresseux d'un sous-ensemble du sous-language
;; des mots sans cycle du puzzle d'au moins N symboles.
;; Ces mots sont donnés par ordre croissant de longueur.
(define (fun adj acc-state? stack acc)
	(cond 
		((null? stack)
			(if (null? acc)
				'()
				(fun adj acc-state? acc '())
			)
		)
		((acc-state? (last-state (car stack)))
			(cons
				(reverse (word (car stack)))
				(lambda () (fun adj acc-state? (cdr stack) acc))
			)
		)
		(else
			(fun adj acc-state? (cdr stack) (append (child adj (car stack)) acc))
		)
	)
)

(define (rp-solve s adj acc-state?)
	(lambda ()
		(fun
			adj
			acc-state?
			(list (start s))
			'()
		)
	)
)

;; SOLVER-HEURISTIQUE

;; Pour les fonctions d'adjacence, d'acceptation et heuristique, adj, acc-state? et heuristic
;; et la liste de paires (h . path), triées par car croissant, où path est un chemin et
;; h l'heuristique évaluée au dernier état de ce chemin,
;; fun-heuristic est un itérateur paresseux d'un sous-ensemble du sous-language des mots sans cycle du puzzle.
;; Ces mots sont donnés dans l'ordre de l'heuristique.
(define (fun-heuristic adj acc-state? heuristic p-queue)
	(cond
		((null? p-queue) '())
		((zero? (caar p-queue))
			(cons
				(reverse (word (cdar p-queue)))
				(lambda () (fun-heuristic adj acc-state? heuristic (cdr p-queue)))
			)
		)
		(else
			(fun-heuristic
				adj
				acc-state?
				heuristic
				(merge
					(sort
						(map
							(lambda (x)
								(cons
									(heuristic (last-state x))
									x
								)
							)
							(child adj (cdar p-queue))
						)
						car<?
					)
					(cdr p-queue)
					car<?
				)
			)
		)
	)
)

(define (rp-solve-heuristic s adj acc-state? heuristic)
	(lambda ()
		(fun-heuristic
			adj
			acc-state?
			heuristic
			(list
				(cons
					(heuristic s)
					(start s)
				)
			)
		)
	)
)