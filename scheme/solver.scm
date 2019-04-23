#lang racket

(provide rp-solve)
(provide rp-solve-heuristic)

;; UTILS

;; Pour le comparateur comp, l'élément x et la liste l triée selon comp,
;; insert retourne la liste triée l où a été inséré x.
(define (insert x l comp)
	(cond
		((null? l) (list x))
		((comp x (car l)) (cons x l))
		(else
			(cons
				(car l)
				(insert x (cdr l) comp)
			)
		)
	)
)

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

;; Pour le prédicat unaire p? et la liste l,
;; divide-filter retourne la paire dont
;; le car est la liste des éléments de l pour lesquels p? est #t et
;; le cdr la liste des autres éléments de l.
(define (divide-filter p? l)
	(divide-filter-acc p? l '() '())
)

;; Pour le prédicat unaire p? et les listes l, acc-t et acc-f,
;; divide-filter-acc retourne la paire dont
;; le car est (append "la liste des éléments de l pour lesquels p? est #t" acc-t) et
;; le cdr (append "la liste des autres éléments de l" acc-f).
(define (divide-filter-acc p? l acc-t acc-f)
	(cond 
		((null? l) (cons acc-t acc-f))
		((p? (car l))
			(divide-filter-acc p? (cdr l) (cons (car l) acc-t) acc-f)
		)
		(else
			(divide-filter-acc p? (cdr l) acc-t (cons (car l) acc-f))
		)
	)
)

;; SOLVER

;; N.B. Un chemin est une paire dont le car est la liste de coups du dernier au premier joués et
;; le cdr une paire dont le car est le dernier état visité et le cdr le set d'états intermédiaires.

;; Pour un chemin path, word retourne la liste de coups joués du premier au dernier.
(define (word path) (reverse (car path)))

;; Pour un chemin, last-state retourne le dernier état visité.
(define last-state cadr)

;; Pour un chemin, states retourne le set d'états intermédiaires.
(define states cddr)

;; Pour un chemin path et une fonction d'adjacence adj,
;; child retourne la liste des chemins valides enfants de path.
(define (child adj path)
	(map
		(lambda (x)
			(cons
				(cons (car x) (car path))
				(cons (cdr x) (set-add (states path) (cadr path)))
			)
		)
		(filter
			(lambda (y) (not (set-member? (states path) (cdr y))))
			(adj (last-state path))
		)
	)
)

;; Pour les chemins l et r,
;; shorter? retourne #t si l est plus court que r, #f sinon.
(define (shorter? l r)
	(<= (length (word l)) (length (word r)))
)

;; Pour les fonctions d'adjacence et d'acceptation adj et acc-state?,
;; la liste de chemins favoris queue,
;; le set d'états présents dans les chemins de queue visited et
;; la liste de chemins laissés de côtés waiting,
;; fun est un itérateur paresseux d'un sous-ensemble du sous-language des mots sans cycle du puzzle.
;; Ces mots sont donnés par ordre croissant de longueur.
(define (fun adj acc-state? visited queue waiting)
	(if (null? queue)
		'()
		(let*
			(
				(path (car queue))
				(q (last-state path))
			)
			(cond
				((acc-state? q)
					(let*
						(
							(temp
								(divide-filter
									(lambda (x) (set-member? (states path) (last-state x)))
									waiting
								)
							)
							(visited* (set-subtract visited (states path)))
							(queue* (merge (sort (car temp) shorter?) (cdr queue) shorter?))
							(waiting* (cdr temp))
						)
						(cons
							(word path)
							(lambda () (fun adj acc-state? visited* queue* waiting*))
						)
					)
				)
				((set-member? visited q)
					(fun adj acc-state? visited (cdr queue) (cons path waiting))
				)
				(else
					(fun adj acc-state? (set-add visited q) (merge (cdr queue) (child adj path) shorter?) waiting)
				)
			)
		)
	)
)

(define (rp-solve s adj acc-state?)
	(lambda ()
		(fun
			adj
			acc-state?
			(set)
			(list (cons '() (cons s (set))))
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
		((acc-state? (last-state (cdar p-queue)))
			(cons
				(word (cdar p-queue))
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
							(lambda (path)
								(cons
									(heuristic (last-state path))
									path
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
					(cons '() (cons s (set)))
				)
			)
		)
	)
)