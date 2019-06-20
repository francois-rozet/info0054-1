#lang racket

(provide s)
(provide adj)
(provide acc-state?)
(provide (rename-out (h2 heuristic)))

;; UTILS

;; Pour les listes l et r de même taille n et le prédicat p? à deux paramètres,
;; swap-if retourne la liste des deux listes l* et r* telles que
;; l*_i = l_i et r*_i = r_i lorsque (p? l_i r_i) est #f et
;; l*_i = r_i et r*_i = l_i sinon.
(define (swap-if p? l r)
	(let
		((lr
			(map
				(lambda (a b)
					(if (p? a b)
						(cons b a)
						(cons a b)
					)
				)
				l
				r
			)
		))
		(list (map car lr) (map cdr lr))
	)
)

;; Pour deux paires de réels a et b représentant des points du plan,
;; manhattan retourne la distance de Manhattan entre les points a et b.
(define (manhattan a b)
	(+
		(abs (- (car a) (car b)))
		(abs (- (cdr a) (cdr b)))
	)
)

;; Pour le naturel a et le naturel non nul b,
;; div retourne une paire dont le car est le reste de la division de a par b et le cdr en est le quotient.
(define (div a b)
	(cons
		(modulo a b)
		(floor (/ a b))
	)
)

;; PUZZLE

;; Sigma est l'alphabet des coups possibles.
(define Sigma '(u d l r))

;; Pour la liste de N listes l représentant un taquin selon la représentation de l'énoncé,
;; make-state retourne l dans laquelle chaque numéro a été diminué de une unité et l'élément 'x été remplacé par N^2 - 1.
(define (make-state l)
	(make-state* (length l) l)
)

;; N.B. Pour la suite, chaque état de taquin sera supposé sous cette représentation.

;; Pour la liste ou liste de listes de nombres l et le nombre N,
;; make-state* retourne l dans laquelle chaque numéro a été diminué de une unité et l'éventuel élément 'x été remplacé par N^2 - 1.
(define (make-state* N l)
	(cond
		((null? l) '())
		((list? (car l))
			(cons
				(make-state* N (car l))
				(make-state* N (cdr l))
			)
		)
		(else
			(cons
				(if (equal? (car l) 'x)
					(- (* N N) 1)
					(- (car l) 1)
				)
				(make-state* N (cdr l))
			)
		)
	)
)

(define s (make-state '((2 3 6) (1 x 5) (7 8 4))))

;; Pour le nombre N2-1, la liste de listes q et le coup sigma appartenant à Sigma,
;; delta retourne q dans lequel l'élément N2-1 a été déplacé selon le coup sigma.
;;
;; Si N2-1 n'existe pas dans q, si sigma est impossible ou si sigma n'appartient pas à Sigma, delta retourne #f.
;;
;; En particulier si q est l'état d'un taquin, N2-1 le numéro représentant le "trou" du taquin et sigma un coup valide,
;; delta retourne l'état du taquin après le coup.
(define (delta N2-1 q sigma)
	(cond
		((null? q) #f)
		((member N2-1 (car q))
			(cond
				((equal? sigma 'd)
					(if (null? (cdr q))
						#f
						(append
							(swap-if
								(lambda (a b)
									(= a N2-1)
								)
								(car q)
								(cadr q)
							)
							(cddr q)
						)
					)
				)
				((equal? sigma 'l)
					(let ((l (delta-l N2-1 (car q))))
						(if l
							(cons l (cdr q))
							#f
						)
					)
				)
				((equal? sigma 'r)
					(let ((l (delta-r N2-1 (car q))))
						(if l
							(cons l (cdr q))
							#f
						)
					)
				)
				(else #f)
			)
		)
		((null? (cdr q)) #f)
		((and (equal? sigma 'u) (member N2-1 (cadr q)))
			(append
				(swap-if
					(lambda (a b)
						(= b N2-1)
					)
					(car q)
					(cadr q)
				)
				(cddr q)
			)
		)
		(else
			(let ((q* (delta N2-1 (cdr q) sigma)))
				(if q*
					(cons
						(car q)
						q*
					)
					#f
				)
			)
		)
	)
)

;; Pour le nombre N2-1 et la liste q,
;; delta-r retourne q dans laquelle l'élément N2-1 a été échangé avec l'élément à sa droite.
;;
;; Si N2-1 n'existe pas dans q ou si N2-1 est l'élément final de q, delta-r retourne #f.
(define (delta-r N2-1 q)
	(cond
		((null? q) #f)
		((null? (cdr q)) #f)
		((= N2-1 (car q))
			(cons
				(cadr q)
				(cons N2-1 (cddr q))
			)
		)
		(else
			(let ((q* (delta-r N2-1 (cdr q))))
				(if q*
					(cons
						(car q)
						q*
					)
					#f
				)
			)
		)
	)
)

;; Pour le nombre N2-1 et la liste q,
;; delta-l retourne q dans laquelle l'élément N2-1 a été échangé avec l'élément à sa gauche.
;;
;; Si N2-1 n'existe pas dans q ou si N2-1 est l'élément initial de q, delta-l retourne #f.
(define (delta-l N2-1 q)
	(cond
		((null? q) #f)
		((null? (cdr q)) #f)
		((= N2-1 (cadr q))
			(cons
				N2-1
				(cons (car q) (cddr q))
			)
		)
		(else
			(let ((q* (delta-l N2-1 (cdr q))))
				(if q*
					(cons
						(car q)
						q*
					)
					#f
				)
			)
		)
	)
)

;; adj est la fonction d'adjacence.
(define (adj q)
	(filter
		(lambda (x) (cdr x))
		(map
			(let ((N (length q)))
				(lambda (sigma) (cons sigma (delta (- (* N N) 1) q sigma)))
			)
			Sigma
		)
	)
)

;; acc-state? est le prédicat d'acceptation.
(define (acc-state? q)
	(acc-state?* q 0)
)

;; Pour la liste de listes q et le nombre n,
;; acc-state?* retourne 
;; #t si (apply append q) est une suite arithmétique de raison 1 et d'élément initial n
;; #f sinon
;;
;; En particulier, si q est un état de taquin et n est 0,
;; acc-state?* retourne #t si q est l'état accepteur, #f sinon.
(define (acc-state?* q n)
	(cond
		((null? q) #t)
		((null? (car q))
			(acc-state?* (cdr q) n)
		)
		((= (caar q) n)
			(acc-state?* (cons (cdar q) (cdr q)) (+ n 1))
		)
		(else #f)
	)
)

;; HEURISTIQUES

;; Pour un état de taquin q,
;; h1 retourne le nombre de cases mal placées par rapport à l'état accepteur.
(define (h1 q)
	(h1-acc q 0 0)
)

;; Pour une liste de listes q et les naturels n et acc,
;; h1-acc retourne acc augmenté du nombre d'éléments de (apply append q) qui diffèrent 
;; de l'élément correspondant dans une suite arithmétique de raison 1 et d'élément initial n
;;
;; En particulier, si q est un état de taquin et n est 0,
;; h1 retourne le nombre de cases mal placées par rapport à l'état accepteur.
(define (h1-acc q n acc)
	(cond
		((null? q) acc)
		((null? (car q))
			(h1-acc (cdr q) n acc)
		)
		(else
			(h1-acc
				(cons (cdar q) (cdr q))
				(+ n 1)
				(if (= (caar q) n)
					acc
					(+ acc 1)
				)
			)
		)
	)
)

;; Pour un état de taquin q,
;; h2 retourne la somme des distances de Manhattan entre les positions des cases dans q et leur position dans l'état accepteur.
(define (h2 q)
	(h2-acc (length q) q 0 0 0 0)
)

;; Pour un naturel non nul N, une liste de listes q et les naturels n, x, y et acc,
;; h2-acc retourne acc augmenté de la somme de
;; la somme des distances de Manhattan entre (div q_0j N) et (cons (+ x j) y) où q_0j est le jème élément de la première liste de q (j >= 0)
;; et la somme des distances de Manhattan entre (div q_ij N) et (cons j (+ y i)) où q_ij est le jème élément de la ième liste de q (j >=0 et i > 0)
;;
;; En particulier, si q est un état de taquin, N sa longueur, n, x, y et acc sont nuls,
;; h2 retourne la somme des distances de Manhattan entre les positions des cases dans q et leur position dans l'état accepteur.
(define (h2-acc N q n x y acc)
	(cond
		((null? q) acc)
		((null? (car q))
			(h2-acc N (cdr q) n 0 (+ y 1) acc)
		)
		(else
			(h2-acc
				N
				(cons (cdar q) (cdr q))
				(+ n 1)
				(+ x 1)
				y
				(if (= (caar q) n)
					acc
					(+
						acc
						(manhattan
							(div (caar q) N)
							(cons x y)
						)
					)
				)
			)
		)
	)
)