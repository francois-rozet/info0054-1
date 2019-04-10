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

;; PUZZLE

;; Sigma est l'alphabet des coups possibles.
(define Sigma '(u d l r))

;; Pour la liste de N listes l représentant un taquin selon la représentation de l'énoncé,
;; taquin-make-state retourne l dans laquelle chaque numéro a été diminué de une unité et l'élément 'x été remplacé par N^2 - 1.
(define (taquin-make-state l)
	(taquin-make-state* (length l) l)
)

;; N.B. Pour la suite, chaque état de taquin sera supposé sous cette représentation.

;; Pour la liste ou liste de listes de nombres l et le nombre N,
;; taquin-make-state* retourne l dans laquelle chaque numéro a été diminué de une unité et l'éventuel élément 'x été remplacé par N^2 - 1.
(define (taquin-make-state* N l)
	(cond
		((null? l) '())
		((list? (car l))
			(cons
				(taquin-make-state* N (car l))
				(taquin-make-state* N (cdr l))
			)
		)
		(else
			(cons
				(if (equal? (car l) 'x)
					(- (* N N) 1)
					(- (car l) 1)
				)
				(taquin-make-state* N (cdr l))
			)
		)
	)
)

;; Pour le nombre N2-1, la liste de listes q et le coup sigma appartenant à Sigma,
;; taquin-delta retourne q dans lequel l'élément N2-1 a été déplacé selon le coup sigma.
;;
;; Si N2-1 n'existe pas dans q, si sigma est impossible ou si sigma n'appartient pas à Sigma, taquin-delta retourne #f.
;;
;; En particulier si q est l'état d'un taquin, N2-1 le numéro représentant le "trou" du taquin et sigma un coup valide,
;; taquin-delta retourne l'état du taquin après le coup.
(define (taquin-delta N2-1 q sigma)
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
					(let ((l (taquin-delta-l N2-1 (car q))))
						(if l
							(cons l (cdr q))
							#f
						)
					)
				)
				((equal? sigma 'r)
					(let ((l (taquin-delta-r N2-1 (car q))))
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
			(let ((q* (taquin-delta N2-1 (cdr q) sigma)))
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
;; taquin-delta-r retourne q dans laquelle l'élément N2-1 a été échangé avec l'élément à sa droite.
;;
;; Si N2-1 n'existe pas dans q ou si N2-1 est l'élément final de q, taquin-delta-r retourne #f.
(define (taquin-delta-r N2-1 q)
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
			(let ((q* (taquin-delta-r N2-1 (cdr q))))
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
;; taquin-delta-l retourne q dans laquelle l'élément N2-1 a été échangé avec l'élément à sa gauche.
;;
;; Si N2-1 n'existe pas dans q ou si N2-1 est l'élément initial de q, taquin-delta-l retourne #f.
(define (taquin-delta-l N2-1 q)
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
			(let ((q* (taquin-delta-l N2-1 (cdr q))))
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

;; taquin-adj est la fonction d'adjacence.
(define (taquin-adj q)
	(filter
		(lambda (x) (cdr x))
		(map
			(let ((N (length q)))
				(lambda (sigma) (cons sigma (taquin-delta (- (* N N) 1) q sigma)))
			)
			Sigma
		)
	)
)

;; taquin-acc-state? est le prédicat d'acceptation.
(define (taquin-acc-state? q)
	(taquin-acc-state?* q 0)
)

;; Pour la liste de listes q et le nombre n,
;; taquin-acc-state?* retourne 
;; #t si (apply append q) est une suite arithmétique de raison 1 et d'élément initial n
;; #f sinon
;;
;; En particulier, si q est un état de taquin et n est 0,
;; taquin-acc-state?* retourne #t si q est l'état accepteur, #f sinon.
(define (taquin-acc-state?* q n)
	(cond
		((null? q) #t)
		((null? (car q))
			(taquin-acc-state?* (cdr q) n)
		)
		((= (caar q) n)
			(taquin-acc-state?* (cons (cdar q) (cdr q)) (+ n 1))
		)
		(else #f)
	)
)

;; taquin-s est l'état de taquin N * N de l'énoncé.
(define taquin-s (taquin-make-state '((2 3 6) (1 x 5) (7 8 4))))

;; SOLVE

;; ((rp-solve taquin-s taquin-adj taquin-acc-state?))