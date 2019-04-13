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

;; Pour la procédure p à un argument et la liste l,
;; map-reverse retourne (reverse (map p l)).
(define (map-reverse p l)
	(map-reverse-acc p l '())
)

;; Pour la procédure p à un argument, la liste l et la liste acc,
;; map-reverse-acc retourne (append (reverse (map p l)) acc).
;;
;; En particulier, si acc est la liste vide,
;; map-reverse-acc retourne (reverse (map p l)).
(define (map-reverse-acc p l acc)
	(if (null? l)
		acc
		(map-reverse-acc p (cdr l) (cons (p (car l)) acc))
	)
)

;; Pour les paires l et r dont les car sont des nombres,
;; car<? retourne #t si (car l) est plus petit que (car r), #f sinon.
(define (car<? l r)
	(< (car l) (car r))
)

;; Pour les listes l et r,
;; shorter? retourne #t si l est plus courte que r, #f sinon.
(define (shorter? l r)
	(< (length l) (length r))
)

;; Pour les paires l et r dont les car sont des listes,
;; car-shorter? retourne #t si (car l) est plus courte que (car r), #f sinon.
(define (car-shorter? l r)
	(shorter? (car l) (car r))
)

;; TERMINOLOGIE

;;
;; Un chemin est une liste de paires (sigma_i . q_i) représentant, de droite à gauche, une suite de coups.
;;
;; Un chemin est valide s'il ne passe pas deux fois par le même état.
;;
;; Un chemin est dit non-redondant si l'apparition de chacun de ses q_i a coïncidé avec la première apparition de cet état.
;; En d'autres mots, un chemin non-redondant est un des chemins les plus courts pour accéder à ses états.
;;
;; Un chemin est dit presque non-redondant si son chemin parent (sans le dernier coup) est non-redondant.
;; Un chemin non-redondant est presque non-redondant
;;
;; Une clé est un chemin presque non-redondant dont le dernier état est accepteur.
;;
;; Un mot est une liste de sigma_i représentant, de gauche à droite, une suite de coups pour atteindre un état accepteur.

;; visited est toujours un set d'états visités.
;;
;; queue est toujours une liste de paires dont le car est un état q
;; et le cdr une liste de chemins presque non-redondants dont le dernier état est q.
;; Il n'y a pas deux paires avec le même car.

;; SOLVER

;; Pour une paire pointée p,
;; state retourne (cdr p).
;;
;; En particulier si p est du type (sigma . q),
;; state retourne q.
(define state cdr)

;; Pour une paire pointée p,
;; state retourne (car p).
;;
;; En particulier si p est du type (sigma . q),
;; state retourne sigma.
(define sigma car)

;; Pour un chemin path et une fonction d'adjacence adj,
;; child retourne la liste des chemins valides enfants de path.
;;
;; En particulier si path est non-redondant,
;; child retourne une liste de chemins presque non-redondants.
(define (child adj path)
	(map
		(lambda (x) (cons x path))
		(filter
			(lambda (y) (not (member (state y) (map state path))))
			(adj (state (car path)))
		)
	)
)

;; Pour un état q et la liste queue,
;; pop retourne le cdr de la paire de queue dont le car est q.
;;
;; Si cette paire n'existe pas,
;; pop retourne '().
(define (pop queue q)
	(cond
		((null? queue) '())
		((equal? (caar queue) q) (cdar queue))
		(else (pop (cdr queue) q))
	)
)

;; Pour un chemin path presque non-redondant et la liste queue,
;; push retourne queue dans laquelle a été ajouté path.
;;
;; C.-à-d. que push ajoute path à la fin du cdr de la paire de queue dont le car est le dernier état de path,
;; ou, si elle n'existe pas, crée cette paire avec, comme cdr, la liste comprenant path.
(define (push queue path)
	(cond
		((null? queue) (list (cons (state (car path)) (list path))))
		((equal? (caar queue) (state (car path)))
			(append
				(cdr queue)
				(list (cons (caar queue) (cons path (cdar queue))))
			)
		)
		(else
			(cons
				(car queue)
				(push (cdr queue) path)
			)
		)
	)
)

;; Pour un chemin path, la fin de mot word-end et la liste queue,
;; queue2words retourne une liste de toutes les paires dont
;; le car est un mot commençant par un mot associé à un chemin path* de queue, relié à word-end par un sous-mot terminal sub-word associé à path
;; et
;; le cdr est une procédure de paramètre queue* appelant queue2words pour le chemin (cdr path*), la fin de mot (cons (sigma (car path*)) (append sub-word word-end)) et queue*.
;;
;; Le chemin path* doit se terminer par un état q_n-k contenu dans path et sub-word commence à partir de l'état suivant q_n-k dans path.
;;
;; Par exemple, si
;; path est '((sigma_n . q_n) (sigma_n-1 . q_n-1) ... (sigma_n-k . q_n-k) ... (sigma_0 . q_0)' avec 0 <= k <= n,
;; word-end est '(sigma_m sigma_m+1 ... sigma_end) et
;; path* est '((sigma*_n-k . q_n-k) (sigma*_n-k-1 . q*_n-k-1) ... (sigma*_0 . q*_0)),
;; la paire retournée est
;;	(cons
;;		'(sigma*_0 sigma*_1 ... sigma*_n-k sigma_n-k+1 ... sigma_n sigma_m ... sigma_end)
;;		(lambda (queue*)
;;			(queue2words
;;				queue*
;;				'((sigma*_n-k-1 . q*_n-k-1) ... (sigma*_0 . q*_0))
;;				'(sigma*_n-k sigma_n-k+1 ... sigma_n sigma_m ... sigma_end)
;;			)
;;		)
;;	)
(define (queue2words queue path word-end)
	(if (null? path)
		'()
		(merge
			(map
				(lambda (path*)
					(cons
						(append (map-reverse sigma path*) word-end)
						(lambda (queue*) (queue2words queue* (cdr path*) (cons (sigma (car path*)) word-end)))
					)
				)
				(pop queue (state (car path)))
			)
			(queue2words queue (cdr path) (cons (sigma (car path)) word-end))
			car-shorter?
		)
	)
)

;; Pour une clé key et un chemin path,
;; key2word retourne le sous-mot terminal de key (renversé) permettant d'accéder au dernier état de key depuis celui de path.
;;
;; Si ce dernier n'existe pas, c.-à-d. si key ne passe pas par le dernier état de path,
;; key2word retourne #f.
(define (key2word key path)
	(cond
		((null? key) #f)
		((equal? (state (car key)) (state (car path))) '())
		(else
			(let ((rev-word-end (key2word (cdr key) path)))
				(if rev-word-end
					(cons
						(sigma (car key))
						rev-word-end
					)
					#f
				)
			)
		)
	)
)

;; Pour une list de clés keys et un chemin path,
;; keys2words retourne une liste des paires dont 
;; le car est un mot commençant par le mot associé à path et se terminant par le sous-mot word-end retourné, après renversement, par key2word pour une clé key et path
;; et
;; le cdr est une procédure de paramètre queue* appelant queue2words pour le chemin (cdr path), la fin de mot (cons (sigma (car path)) word-end) et queue*.
;;
;; Si word-end est #f, la paire n'est pas ajoutée à la liste.
(define (keys2words keys path)
	(if (null? keys)
		'()
		(let
			((rev-word-end (key2word (car keys) path)))
			(if rev-word-end
				(let ((word-end (reverse rev-word-end)))
					(cons
						(cons
							(append (map-reverse sigma path) word-end)
							(lambda (queue*) (queue2words queue* (cdr path) (cons (sigma (car path)) word-end)))
						)
						(keys2words (cdr keys) path)
					)
				)
				(keys2words (cdr keys) path)
			)
		)
	)
)

;; Pour la fonction d'acceptation acc-state?, le set visited, la liste queue
;; et la liste de chemins presque non-redondants paths,
;; update retourne
;; (cons visited* (cons queue* (cons paths* (cons keys queueds))))
;; où
;; visited* est visited dans lequel ont été rajoutés les derniers états des chemins de paths,
;; paths* la liste des chemins non-redondants de paths gardés pour exploration,
;; queue* est queue dans laquelle ont été rajoutés les chemins de paths qui ne sont pas gardés pour exploration,
;; keys est la liste des clés de paths et
;; queueds est la liste des chemins de paths qui ne sont pas gardés pour exploration et qui ne sont pas des clés.
;;
;; N.B. Il est possible que plusieurs chemins non-redondants de paths aient le même dernier état.
;; Dans ce cas, un seul est gardé : le premier dans paths.
(define (update acc-state? visited queue paths)
	(update-acc acc-state? visited queue paths '() '() '())
)

;; Pour la fonction d'acceptation acc-state?, un set provisoire visited, la liste queue provisoire, 
;; une liste de chemins presque non-redondants paths, une liste provisoire de chemins non-redondants gardés pour exploration paths*,
;; une liste provisoire de clés keys et
;; queueds une liste provisoire de chemins qui ne sont pas gardés pour exploration et qui ne sont pas des clés,
;; update-acc retourne
;; (cons visited* (cons queue* (cons paths** (cons keys* queueds*))))
;; où
;; visited* est visited dans lequel ont été rajoutés les derniers états des chemins de paths,
;; paths** est paths* dans laquelle ont été rajoutés les chemins non-redondants de paths gardés pour exploration,
;; queue* est queue dans laquelle ont été rajoutés les chemins de paths qui ne sont pas gardés pour exploration,
;; keys* est keys dans laquelle ont été rajoutées les clées de paths,
;; queueds* est queueds dans laquelle ont été rajoutés les chemins de paths qui ne sont pas gardés pour exploration et qui ne sont pas des clés.
;;
;; N.B. Il est possible que plusieurs chemins non-redondants de paths aient le même dernier état n'étant pas dans visited.
;; Dans ce cas, un seul est gardé : le premier dans paths.
(define (update-acc acc-state? visited queue paths paths* keys queueds)
	(if (null? paths)
		(cons visited (cons queue (cons paths* (cons keys queueds))))
		(let*
			(
				(path (car paths))
				(q (state (car path)))
			)
			(if (set-member? visited q)
				(update-acc
					acc-state?
					visited
					(push queue path)
					(cdr paths)
					paths*
					(if (acc-state? q)
						(cons path keys)
						keys
					)
					(if (acc-state? q)
						queueds
						(cons path queueds)
					)
				)
				(update-acc
					acc-state?
					(set-add visited q)
					queue
					(cdr paths)
					(cons path paths*)
					(if (acc-state? q)
						(cons path keys)
						keys
					)
					queueds
				)
			)
		)
	)
)

;; Pour les fonctions d'adjacence et d'acceptation adj et acc-state?, le set visited, la liste queue,
;; la liste de chemins presque non-redondants paths, la liste des clés trouvées keys,
;; la liste des paires (mot . queue2words) words et le nombre de coups joués n,
;; fun est un itérateur paresseux d'un sous-ensemble du sous-language des mots sans cycle du puzzle.
;; Ces mots sont donnés par ordre croissant de longueur.
(define (fun adj acc-state? visited queue paths keys words n)
	(cond
		((and (null? paths) (null? words)) '())
		((or (null? paths) (and (not (null? words)) (<= (length (caar words)) n)))
			(cons
				(caar words)
				(lambda ()
					(fun
						adj
						acc-state?
						visited
						queue
						paths
						keys
						(merge
							((cdar words) queue)
							(cdr words)
							car-shorter?
						)
						n
					)
				)
			)
		)
		(else
			(let*
				(
					(temp (update acc-state? visited queue paths))
					(visited* (car temp))
					(queue* (cadr temp))
					(paths* (caddr temp))
					(keys+ (cadddr temp))
					(queueds (cddddr temp))
					(words*
						(merge
							(merge
								(map
									(lambda (key)
										(cons
											(map-reverse sigma key)
											(lambda (queue) (queue2words queue (cdr key) (list (sigma (car key)))))
										)
									)
									keys+
								)
								(sort
									(apply append
										(map
											(lambda (queued) (keys2words keys queued))
											queueds
										)
									)
									car-shorter?
								)
								car-shorter?
							)
							words
							car-shorter?
						)
					)
				)
				(fun
					adj
					acc-state?
					visited*
					queue*
					(append-map (lambda (path) (child adj path)) paths*)
					(append keys+ keys)
					words*
					(+ n 1)
				)
			)
		)
	)
)

(define (rp-solve s adj acc-state?)
	(lambda () (fun adj acc-state? (set) '() (list (list (cons '() s))) '() '() 0))
)

;; SOLVER-HEURISTIQUE

;; Pour les fonctions d'adjacence, d'acceptation et heuristique, adj, acc-state? et heuristic
;; et la liste de paires ((+ h l) . (l . path)), triées par car croissant, où path est un chemin,
;; h l'heuristique évaluée au dernier état de ce chemin et l sa longueur,
;; fun-heuristic est un itérateur paresseux d'un sous-ensemble du sous-language des mots sans cycle du puzzle.
;; Ces mots sont donnés dans l'ordre de l'heuristique.
(define (fun-heuristic adj acc-state? heuristic p-queue)
	(cond
		((null? p-queue) '())
		((acc-state? (state (caddar p-queue)))
			(cons
				(map-reverse sigma (cddar p-queue))
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
									(+ (heuristic (state (car path))) (+ (cadar p-queue) 1))
									(cons
										(+ (cadar p-queue) 1)
										path
									)
								)
							)
							(child adj (cddar p-queue))
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
					(cons
						0
						(list (cons '() s))
					)
				)
			)
		)
	)
)