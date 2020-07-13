;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 1
;       Chocs axés dans une enceinte carrée

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

(require (lib "world.ss" "htdp"))



(define SIZE-FEN 300)
(define MI-FENETRE (/ SIZE-FEN 2.0))
(define SIZE (* 0.9 SIZE-FEN))    


;**************************************
;Conditions initiales

(define MASSE1 2) ;choix arbitraire
(define MASSE2 2) ;choix arbitraire
(define VITESSE1 6) ;choix arbitraire
(define VITESSE2 2) ;choix arbitraire
(define POSITION1 (* 3 (/ SIZE-FEN 10.))) ;choix arbitraire
(define POSITION2 (* 7 (/ SIZE-FEN 10.)))  ;choix arbitraire
;**************************************


(define-struct masse (x y vx vy m rayon couleur))    ; (x,y) la position, (vx,vy) la vitesse, m la masse

(define (balle r c)
  (circle r 'solid c))       ;renvoie l'image d'une sphere pleine de rayon r de couleur c

(define BACKGROUND (let* ((rext (+ SIZE (/ (- SIZE-FEN SIZE) 5.0))))   ; le fond d'ecran (la surface d'etude ne fait pas la taille dela fenetre)
                     (place-image (rectangle SIZE SIZE 'solid "yellow") MI-FENETRE MI-FENETRE 
                                  (place-image (rectangle rext rext 'solid "orange") MI-FENETRE MI-FENETRE 
                                               (empty-scene SIZE-FEN SIZE-FEN)))))

(define (next masse)     ;gere les collisions aux bord de la surface, independante de la seconde masse en mouvement
  (let ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse)) (m (masse-m masse)) (r (masse-rayon masse)) (c (masse-couleur masse)))
    (let ((xs (+ x vx)) (ys (+ y vy)) (EPS (* 0.5 0.1 SIZE-FEN)))
      (cond ((>= (+ xs r) (+ EPS SIZE))    ; collision mur droit
             (cond ((>= (+ ys r) (+ EPS SIZE))                ; collision simultanee mur droit et mur bas
                    (make-masse (+ EPS SIZE (- r)) (+ EPS SIZE (- r)) (- vx) (- vy) m r c))
                   ((<= (- ys r) EPS)                ; collision simultanee mur droit et mur haut
                    (make-masse (+ EPS SIZE (- r)) (+ EPS r) (- vx) (- vy) m r c))
                   (else               ; collision mur droit seul
                    (make-masse (+ EPS SIZE (- r)) ys (- vx) vy m r c))))
            ((<= (- xs r) EPS)       ; collision mur gauche
             (cond ((>= (+ ys r) (+ EPS SIZE))              ; collision simultanee mur gauche et mur bas
                    (make-masse (+ r EPS) (+ SIZE EPS (- r)) (- vx) (- vy) m r c))
                   ((<= (- ys r) EPS)           ; collision simultanee mur gauche et mur haut
                    (make-masse (+ r EPS) (+ r EPS) (- vx) (- vy) m r c))
                   (else               ; collision mur gauche seul
                    (make-masse (+ r EPS) ys (- vx) vy m r c))))
            ((>= (+ ys r) (+ EPS SIZE))          ; collision mur bas seul
             (make-masse xs (+ EPS SIZE (- r)) vx (- vy) m r c))
            ((<= (- ys r) EPS)                       ; collision mur haut seul
             (make-masse xs (+ EPS r) vx (- vy) m r c))
            (else                          ; aucune collision
             (make-masse xs ys vx vy m r c))))))

(define (distance monde)  ; il y a choc si la distance entre les deux masses est inferieur ou egale a la somme des rayons des masses
  (> (abs (- (masse-x (car monde)) (masse-x (cadr monde))))
     (+ (masse-rayon (car monde)) (masse-rayon (cadr monde)))))

(define (chocx monde)   ; on gere la collision des deux masses, et on calcul les nouvelles vitesses v1s v2s
  (let* ((x1 (masse-x (car monde))) (x2 (masse-x (cadr monde))) 
                                    (y1 (masse-y (car monde))) (y2 (masse-y (cadr monde))) 
                                    (v1 (masse-vx (car monde))) (v2 (masse-vx (cadr monde))) 
                                    (vya (masse-vy (car monde))) (vyd (masse-vy (cadr monde))) ; non modifiees car choc axial selon (Ox)
                                    (m1 (masse-m (car monde))) (m2 (masse-m (cadr monde)))
                                    (v1s (exact->inexact (+ (* v1 (/ (- m1 m2) (+ m1 m2))) (* v2 (/ (* 2 m2) (+ m1 m2)))))) ;nouvelles vitesses
                                    (v2s (exact->inexact (+ (* v2 (/ (- m2 m1) (+ m1 m2))) (* v1 (/ (* 2 m1) (+ m1 m2)))))) ;selon (Ox)
                                    (r1 (masse-rayon (car monde))) (c1 (masse-couleur (car monde)))
                                    (r2 (masse-rayon (cadr monde))) (c2 (masse-couleur (cadr monde))))
    (list (make-masse x1 y1 v1s vya m1 r1 c1)            ;creation d'un nouveau monde (liste de deux masses)
          (make-masse x2 y2 v2s vyd m2 r2 c2))))

(define verifie-em 
  (lambda (monde) 
    (printf "EM = ~a\n" (+ (* (masse-m (car monde)) (sqr (masse-vx (car monde)))) (* (masse-m (cadr monde)) (sqr (masse-vx (cadr monde))))))))

(define (next-world monde)
  ;  (verifie-em monde)
  (if (distance monde)   ; s'il n'y a pas de choc entre les masses
      (list (next (car monde)) (next (cadr monde))) ;on les traite de maniere independante 
      (let ((p (chocx monde))) (list (next (car p)) (next (cadr p)) 2)))) ;sinon, on calcul prealablement leur nouvelle vitesse

(define (play masse img)
  (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img)) ;ajoute a img l'image de la masse

(define (play-world monde) 
  (play (cadr monde) (play (car monde) BACKGROUND)))

(define (rayon poid)
  (* poid (/ SIZE 20.))) ;choix arbitriaire (NE VERIFIE PAS LES CONDITIONS INITIALES)

(define INIT (list (make-masse POSITION1 MI-FENETRE VITESSE1 0 MASSE1 (rayon MASSE1) "blue") (make-masse POSITION2 MI-FENETRE VITESSE2 0 MASSE2 (rayon MASSE2) "red"))) ;monde initial

(big-bang SIZE-FEN SIZE-FEN 0.02 INIT)
(on-redraw play-world)
(on-tick-event next-world)