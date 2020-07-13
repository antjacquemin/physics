;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 5
;       Chute libre dans une enceinte carrée

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

(require (lib "world.ss" "htdp"))
(if (not (file-exists? "adt-outils.scm"))
    (error "Le fichier adt-outils.scm est introuvable")
    (load "adt-outils.scm"))

(define SIZE-FEN 300)
(define MI-FENETRE (/ SIZE-FEN 2.0))
(define SIZE (* 0.9 SIZE-FEN))   
(define BOOL #t)

(define (coeff-restitution matiere)  ;pour la simulation, on considerera que sur les parois latterales la collisions est elastique
  (case matiere                      ;mais que la balle et la paroi horizontale sont de type matière
    ((bois) (/ 1 2.0))                 ;coefficient de restitution e=Vf/Vi
    ((liege) (/ 5 9.0))
    ((ivoire) (/ 8 9.0))
    ((verre) (/ 15 16.0))
    ((acier) (/ 19 20.0))
    ((elastique) 1)
    (else (error "matiere inconuue" matiere))))


;**************************************
;Conditions initiales

(define MASSE1 2) ;choix arbitraire
(define VITESSE1X 3) ;choix arbitraire
(define VITESSE1Y -3) ;choix arbitraire
(define POSITION1 (* 2 (/ SIZE-FEN 10.))) ;choix arbitraire
(define GRAVITE 0.4)   ;choix arbitraire
(define COEFF (coeff-restitution 'elastique))   ;matiere a choisir parmis bois, liege, ivoire, verre, acier
;**************************************


(define-struct masse (x y vx vy m rayon couleur))    ; (x,y) la position, (vx,vy) la vitesse, m la masse

(define (balle r c)
  (circle r 'solid c))       ;renvoie l'image d'une sphere pleine de rayon r de couleur c

(define BACKGROUND (let* ((rext (+ SIZE (/ (- SIZE-FEN SIZE) 5.0))))   ; le fond d'ecran (la surface d'etude ne fait pas la taille dela fenetre)
                     (place-image (rectangle SIZE SIZE 'solid "yellow") MI-FENETRE MI-FENETRE 
                                  (place-image (rectangle rext rext 'solid "black") MI-FENETRE MI-FENETRE 
                                               (empty-scene SIZE-FEN SIZE-FEN)))))

(define (next masse)     ;gere les collisions aux bord de la surface
  (let ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse)) (m (masse-m masse)) (r (masse-rayon masse)) (c (masse-couleur masse)))
    (let ((xs (+ x vx)) (ys (+ y vy)) (EPS (* 0.5 0.1 SIZE-FEN)))
      (cond ((>= (+ xs r) (+ EPS SIZE))    ; collision mur droit
             (cond ((>= (+ ys r) (+ EPS SIZE))                ; collision simultanee mur droit et mur bas
                    (make-masse (+ EPS SIZE (- r)) (+ EPS SIZE (- r)) (* COEFF (- vx)) (* COEFF (- vy)) m r c))
                   ((<= (- ys r) EPS)                ; collision simultanee mur droit et mur haut
                    (make-masse (+ EPS SIZE (- r)) (+ EPS r) (- vx) (- vy) m r c))
                   (else               ; collision mur droit seul
                    (make-masse (+ EPS SIZE (- r)) ys (- vx) vy m r c))))
            ((<= (- xs r) EPS)       ; collision mur gauche
             (cond ((>= (+ ys r) (+ EPS SIZE))              ; collision simultanee mur gauche et mur bas
                    (make-masse (+ r EPS) (+ SIZE EPS (- r)) (* COEFF (- vx)) (* COEFF (- vy)) m r c))
                   ((<= (- ys r) EPS)           ; collision simultanee mur gauche et mur haut
                    (make-masse (+ r EPS) (+ r EPS) (- vx) (- vy) m r c))
                   (else               ; collision mur gauche seul
                    (make-masse (+ r EPS) ys (- vx) vy m r c))))
            ((>= (+ ys r) (+ EPS SIZE))          ; collision mur bas seul
             (make-masse xs (+ EPS SIZE (- r)) (* COEFF vx) (* COEFF (- vy)) m r c))
            ((<= (- ys r) EPS)                       ; collision mur haut seul
             (make-masse xs (+ EPS r) vx (- vy) m r c))
            (else                          ; aucune collision
             (make-masse xs ys vx vy m r c))))))

(define (traitement-gravite monde g)
  (make-masse (masse-x monde) (masse-y monde) (masse-vx monde)
              (+ g (masse-vy monde)) (masse-m monde) (masse-rayon monde) (masse-couleur monde)))

(define (next-world monde)
  (if (>= (+ (masse-y monde) (masse-rayon monde)) (+ (* 0.5 0.1 SIZE-FEN) SIZE))
      (if BOOL (begin (set! BOOL #f) monde) (next monde))
      (next (traitement-gravite monde GRAVITE))))

;(define (play masse img)
 ;(place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img)) ;ajoute a img l'image de la masse

(define (play masse image) ;avec table tracante
  (let ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse)))
    (set! image (add-line image x y (+ x vx) (+ y vy) "red"))
    (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) image)))

(define (play-world monde) 
  (play monde BACKGROUND))

(define (rayon poid)
  (* poid (/ SIZE 20.0))) ;choix arbitraire (NE VERIFIE PAS LES CONDITIONS INITIALES)

(define INIT (make-masse POSITION1 (* 0.85 MI-FENETRE) VITESSE1X VITESSE1Y MASSE1 (rayon MASSE1) "blue")) ;monde initial

(big-bang SIZE-FEN SIZE-FEN 1/24 INIT)
(on-redraw play-world)
(on-tick-event next-world)