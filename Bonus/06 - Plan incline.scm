
;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 6
;         Chute libre sur un plan incliné

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

(require (lib "world.ss" "htdp"))
(if (not (file-exists? "adt-outils.scm"))
    (error "Le fichier adt-outils.scm est introuvable")
    (load "adt-outils.scm"))
(if (not (file-exists? "adt-vecteurs.scm"))
    (error "Le fichier adt-outils.scm est introuvable")
    (load "adt-vecteurs.scm"))

(define SIZE-FEN 300)
(define MI-FENETRE (/ SIZE-FEN 2.0))
(define SIZE (* 0.9 SIZE-FEN))   
(define BOOL #t)

(define (coeff-restitution matiere)  ;pour la simulation, on considerera que sur les parois latterales la collisions est elastique
  (case matiere                      ;mais que la balle et la paroi oblique sont de type matière   ;;PAS ENCORE MIS EN OEUVRE
    ((bois) (/ 1 2.0))                 ;coefficient de restitution e=Vf/Vi
    ((liege) (/ 5 9.0))
    ((ivoire) (/ 8 9.0))
    ((verre) (/ 15 16.0))
    ((acier) (/ 19 20.0))
    ((elastique) 1)
    (else (error "matiere inconuue" matiere))))


;**************************************
;Conditions initiales

(define XBORD (* 0.9 SIZE-FEN)) ;definit la pente
(define YBORD (* 0.6 SIZE-FEN)) ;de la paroi oblique
(define MASSE1 1.5) ;choix arbitraire
(define VITESSE1X 0) ;choix arbitraire
(define VITESSE1Y -6) ;choix arbitraire
(define POSITION1 (* 1.5 (/ SIZE-FEN 10.))) ;choix arbitraire
(define GRAVITE 0.4)   ;choix arbitraire
(define COEFF (coeff-restitution 'elastique))   ;matiere a choisir parmis bois, liege, ivoire, verre, acier
;**************************************


(define-struct masse (x y vx vy m rayon couleur))    ; (x,y) la position, (vx,vy) la vitesse, m la masse
(define XGAUCHE (* 0.05 SIZE-FEN))
(define YDROIT (* 0.95 SIZE-FEN))

(define (balle r c)
  (circle r 'solid c))       ;renvoie l'image d'une sphere pleine de rayon r de couleur c

(define (plan img)
  (add-line img XGAUCHE YBORD XBORD YDROIT "red"))

(define BACKGROUND (let* ((rext (+ SIZE (/ (- SIZE-FEN SIZE) 5.0))))   ; le fond d'ecran (la surface d'etude ne fait pas la taille dela fenetre)
                     (plan (place-image (rectangle SIZE SIZE 'solid "yellow") MI-FENETRE MI-FENETRE 
                                        (place-image (rectangle rext rext 'solid "black") MI-FENETRE MI-FENETRE 
                                                     (empty-scene SIZE-FEN SIZE-FEN))))))

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

(define (play masse img)
  (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img)) ;ajoute a img l'image de la masse

(define (chocx monde) ;gere la colision entre deux masses (dans une liste)
  (let* ((x1 (masse-x (car monde))) (x2 (masse-x (cadr monde))) 
                                    (y1 (masse-y (car monde))) (y2 (masse-y (cadr monde))) 
                                    (vxa (masse-vx (car monde))) (vxd (masse-vx (cadr monde))) 
                                    (vya (masse-vy (car monde))) (vyd (masse-vy (cadr monde)))
                                    (m1 (masse-m (car monde))) (m2 (masse-m (cadr monde)))
                                    (r1 (masse-rayon (car monde))) (r2 (masse-rayon (cadr monde)))
                                    (c1 (masse-couleur (car monde))) (c2 (masse-couleur (cadr monde)))
                                    (vxproj1 (projection 0 0 (- x2 x1) (- y2 y1) vxa vya))
                                    (alpha (angle (+ (* +i (- y2 y1)) (- x2 x1))))
                                    (alpha1 (if (>= (car vxproj1) 0) 
                                                (if (zero? (cdr vxproj1))
                                                    (- alpha)
                                                    (- (angle (+ (* +i (cdr vxproj1)) (car vxproj1)))))
                                                (- pi (angle (+ (* +i (cdr vxproj1)) (car vxproj1))))))
                                    (vxproj2 (projection 0 0 (- x2 x1) (- y2 y1) vxd vyd))
                                    (alpha2 (if (>= (car vxproj2) 0) 
                                                (if (zero? (cdr vxproj2))
                                                    (- alpha)
                                                    (- (angle (+ (* +i (cdr vxproj2)) (car vxproj2)))))
                                                (- pi (angle (+ (* +i (cdr vxproj2)) (car vxproj2))))))
                                    (vyproj1 (cons (- vxa (car vxproj1)) (- vya (cdr vxproj1))))
                                    (vyproj2 (cons (- vxd (car vxproj2)) (- vyd (cdr vxproj2))))
                                    (vxproj1 (car (rotation 0 0 (car vxproj1) (cdr vxproj1) alpha1)))
                                    (vxproj2 (car (rotation 0 0 (car vxproj2) (cdr vxproj2) alpha2)))
                                    (v1s (+ (* vxproj1 (/ (- m1 m2) (+ m1 m2))) (* vxproj2 (/ (* 2 m2) (+ m1 m2)))))
                                    (v2s (+ (* vxproj2 (/ (- m2 m1) (+ m1 m2))) (* vxproj1 (/ (* 2 m1) (+ m1 m2)))))
                                    (v1s (rotation 0 0 v1s 0 (- alpha1)))
                                    (v2s (rotation 0 0 v2s 0 (- alpha2)))
                                    (vxxproj1 (car v1s))
                                    (vxxproj2 (car v2s))
                                    (vxyproj1 (cdr v1s))
                                    (vxyproj2 (cdr v2s))
                                    (vyxproj1 (car vyproj1))
                                    (vyxproj2 (car vyproj2))
                                    (vyyproj1 (cdr vyproj1))
                                    (vyyproj2 (cdr vyproj2))
                                    (vx1 (+ vxxproj1 vyxproj1))
                                    (vy1 (+ vxyproj1 vyyproj1))
                                    (vx2 (+ vxxproj2 vyxproj2))
                                    (vy2 (+ vxyproj2 vyyproj2)))
    (list (make-masse x1 y1 vx1 vy1 m1 r1 c1) 
          (make-masse x2 y2 vx2 vy2 m2 r2 c2))))

(define (next-world monde)
  (let* ((x (masse-x monde)) (y (masse-y monde)) (vx (masse-vx monde)) (vy (masse-vy monde)) (r (masse-rayon monde))
                            (p (projection XGAUCHE YBORD XBORD YDROIT x y)) 
                            (d (norme (+vect (make-vect (+ vx x) (+ vy y)) (*vect -1 (make-vect (car p) (cdr p)))))))
    (if (< d r) ;si la distance entre la boule et la droite est plus grande que le rayon
        (next (car (chocx (list monde (make-masse (car p) (cdr p) 0 0 1e10 0 "red"))))) ;on calcule la nouvelle vitesse de la balle en simulant un choc avec une masse infinie centree au point de projection de la balle sur le plan
        (if (>= (+ (masse-y monde) (masse-rayon monde)) (+ (* 0.5 0.1 SIZE-FEN) SIZE)) ;sera utile pour le choc inelastique
            (if BOOL (begin (set! BOOL #f) monde) (next monde))
            (next (traitement-gravite monde GRAVITE))))))

(define (play-world monde) 
  (play monde BACKGROUND))

(define (rayon poid)
  (* poid (/ SIZE 20.0))) ;choix arbitraire (NE VERIFIE PAS LES CONDITIONS INITIALES)

(define INIT (make-masse POSITION1 (* 0.85 MI-FENETRE) VITESSE1X VITESSE1Y MASSE1 (rayon MASSE1) "blue")) ;monde initial

(big-bang SIZE-FEN SIZE-FEN 0.02 INIT)
(on-redraw play-world)
(on-tick-event next-world)