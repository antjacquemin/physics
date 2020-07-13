;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 3
;   Chocs quelconques dans une enceinte carrée

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

;**************************************
;Conditions initiales

(define MASSE1 2) ;choix arbitraire
(define MASSE2 2) ;choix arbitraire
(define VITESSE1X 5) ;choix arbitraire
(define VITESSE2X 0) ;choix arbitraire
(define VITESSE1Y 0) ;choix arbitraire
(define VITESSE2Y 0) ;choix arbitraire
(define POSITION1 (* 2 (/ SIZE-FEN 10.))) ;choix arbitraire
(define POSITION2 (* 6 (/ SIZE-FEN 10.)))  ;choix arbitraire
;**************************************


(define-struct masse (x y vx vy m rayon couleur))    ; (x,y) la position, (vx,vy) la vitesse, m la masse

(define (balle r c)
  (circle r 'solid c))       ;renvoie l'image d'une sphere pleine de rayon r de couleur c

(define BACKGROUND (let* ((rext (+ SIZE (/ (- SIZE-FEN SIZE) 5.0))))   ; le fond d'ecran (la surface d'etude ne fait pas la taille dela fenetre)
                     (place-image (rectangle SIZE SIZE 'solid "yellow") MI-FENETRE MI-FENETRE 
                                  (place-image (rectangle rext rext 'solid "black") MI-FENETRE MI-FENETRE 
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

(define (distance monde) ;y a-t-il choc ?
  (>= (+ (sqr (- (+ (masse-vx (car monde)) (masse-x (car monde)))
                 (+ (masse-vx (cadr monde)) (masse-x (cadr monde)))))
         (sqr (- (+ (masse-vy (car monde)) (masse-y (car monde)))
                 (+ (masse-vy (cadr monde)) (masse-y (cadr monde)))))) 
      (sqr (+ (masse-rayon (cadr monde)) (masse-rayon (car monde))))))

(define (chocx monde) ;la pour le coup je pense qu'il faudra plus que je te l'explique en live qu'en simple commentaire ^^
  (let* ((x1 (masse-x (car monde))) (x2 (masse-x (cadr monde))) 
                                    (y1 (masse-y (car monde))) (y2 (masse-y (cadr monde))) 
                                    (vxa (masse-vx (car monde))) (vxd (masse-vx (cadr monde))) 
                                    (vya (masse-vy (car monde))) (vyd (masse-vy (cadr monde)))
                                    (m1 (masse-m (car monde))) (m2 (masse-m (cadr monde)))
                                    (r1 (masse-rayon (car monde))) (r2 (masse-rayon (cadr monde)))
                                    (c1 (masse-couleur (car monde))) (c2 (masse-couleur (cadr monde)))
                                    (vxproj1 (projection 0 0 (- x2 x1) (- y2 y1) vxa vya))
                                    (alpha (atan (- y2 y1) (- x2 x1)))
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


(define (verifie-em monde) ; affiche l'energie mecanique du systeme a chaque tic d'horloge
  (let* ((vxa (masse-vx (car monde))) 
         (vxd (masse-vx (cadr monde))) 
         (vya (masse-vy (car monde))) (vyd (masse-vy (cadr monde)))
         (m1 (masse-m (car monde))) (m2 (masse-m (cadr monde)))
         (v1 (sqrt (+ (sqr vxa) (sqr vya)))) (v2 (sqrt (+ (sqr vxd) (sqr vyd)))))
    (printf "Em = ~a\n" (/ (+ (* m1 (sqr v1)) (* m2 (sqr v2))) 2))))

(define (next-world monde)
  ;  (verifie-em monde)   ;a activer pour verifier la conservation de l'energie (ca marche !!!)
  (if (distance monde)   ; s'il n'y a pas de choc entre les masses
      (list (next (car monde)) (next (cadr monde))) ;on les traite de maniere independante 
      (chocx monde))) ;sinon, on calcul prealablement leur nouvelle vitesse

(define (play masse img)
  (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img)) ;ajoute a img l'image de la masse

(define (play-world monde) 
  (play (cadr monde) (play (car monde) BACKGROUND)))

(define (rayon poid)
  (* poid (/ SIZE 20.))) ;choix arbitraire (NE VERIFIE PAS LES CONDITIONS INITIALES)

(define INIT (list (make-masse POSITION1 (* 0.80 MI-FENETRE) VITESSE1X VITESSE1Y MASSE1 (rayon MASSE1) "blue") (make-masse POSITION2 MI-FENETRE VITESSE2X VITESSE2Y MASSE2 (rayon MASSE2) "red"))) ;monde initial

(big-bang SIZE-FEN SIZE-FEN 0.02 INIT)
(on-redraw play-world)
(on-tick-event next-world)