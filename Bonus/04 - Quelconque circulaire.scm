;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 4
;       Chocs quelconques entre deux boules
;         dans une enceinte circulaire

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************


(require (lib "world.ss" "htdp"))

(if (not (file-exists? "adt-outils.scm"))
    (error "Le fichier adt-outils.scm est introuvable")
    (load "adt-outils.scm"))

(define SIZE-FEN 500)
(define MI-FENETRE (/ SIZE-FEN 2.0))
(define SIZE (* 0.9 MI-FENETRE))          
(define-struct masse (x y vx vy m rayon couleur))

;**************************************
;Conditions initiales

(define MASSE1 3) ;choix arbitraire
(define MASSE2 3) ;choix arbitraire
(define VITESSE1X 0) ;choix arbitraire
(define VITESSE2X -10) ;choix arbitraire
(define VITESSE1Y 9) ;choix arbitraire
(define VITESSE2Y 0) ;choix arbitraire
(define POSITION1 (* 3 (/ SIZE-FEN 10.))) ;choix arbitraire
(define POSITION2 (* 7 (/ SIZE-FEN 10.)))  ;choix arbitraire
;**************************************

(define BACKGROUND (let ((rext (+ SIZE (/ (- MI-FENETRE SIZE) 10.0))))
                     (place-image (circle SIZE 'solid "cyan") MI-FENETRE MI-FENETRE 
                                  (place-image (circle rext 'solid "black") MI-FENETRE MI-FENETRE 
                                               (place-image (rectangle SIZE-FEN SIZE-FEN 'solid "orange") MI-FENETRE MI-FENETRE 
                                                            (empty-scene SIZE-FEN SIZE-FEN))))))

(define (balle r c)   ;construit l'image d'une masse
  (circle r 'solid c))

(define (complexe x y)   ;changement de repere : du repere scheme au plan complexe centre
  (+ (- x MI-FENETRE) (- (* +i y) (* +i MI-FENETRE))))

(define (play masse img)
  (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img)) ;ajoute a img l'image de la masse

(define (play-world monde) 
  (play (cadr monde) (play (car monde) BACKGROUND)))

(define (chocx monde) ;choc entre deux particules
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

(define (distance monde) ;y a-t-il choc ?
  (>= (+ (sqr (- (+ (masse-vx (car monde)) (masse-x (car monde)))
                 (+ (masse-vx (cadr monde)) (masse-x (cadr monde)))))
         (sqr (- (+ (masse-vy (car monde)) (masse-y (car monde)))
                 (+ (masse-vy (cadr monde)) (masse-y (cadr monde)))))) 
      (sqr (+ (masse-rayon (cadr monde)) (masse-rayon (car monde))))))

(define (next-masse masse) ;cas de la masse seule
  (let* ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse))
                             (c (masse-couleur masse)) (r (masse-rayon masse))
                             (z (complexe (+ vx x) (+ vy y))) (m (masse-m masse)))
    (if (< (+ r (magnitude z)) SIZE) ;touche-t-elle le rebord ?
        (make-masse (+ x vx) (+ y vy) vx vy m r c) ;si non, on continue
        (car (chocx (list masse (make-masse MI-FENETRE MI-FENETRE 0 0 1e10 r "red"))))))) ;si oui, cela revient a une collision avec une masse infinie

(define (next-monde monde) ;passage au monde suivant
  (if (distance monde) ;s'il n'y a pas collision
      (list (next-masse (car monde)) (next-masse (cadr monde))) ;chaque particule evolue normalement
      (chocx monde))) ;sinon, on traite le choc

(define (rayon poid)
  (* poid (/ SIZE 20.))) ;choix arbitraire (NE VERIFIE PAS LES CONDITIONS INITIALES)

(big-bang SIZE-FEN SIZE-FEN 0.02 (list (make-masse POSITION1 (* 0.85 MI-FENETRE) VITESSE1X VITESSE1Y MASSE1 (rayon MASSE1) "blue")
                                       (make-masse POSITION2 MI-FENETRE VITESSE2X VITESSE2Y MASSE2 (rayon MASSE2) "red")))
(on-redraw play-world)
(on-tick-event next-monde)