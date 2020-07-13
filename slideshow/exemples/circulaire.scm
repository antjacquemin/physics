

;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "circulaire.scm"
;  ce fichier est un des modules du slideshow
;  il ne fait appel à aucun autre fichier pour
;    être totalement indépendant et utilisé
;          indépendemment du projet
;       (difusion internet notamment)

; ce module est une animation montrant les collisions
;entre deux boules dans une enceinte circulaire
;http://projetphysics.teria.org/Slideshow/Quantité_de_mouvement_et_chocs.html

;*****************************************************


;*****************************************************

;                   LIBRAIRIE 

;*****************************************************

(require (lib "world.ss" "htdp")
         (lib "math.ss" "mzlib"))


;*****************************************************

;                     VECTEURS 
;            type abstrait de vecteur

;*****************************************************


;un vecteur est la donnée de ses coordonnées x et y
(define-struct vect (x y))

;vect-affiche : vecteur -> (void)
;affiche les coordonnées d'un vecteur au toplevel

(define (vect-affiche v)
  (printf "(~a , ~a)" (vect-x v) (vect-y v)))

;=vect? : vecteur x vecteur -> bool
;predicat de comparaison de deux vecteurs
(define (=vect? u v)
  (and (= (vect-x u) (vect-x v)) (= (vect-y u) (vect-y v))))

;*vect : float x vecteur -> vecteur
;la multiplication scalaire d'un vecteur
(define *vect
  (lambda (x v)
    (make-vect (* x (vect-x v)) (* x (vect-y v)))))

;+vect : vecteur x vecteur list -> vecteur
;l'addition vectorielle (de deux vecteurs ou plus)
(define (+vect u . L)
  (if (null? L)
      u
      (let ((v (+vect (apply +vect  L))))
        (make-vect (+ (vect-x u) (vect-x v)) (+ (vect-y u) (vect-y v))))))

;-vect : vecteur x vecteur -> vecteur
;soustraction de deux vecteurs
(define (-vect u v)
  (+vect u (*vect -1 v)))

;.scal : vecteur x vecteur -> float
;le produit scalaire usuel de IR^2
(define (.scal u v)
  (+ (* (vect-x u) (vect-x v)) (* (vect-y u) (vect-y v))))

;norme : vecteur -> float
;norme d'un vecteur
(define (norme v)
  (sqrt (.scal v v)))

;vect-angle : vecteur* -> float
;renvoit l'angle entre ]-pi,pi] compris entre (Ox) et le vecteur (non nul)
(define (vect-angle v)
  (cond 
    ;si l'ordonnée du vecteur est nulle
    ((zero? (vect-y v))
     ;il faut regarder le signe de x
     (if (> (vect-x v) 0)
         0
         pi))
    ;sinon, un appel a atan donne le résultat
    (else (atan (vect-y v) (vect-x v)))))


;;OUTILS

(define (projection xA yA xB yB xM yM)
  (if (= xA xB)
      (cons xA yM)
      (let ((a (exact->inexact (/ (- yA yB) (- xA xB)))) (b (exact->inexact (/ (- (* xA yB) (* yA xB)) (- xA xB)))) (x 0))
        (set! x (/ (+ (* xM xB) (- (* xM xA)) (- (* b yB)) (* b yA) (* yM yB) (- (* yM yA))) (+ xB (* a yB) (- xA) (- (* a yA)))))
        (cons x (+ b (* a x))))))

(define (rotation xA yA xM yM theta)
  (let* ((zA (+ xA (* +i yA))) (zM (+ xM (* +i yM))) (z (+ zA (* (exp (* theta +i)) (- zM zA)))))
    (cons (real-part z) (imag-part z))))

;*****************************************************

;                   CONSTANTES 

;*****************************************************

;taille de la fenêtre
(define SIZE-FEN 500)

;définition de la moitié de la fenêtre
(define MI-FENETRE (/ SIZE-FEN 2.0))

;surface de travail (pour dessiner un cadre)
(define SIZE (* 0.9 MI-FENETRE))          

;définition d'une structure de masse
(define-struct masse 
  (;position x
   x 
   ;position y
   y
   ;vitesse en x
   vx
   ;vitesse en y
   vy 
   ;masse de la boule
   m 
   ;rayon de la boule
   rayon
   ;couleur de la masse
   couleur))

;gestion du temps de l'animation
(define TIME 0)

;masse de la boule 1
(define MASSE1 3)

;masse de la boule 2
(define MASSE2 3) 

;composante horizontale de la vitesse de la boule 1
(define VITESSE1X 0)

;composante horizontale de la vitesse de la boule 2
(define VITESSE2X -5) 

;composante verticale de la vitesse de la boule 1
(define VITESSE1Y 9) 

;composante verticale de la vitesse de la boule 2
(define VITESSE2Y 0)

;position de la boule 1
(define POSITION1 (* 3 (/ SIZE-FEN 10.)))

;position de la boule 2
(define POSITION2 (* 7 (/ SIZE-FEN 10.))) 

;*****************************************************

;                  DEFINE THE WORLD...

;*****************************************************

;définition de l'arrière plan
(define BACKGROUND (let ((rext (+ SIZE (/ (- MI-FENETRE SIZE) 10.0))))
                     (place-image (circle SIZE 'solid "cyan") MI-FENETRE MI-FENETRE 
                                  (place-image (circle rext 'solid "black") MI-FENETRE MI-FENETRE 
                                               (place-image (rectangle SIZE-FEN SIZE-FEN 'solid "orange") MI-FENETRE MI-FENETRE 
                                                            (empty-scene SIZE-FEN SIZE-FEN))))))

;renvoit l'image de la masse de rayon r de couleur c
(define (balle r c)  
  (circle r 'solid c))

;changement de coordonnées : du repère scheme au plan complexe usuel centré
;par rapport à la fenêtre
(define (complexe x y)
  (+ (- x MI-FENETRE) (- (* +i y) (* +i MI-FENETRE))))

;intègre l'image de la masse passée en argument sur l'image img
(define (play masse img)
  (place-image (balle (masse-rayon masse) (masse-couleur masse)) (masse-x masse) (masse-y masse) img))

;pour mettre en mouvement le monde
(define (play-world monde) 
  (if 
   ;tant que le temps de l'animation n'est pas écoulé 
   (not (= TIME 1500)) 
   ;on incrémente le temps et l'on fait évoluer le monde
   (begin (set! TIME (add1 TIME)) (play (cadr monde) (play (car monde) BACKGROUND)))
   ;sinon, on affiche un message invitant à fermer la fenêtre
   (begin (end-of-time "Fin de l'animation")
          (place-image (text "Fin de l'animation" 30 "blue") 
                       125 235 
                       BACKGROUND))))

(define (chocx monde)
  (let* ((m1 (car monde))
         (m2 (cadr monde))
         (r1 (make-vect (masse-x m1) (masse-y m1)))
         (r2 (make-vect (masse-x m2) (masse-y m2)))
         (v1 (make-vect (masse-vx m1) (masse-vy m1)))
         (v2 (make-vect (masse-vx m2) (masse-vy m2)))
         ;la normale est colinéaire au vecteur M1M2 liant les deux centre de masses
         (normale (*vect (/ 1 (norme (-vect r1 r2))) (-vect r1 r2)))
         ;calcul de j selon la formule du slideshow
         (j (/ (- (* 2 (.scal (-vect v1 v2) normale))) 
               (+ (/ (masse-m m1)) 
                  (/ (masse-m m2)))))
         ;modification des nouvelles vitesses
         (v1s (+vect v1 (*vect (/ j (masse-m m1)) normale)))
         (v2s (+vect v2 (*vect (/ (- j) (masse-m m2)) normale))))
    (list (make-masse (masse-x m1) (masse-y m1) (vect-x v1s) (vect-y v1s) (masse-m m1) (masse-rayon m1) (masse-couleur m1))
          (make-masse (masse-x m2) (masse-y m2) (vect-x v2s) (vect-y v2s) (masse-m m2) (masse-rayon m2) (masse-couleur m2)))))

;repère les chocs entre les deux boules du monde passé en paramètre
(define (distance monde)
  (>= (+ (sqr (- (+ (masse-vx (car monde)) (masse-x (car monde)))
                 (+ (masse-vx (cadr monde)) (masse-x (cadr monde)))))
         (sqr (- (+ (masse-vy (car monde)) (masse-y (car monde)))
                 (+ (masse-vy (cadr monde)) (masse-y (cadr monde)))))) 
      (sqr (+ (masse-rayon (cadr monde)) (masse-rayon (car monde))))))

;si la boule n'est pas en collision avec l'autre masse du monde, 
;il faut gérer les collisions avec le rebord de l'enceinte circulaire
(define (next-masse masse)
  (let* ((x (masse-x masse)) 
         (y (masse-y masse)) 
         (vx (masse-vx masse)) 
         (vy (masse-vy masse))
         (c (masse-couleur masse)) 
         (r (masse-rayon masse))
         ;soit z la position de la masse dans le repère complexe
         (z (complexe (+ vx x) (+ vy y))) 
         (m (masse-m masse)))
    (if 
     ;si sa position est à une distance supèrieure au rayon du bord
     (< (+ r (magnitude z)) SIZE)
     ;la boule continue sa progression normalement
     (make-masse (+ x vx) (+ y vy) vx vy m r c)
     ;sinon, elle est soumise a un choc avec une masse infinie placé dans la direction radiale
     (car (chocx (list masse (make-masse MI-FENETRE MI-FENETRE 0 0 1e10 r "red")))))))

;passage du monde initial au nouveau monde :
(define (next-monde monde)
  (if
   ;s'il n'y a pas de choc
   (distance monde)
   ;chacune des boules évolue indépendemment de l'autre
   (list (next-masse (car monde)) (next-masse (cadr monde))) 
   ;sinon il y a collision entre les deux boules
   (chocx monde))) 

;on suppose que masse et rayon sont proportionnels
(define (rayon poid)
  (* poid (/ SIZE 20.)))

;le monde initiale est une liste a deux masses
(big-bang SIZE-FEN SIZE-FEN 0.01 (list (make-masse POSITION1 (* 0.85 MI-FENETRE) VITESSE1X VITESSE1Y MASSE1 (rayon MASSE1) "blue")
                                       (make-masse POSITION2 MI-FENETRE VITESSE2X VITESSE2Y MASSE2 (rayon MASSE2) "red")))
(on-redraw play-world)
(on-tick-event next-monde)