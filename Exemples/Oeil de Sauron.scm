
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Oeil de Sauron.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
;    Deux ellipses imbriquées l'une dans l'autre

;*****************************************************

(define POLY1 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i 2 pi (/ 30.)))) (make-masse 1 (make-vect (+ (* 50 (sin k)) 250) (+ (* 30 (cos k)) 250)) 0))))
               0.01 
               (make-vect 0.5 -0.5)))

(define POLY2 (make-poly 
               (build-list 50 (lambda (i) (let ((k (* i -2 pi (/ 50.)))) (make-masse 1 (make-vect (+ (* 100 (sin k)) 250) (+ (* 200 (cos k)) 250)) 0))))
               -0.01 
               (make-vect 0.5 0.5)))



(set! GRAVITE (make-vect 0 0))
(define POLYGONE_EXS (vector POLY1 POLY2))