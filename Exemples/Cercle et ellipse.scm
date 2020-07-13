;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;              "Cercle et ellipse.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
; Un cercle et une ellipse évoluent dans la fenêtre

;*****************************************************

(define POLY1 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i 2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 100 (sin k)) 300) (+ (* 60 (cos k)) 150)) 0))))
               0.005 
               (make-vect 0.5 -0.5)))

(define POLY2 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i -2 pi (/ 30.)))) (make-masse 1 (make-vect (+ (* 80 (sin k)) 300) (+ (* 80 (cos k)) 350)) 0))))
               -0. 
               (make-vect 0.5 0.5)))



(set! GRAVITE (make-vect 0 0))
(define POLYGONE_EXS (vector POLY1 POLY2))