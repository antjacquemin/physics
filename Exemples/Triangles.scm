;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                   "Triangles.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
;                    Deux triangles

;*****************************************************

(define POLY1 (make-poly (list (make-masse 10 (make-vect 300 200) 0) (make-masse 10 (make-vect 350 400) 0) (make-masse 10 (make-vect 190 400) 0))
                         0.000 (make-vect 0 -0.51)))

(define POLY2 (make-poly (list (make-masse 10 (make-vect 100 300) 0) (make-masse 10 (make-vect 300 170) 0) (make-masse 10 (make-vect 80 110) 0))
                         0.002 (make-vect 0.51 0)))



(define POLYGONE_EXS (vector POLY1 POLY2))