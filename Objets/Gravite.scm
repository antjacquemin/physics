;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Gravite.scm"
;              Exemple de la partie "POO"
;    Le monde est une réduit a une boule soumise
;          uniquement à la gravité

;*****************************************************

(set! WORLD (cons (new masse% (ray R_GRAV) (masse M_GRAV) (couleur "blue") (position (make-vect 350 250)) (vitesse V_GRAV)) WORLD))