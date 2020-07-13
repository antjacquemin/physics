;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Deux boules.scm"
;              Exemple de la partie "POO"
; Le monde est réduit a deux boules qui s'entrechoquent

;*****************************************************

(set! GRAVITE 0.)
(set! WORLD (cons (new masse% (ray R1_2B) (masse M1_2B) (couleur "blue") (position (make-vect 350 140)) (vitesse V1_2B)) WORLD))
(set! WORLD (cons (new masse% (ray R2_2B) (masse M2_2B) (couleur "green") (position (make-vect 150 200)) (vitesse V2_2B)) WORLD))