;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                  "pendule.scm"
;              Exemple de la partie "POO"
;    Le monde est une liste de boules (de 3 à 7)
;       alignées qui s'entrechoquent comme
;                 un pendule de newton

;*****************************************************

(set! GRAVITE 0.0)
(set! WORLD (cons (new masse% (ray 20) (masse 1) (couleur "cyan") (position (make-vect 25 250)) (vitesse (make-vect -2 0))) WORLD))
(for k from 0 to (- NB_PEND 2)
  (set! WORLD (cons (new masse% (ray 20) (masse 1) (couleur "cyan") (position (make-vect (+ (* k 40) 175)  250))
                         (vitesse (make-vect 0 0))) WORLD)))