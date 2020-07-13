;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "barre.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : Une barre unique
; Paramètres : LONG_BAR sa taille et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly (list (make-masse 1 (make-vect 200 150) 0) 
                                    (make-masse 1 (make-vect 210 150) 0)
                                    (make-masse 1 (make-vect 210 (+ 150 LONG_BAR)) 0)
                                    (make-masse 1 (make-vect 200 (+ 150 LONG_BAR)) 0))
                              0.02 (make-vect 0 2)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))