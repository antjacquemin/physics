;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Boucle infinie.scm"
;     Exemple de la partie "Exemples paramétrés"
;    Section "Gravitation" : Une boucle infinie
; Paramètres : TAILLE_INF sa taille et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly (build-list 100 (lambda (i) (let ((k (* i 2 pi (/ 100.)))) (make-masse 1 (make-vect (+ 250 (* 25 TAILLE_INF (/ (sin k) (add1 (sqr (cos k))))))
                                                                                                                         (+ 250 (* 25 TAILLE_INF (cos k) (/ (sin k) (add1 (sqr (cos k))))))) 0))))
                                     -0.02 (make-vect 2 3)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))