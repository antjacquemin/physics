
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "Ellipse.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : Une ellipse
; Paramètres : TAILLE_ELL sa taille et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly (build-list 50 (lambda (i) (let ((k (* i 2 pi (/ 50.)))) (make-masse 1 (make-vect (+ (* TAILLE_ELL (sin k)) 250) (+ (* 50 (cos k)) 200)) 0))))
                                0.1 (make-vect -4 -2)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))