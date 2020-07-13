;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Polygone régulier.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : un polygone régulier
; Paramètres : NB_COTES_POLYREG le nombre de ses côtés
;                   et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly (build-list NB_COTES_POLYREG  (lambda (i) (let ((z (* 98 (exp (/ (* 2 pi i +i) NB_COTES_POLYREG)))))
                                                           (make-masse 1 (make-vect (+ (real-part z) 200) (+ (imag-part z) 300)) 0))))
                                     0.05 (make-vect 0 -2))) 

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))