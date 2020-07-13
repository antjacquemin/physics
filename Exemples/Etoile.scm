;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "Etoile.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : Une étoile
;      Paramètres : NB_BRAN_STAR son nombre de
;             branches et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly 
                    (build-list NB_BRAN_STAR 
                                (lambda (i) (let ((z (exp (/ (* 2 pi i +i) NB_BRAN_STAR)))) 
                                              (if (even? i) 
                                                  (make-masse 1 (make-vect (+ (real-part (* 100 z)) 250) (+ (imag-part (* 100 z)) 300)) 0)
                                                  (make-masse 1 (make-vect (+ (real-part (* 45 z)) 250) (+ (imag-part (* 45 z)) 300)) 0)))))
                    0.00 (make-vect 1 -1)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
     (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                   (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
     (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))