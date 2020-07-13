;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Quadrillage.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : un quadrillage
; Paramètres : COURB_QUAD son paramètre de construction
;      influencant sa forme et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly (build-list 150 (lambda (i) (let ((k (* i 2 pi (/ 150.)))) 
                                                                   (make-masse 1 (make-vect (+ 250 (* 75 (sin (* COURB_QUA k))))
                                                                                            (+ 250 (* 75 (sin (* (add1 COURB_QUA) k))))) 0))))
                                     0.01 (make-vect 1 -1)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))