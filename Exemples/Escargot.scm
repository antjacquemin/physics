;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "Escargot.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : un escargot
; Paramètres : TORSADE_ESC son paramètre de construction
;      influençant sur sa forme et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS (make-poly 
                    (build-list 100 
                                (lambda (i) (let ((k (* i 2 pi (/ 50.)))) 
                                              (make-masse 1 (make-vect (+ 250 (* 10 k (cos (* TORSADE_ESC k))))
                                                                       (+ 250 (* 10 k (sin (* TORSADE_ESC k))))) 0))))
                    0.1 (make-vect 5 -4)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
     (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                   (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
     (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))