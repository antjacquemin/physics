;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "fleur.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : Une Fleur
;            Paramètres :ANGLE son angle

;*****************************************************


(set! POLYGONE_EXS (make-poly (build-list 100 (lambda (i) (let* ((k (* i 2 pi (/ 100.))) 
                                                                        (r (+ 1 (* 3 (cos (* 3 k)))))
                                                                        (theta (sin k))) 
                                                                   (make-masse 1 (make-vect (+ 250 (* 30 r (cos theta)))
                                                                                            (+ 150 (* 30 r (sin theta)))) 0))))
                                     0.01 (make-vect 2 -3)))

(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))