
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "barre.scm"
;     Exemple de la partie "Exemples paramétrés"
;       Section "Gravitation" : Une Haltère
; Paramètres : TAILLE_HALT la taille de sa boule 
;           inférieure et ANGLE son angle

;*****************************************************

(set! POLYGONE_EXS 
      (make-poly (append (build-list 25 
                                     (lambda (i) 
                                       (let ((k (+ (/ pi 2) (* i 2 pi (/ 24.))))) 
                                         (make-masse 3 (make-vect (+ (* 30 (sin k)) 175) (+ (* 30 (cos k)) 175)) 0))))
                         (build-list 25 
                                     (lambda (i) 
                                       (let ((k (+ (* 3/2 pi) (* i 2 pi (/ 24.))))) 
                                         (make-masse TAILLE_HALT (make-vect (+ (* TAILLE_HALT 10 (sin k)) 325) (+ (* TAILLE_HALT 10 (cos k)) 175)) 0)))))
                 0.00 (make-vect 0 0)))



(for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
  (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
  (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))
