;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;           "Polygones imbriques.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
; Des polygones imbriqués les uns dans les autres

;*****************************************************

(define POLY2 (make-poly 
               (build-list 7 (lambda (i) (let ((k (* i 2 pi (/ 7.)))) (make-masse 2.8 (make-vect (+ (* 200 (sin k)) 250) (+ (* 200 (cos k)) 250)) 0))))
               0. 
               (make-vect 0 0)))

(define POLY3 (make-poly 
               (build-list 5 (lambda (i) (let ((k (* i 2 pi (/ 5.)))) (make-masse 4 (make-vect (+ (* 150 (sin k)) 250) (+ (* 150 (cos k)) 200)) 0))))
               0. 
               (make-vect 0 0)))

(define POLY4 (make-poly 
               (build-list 3 (lambda (i) (let ((k (* i 2 pi (/ 3.)))) (make-masse 6.6 (make-vect (+ (* 100 (sin k)) 250) (+ (* 100 (cos k)) 200)) 0))))
               0. 
               (make-vect 0 0)))

(define POLY5 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i 2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 22 (sin k)) 250) (+ (* 22 (cos k)) 200)) 0))))
               0. 
               (make-vect 0 0.5)))

(set! GRAVITE (make-vect 0 0))
(define POLYGONE_EXS (vector POLY2 POLY3 POLY4 POLY5))