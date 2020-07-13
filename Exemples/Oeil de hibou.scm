
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Oeil de hibou.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
; Des cercles concentriques sont perturbés par le
;          mouvement du cercle du centre

;*****************************************************

(define POLY1 (make-poly 
               (build-list 25 (lambda (i) (let ((k (* i 2 pi (/ 25.)))) (make-masse 1 (make-vect (+ (* 100 (sin k)) 125) (+ (* 100 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY2 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i -2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 50 (sin k)) 125) (+ (* 50 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY3 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i -2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 25 (sin k)) 125) (+ (* 25 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY4 (make-poly 
               (build-list 10 (lambda (i) (let ((k (* i -2 pi (/ 10.)))) (make-masse 1 (make-vect (+ (* 5 (sin k)) 125) (+ (* 5 (cos k)) 250)) 0))))
               0.0 
               (make-vect 1 0)))

(define POLY5 (make-poly 
               (build-list 25 (lambda (i) (let ((k (* i 2 pi (/ 25.)))) (make-masse 1 (make-vect (+ (* 100 (sin k)) 375) (+ (* 100 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY6 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i -2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 50 (sin k)) 375) (+ (* 50 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY7 (make-poly 
               (build-list 20 (lambda (i) (let ((k (* i -2 pi (/ 20.)))) (make-masse 1 (make-vect (+ (* 25 (sin k)) 375) (+ (* 25 (cos k)) 250)) 0))))
               0.0 
               (make-vect 0 0)))

(define POLY8 (make-poly 
               (build-list 10 (lambda (i) (let ((k (* i -2 pi (/ 10.)))) (make-masse 1 (make-vect (+ (* 5 (sin k)) 375) (+ (* 5 (cos k)) 250)) 0))))
               0.0 
               (make-vect -1 0)))

(set! GRAVITE (make-vect 0 0))
(define POLYGONE_EXS (vector POLY1 POLY2 POLY3 POLY4 POLY5 POLY6 POLY7 POLY8))