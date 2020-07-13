;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;               "Formes diverses.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
; Cercles, losanges, triangles, et ellipses évoluent
;                 dans la fenêtre

;*****************************************************

(define POLY1 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i 2 pi (/ 30.)))) (make-masse 0.5 (make-vect (+ (* 50 (sin k)) 300) (+ (* 25 (cos k)) 150)) 0))))
               0. 
               (make-vect 0.5 0)))

(define POLY2 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i -2 pi (/ 30.)))) (make-masse 0.5 (make-vect (+ (* 50 (sin k)) 300) (+ (* 50 (cos k)) 350)) 0))))
               -0. 
               (make-vect 0 0)))

(define POLY3 (make-poly 
               (build-list 3 (lambda (i) (let ((k (* i -2 pi (/ 3.)))) (make-masse 8 (make-vect (+ (* 10 (sin k)) 100) (+ (* 10 (cos k)) 450)) 0))))
               -0. 
               (make-vect 0 0.5)))

(define POLY4 (make-poly 
               (build-list 4 (lambda (i) (let ((k (* i -2 pi (/ 4.)))) (make-masse 6 (make-vect (+ (* 25 (sin k)) 100) (+ (* 40 (cos k)) 50)) 0))))
               -0. 
               (make-vect 0 0)))


(define POLY5 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i 2 pi (/ 30.)))) (make-masse 0.5 (make-vect (+ (* 40 (sin k)) 200) (+ (* 20 (cos k)) 250)) 0))))
               0. 
               (make-vect 0.5 0)))

(define POLY6 (make-poly 
               (build-list 30 (lambda (i) (let ((k (* i -2 pi (/ 30.)))) (make-masse 0.5 (make-vect (+ (* 40 (sin k)) 100) (+ (* 40 (cos k)) 375)) 0))))
               -0. 
               (make-vect -0.5 0)))

(define POLY7 (make-poly 
               (build-list 3 (lambda (i) (let ((k (* i -2 pi (/ 3.)))) (make-masse 8 (make-vect (+ (* 10 (sin k)) 400) (+ (* 10 (cos k)) 200)) 0))))
               -0. 
               (make-vect 0 0)))

(define POLY8 (make-poly 
               (build-list 4 (lambda (i) (let ((k (* i -2 pi (/ 4.)))) (make-masse 6 (make-vect (+ (* 25 (sin k)) 300) (+ (* 40 (cos k)) 250)) 0))))
               -0. 
               (make-vect 0 0.5)))


(set! GRAVITE (make-vect 0 0))
(define POLYGONE_EXS (vector POLY1 POLY2 POLY3 POLY4 POLY5 POLY6 POLY7 POLY8))