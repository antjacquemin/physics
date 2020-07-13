;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Triangles necklace.scm"
;     Exemple de la partie "Exemples paramétrés"
;                    Section "Chocs"
;               Un collier de triangles 

;*****************************************************

(set! GRAVITE (make-vect 0 0))

(define (triangle_alea x)
  (make-poly 
   (build-list 3 
               (lambda (i) (let ((k (* i -2 pi (/ 3.)))) 
                             (make-masse 8 (make-vect (+ (* 30 (sin k)) 250 (* 200 (cos (* 2 pi x 1/10))))
                                                      (+ (* 30 (cos k)) 250 (* 200 (sin (* 2 pi x 1/10))))) 0))))
   (if (odd? x) 0.001 -0.001)
   (make-vect (if (odd? x) 0.1 0) (if (odd? x) 0 -0.3))))

(define POLYGONE_EXS
      (build-vector 10 
                    (lambda (x)
                      (triangle_alea x))))