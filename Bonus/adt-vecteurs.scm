;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;              OUTILS VECTORIELS

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

(define-struct vect (x y))

(define (vect-affiche v)
  (printf "(~a , ~a)" (vect-x v) (vect-y v)))

(define *vect
  (lambda (x v)
    (make-vect (* x (vect-x v)) (* x (vect-y v)))))

(define +vect
  (lambda (u v)
    (make-vect (+ (vect-x u) (vect-x v)) (+ (vect-y u) (vect-y v)))))

(define (.scal u v)
  (+ (* (vect-x u) (vect-x v)) (* (vect-y u) (vect-y v))))

(define (norme v)
  (sqrt (.scal v v)))

(define (vect-angle v)
  (angle (+ (vect-x v) (* +i (vect-y v)))))

(printf "\nLes outils vectoriels de adt-vecteurs.scm sont :\n")
(printf "[(make-vect x y),(vect-x v),(vect-y v),(vect-affiche v),(+vect u v),(*vect lambda v),(norme v),(.scal u v),(vect-angle v)]\n")