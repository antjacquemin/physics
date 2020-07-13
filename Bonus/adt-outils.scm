;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;               OUTILS MATHEMATIQUES

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

(define (projection xA yA xB yB xM yM)
  (if (= xA xB)
      (cons xA yM)
      (let ((a (exact->inexact (/ (- yA yB) (- xA xB)))) (b (exact->inexact (/ (- (* xA yB) (* yA xB)) (- xA xB)))) (x 0))
        (set! x (/ (+ (* xM xB) (- (* xM xA)) (- (* b yB)) (* b yA) (* yM yB) (- (* yM yA))) (+ xB (* a yB) (- xA) (- (* a yA)))))
        (cons x (+ b (* a x))))))

(define (rotation xA yA xM yM theta)
  (let* ((zA (+ xA (* +i yA))) (zM (+ xM (* +i yM))) (z (+ zA (* (exp (* theta +i)) (- zM zA)))))
    (cons (real-part z) (imag-part z))))

(printf "\nLes outils mathematiques de adt-outils.scm sont :\n")
(printf "[(projection xA yA xB yB xM yM),(rotation xA yA xM yM theta)]\n")