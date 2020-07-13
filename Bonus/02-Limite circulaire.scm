;*****************************************************

;                 HORS - PROJET 
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                     BONUS 2
;   Chocs d'une boule contre une paroi circulaire

;      Module codé en world.ss, non commenté, 
;         non utilisé dans le projet

;*****************************************************

;;OUTILS MATHEMATIQUES

(define (projection xA yA xB yB xM yM)
  (if (= xA xB)
      (cons xA yM)
      (let ((a (exact->inexact (/ (- yA yB) (- xA xB)))) (b (exact->inexact (/ (- (* xA yB) (* yA xB)) (- xA xB)))) (x 0))
        (set! x (/ (+ (* xM xB) (- (* xM xA)) (- (* b yB)) (* b yA) (* yM yB) (- (* yM yA))) (+ xB (* a yB) (- xA) (- (* a yA)))))
        (cons x (+ b (* a x))))))

(define (rotation xA yA xM yM theta)
  (let* ((zA (+ xA (* +i yA))) (zM (+ xM (* +i yM))) (z (+ zA (* (exp (* theta +i)) (- zM zA)))))
    (cons (real-part z) (imag-part z))))

;;CHOCS DANS UNE ENCEINTE CIRCULAIRE


(require (lib "world.ss" "htdp"))

(define SIZE-FEN 300)
(define MI-FENETRE (/ SIZE-FEN 2.0))
(define SIZE (* 0.9 MI-FENETRE))          
(define-struct masse (x y vx vy m rayon couleur))

(define BACKGROUND (let ((rext (+ SIZE (/ (- MI-FENETRE SIZE) 10.0))))
                     (place-image (circle SIZE 'solid "cyan") MI-FENETRE MI-FENETRE 
                                  (place-image (circle rext 'solid "black") MI-FENETRE MI-FENETRE 
                                               (empty-scene SIZE-FEN SIZE-FEN)))))

(define (BALL masse)
  (circle (masse-rayon masse) 'solid (masse-couleur masse)))

(define (complexe x y)
  (+ (- x MI-FENETRE) (- (* +i y) (* +i MI-FENETRE))))

(define (play masse) ;sans table tracante
  (place-image (BALL masse) (masse-x masse) (masse-y masse) BACKGROUND))

(define (chocx monde) ;renvoie une liste de deux masses qui s'entrechoquent
  (let* ((x1 (masse-x (car monde))) (x2 (masse-x (cadr monde))) 
                                    (y1 (masse-y (car monde))) (y2 (masse-y (cadr monde))) 
                                    (vxa (masse-vx (car monde))) (vxd (masse-vx (cadr monde))) 
                                    (vya (masse-vy (car monde))) (vyd (masse-vy (cadr monde)))
                                    (m1 (masse-m (car monde))) (m2 (masse-m (cadr monde)))
                                    (r1 (masse-rayon (car monde))) (r2 (masse-rayon (cadr monde)))
                                    (c1 (masse-couleur (car monde))) (c2 (masse-couleur (cadr monde)))
                                    (vxproj1 (projection 0 0 (- x2 x1) (- y2 y1) vxa vya))
                                    (alpha (angle (+ (* +i (- y2 y1)) (- x2 x1))))
                                    (alpha1 (if (>= (car vxproj1) 0) 
                                                (if (zero? (cdr vxproj1))
                                                    (- alpha)
                                                    (- (angle (+ (* +i (cdr vxproj1)) (car vxproj1)))))
                                                (- pi (angle (+ (* +i (cdr vxproj1)) (car vxproj1))))))
                                    (vxproj2 (projection 0 0 (- x2 x1) (- y2 y1) vxd vyd))
                                    (alpha2 (if (>= (car vxproj2) 0) 
                                                (if (zero? (cdr vxproj2))
                                                    (- alpha)
                                                    (- (angle (+ (* +i (cdr vxproj2)) (car vxproj2)))))
                                                (- pi (angle (+ (* +i (cdr vxproj2)) (car vxproj2))))))
                                    (vyproj1 (cons (- vxa (car vxproj1)) (- vya (cdr vxproj1))))
                                    (vyproj2 (cons (- vxd (car vxproj2)) (- vyd (cdr vxproj2))))
                                    (vxproj1 (car (rotation 0 0 (car vxproj1) (cdr vxproj1) alpha1)))
                                    (vxproj2 (car (rotation 0 0 (car vxproj2) (cdr vxproj2) alpha2)))
                                    (v1s (+ (* vxproj1 (/ (- m1 m2) (+ m1 m2))) (* vxproj2 (/ (* 2 m2) (+ m1 m2)))))
                                    (v2s (+ (* vxproj2 (/ (- m2 m1) (+ m1 m2))) (* vxproj1 (/ (* 2 m1) (+ m1 m2)))))
                                    (v1s (rotation 0 0 v1s 0 (- alpha1)))
                                    (v2s (rotation 0 0 v2s 0 (- alpha2)))
                                    (vxxproj1 (car v1s))
                                    (vxxproj2 (car v2s))
                                    (vxyproj1 (cdr v1s))
                                    (vxyproj2 (cdr v2s))
                                    (vyxproj1 (car vyproj1))
                                    (vyxproj2 (car vyproj2))
                                    (vyyproj1 (cdr vyproj1))
                                    (vyyproj2 (cdr vyproj2))
                                    (vx1 (+ vxxproj1 vyxproj1))
                                    (vy1 (+ vxyproj1 vyyproj1))
                                    (vx2 (+ vxxproj2 vyxproj2))
                                    (vy2 (+ vxyproj2 vyyproj2)))
    (list (make-masse x1 y1 vx1 vy1 m1 r1 c1) 
          (make-masse x2 y2 vx2 vy2 m2 r2 c2))))


(define (next-masse masse)
  (let* ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse))
                             (c (masse-couleur masse)) (r (masse-rayon masse))
                             (z (complexe (+ vx x) (+ vy y))) (m (masse-m masse)))
    (if (< (+ r (magnitude z)) SIZE)
        (make-masse (+ x vx) (+ y vy) vx vy m r c)
        (car (chocx (list masse (make-masse MI-FENETRE MI-FENETRE 0 0 1e10 r "red"))))))) ;s'il y a un choc avec la paroi, on calcule le choc hypothetique de la boule avec une masse infinie centree au milieu de la fenetre


;(big-bang SIZE-FEN SIZE-FEN 0.02 (make-masse 105 120 5 -2 100 (/ SIZE 10) "red"))
;(on-redraw play)
;(on-tick-event next-masse)


(define (play masse) ;avec table tracante
  (let ((x (masse-x masse)) (y (masse-y masse)) (vx (masse-vx masse)) (vy (masse-vy masse)))
    (set! BACKGROUND (add-line BACKGROUND x y (+ x vx) (+ y vy) "red"))
    (place-image (BALL masse) (masse-x masse) (masse-y masse) BACKGROUND)))

(big-bang SIZE-FEN SIZE-FEN 0.001 (make-masse 100 65 5 3 100 (/ SIZE 10) "red"))  ;application au parcours de la lumiere sur un miroir convexe
(on-redraw play)
(on-tick-event next-masse)