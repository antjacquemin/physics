;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "Boules multiples.scm"
;              Exemple de la partie "POO"
;    Le monde est une liste de boules (de 10 à 30)
;   Qui s'entrechoquent dans une enceinte carrée

;*****************************************************

(define WORLD_COPIE '())

(define (creation)
  (set! GRAVITE 0.)
  (for k from 1 to 30
       (let* ((n (add1 (random 2)))
              (M (new masse% (ray (* n 10)) (masse n) 
                      (couleur (make-object color% (random 255) (random 255) (random 255))) 
                      (position (make-vect (+ (random 460) 20) (+ (random 460) 20))) 
                      (vitesse (make-vect (- (random 20) 10) (- (random 20) 10))))))
         (while (not (null? (send M liste-chocs-multiples WORLD_COPIE)))
                (set! M (new masse% (ray (* n 10)) (masse n) 
                             (couleur (make-object color% (random 255) (random 255) (random 255))) 
                             (position (make-vect (+ (random 460) 20) (+ (random 460) 20))) 
                             (vitesse (make-vect (- (random 20) 10) (- (random 20) 10))))))
         (set! WORLD_COPIE (cons M WORLD_COPIE)))))

(define (actualiser_nombre)
  (do ((k 0 (add1 k)) (L WORLD_COPIE (cdr L)))
  ((= k NB_BAL))
  (set! WORLD (cons (car L) WORLD))))

(creation)
(actualiser_nombre)