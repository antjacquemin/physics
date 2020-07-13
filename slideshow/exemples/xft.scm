

;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "xft.scm"
;  ce fichier est un des modules du slideshow
;  il ne fait appel à aucun autre fichier pour
;    être totalement indépendant et utilisé
;          indépendemment du projet
;       (difusion internet notamment)

; Ce fichier permet de sensibiliser l'utilisateur 
;aux graphiques de la forme x=f(t)
;http://projetphysics.teria.org/Slideshow/Physique_du_point.html

;*****************************************************


;*****************************************************

;                     FILE 
;            type abstrait de file

;*****************************************************

(define (file-vide)
  (cons '() '()))

(define (file-vide? F)
  (and (pair? F) (null? (car F)) (null? (cdr F))))

(define (enfiler! x F)    
  (let ((new (list x)))
    (cond ((file-vide? F) 
           (set-car! F new) 
           (set-cdr! F new))
          (else 
           (set-cdr! (cdr F) new) (set-cdr! F new)))))

(define (defiler! F)    
  (when (not (file-vide? F))
    (if (eq? (car F) (cdr F))
        (begin (set-car! F '()) (set-cdr! F '()))
        (set-car! F (cdar F)))))

(define (premier F)     
  (if (file-vide? F)
      (error "La file est vide ! " F)
      (caar F)))

;*****************************************************

;                     MODULE 

;*****************************************************

(define X 100)
(define COURBE (file-vide))
(enfiler! X COURBE)
(define SIZE 1)
(define V 1)
(define ACTION #f)

;fenêtre principale du module
(define FRAME (new frame%
                   (label "x=f(t)")
                   (style '(metal hide-menu-bar))
                   (stretchable-height #f)
                   (stretchable-width #f)))

;panel des canvas
(define VPANEL (new vertical-panel%
                    (parent FRAME)))

;panel des boutons de gestion du mobile
(define HPANEL (new horizontal-panel%
                    (parent FRAME)
                    (alignment '(center center))))

;panel des boutons de gestion du module
(define HPANEL2 (new horizontal-panel%
                     (parent FRAME)
                     (alignment '(center center))))

;canvas de la courbe
(define CANVAS1 (new canvas%
                     (parent VPANEL)
                     (min-height 500)
                     (min-width 500)
                     (style '(no-autoclear))
                     (paint-callback
                      (lambda (evt dc)
                        (separation BITMAP-DC1)
                        (repere BITMAP-DC1)
                        (draw-courbe BITMAP-DC1)
                        (send dc draw-bitmap BITMAP1 0 0 'solid)))))

(define BITMAP1 (make-object bitmap% 500 500)) 
(define BITMAP-DC1 (new bitmap-dc% (bitmap BITMAP1))) 

;canvas de l'axe
(define CANVAS2 (new canvas%
                     (parent VPANEL)
                     (min-height 100)
                     (min-width 500)
                     (style '(no-autoclear))
                     (paint-callback
                      (lambda (evt dc)
                        (axe BITMAP-DC2)
                        (curseur BITMAP-DC2)
                        (send dc draw-bitmap BITMAP2 0 0 'solid)))))

(define BITMAP2 (make-object bitmap% 500 500)) 
(define BITMAP-DC2 (new bitmap-dc% (bitmap BITMAP2))) 

;bouton ralentissant la vitesse du mobile
(define LENT (new button%
                  (parent HPANEL)
                  (label "-")
                  (callback (lambda (a b)
                              (set! V (- V 0.2))))))

;bouton stoppant l'animation
(define STOP (new button%
                  (parent HPANEL)
                  (label "Go")
                  (callback (lambda (a b)
                              (if ACTION 
                                  (begin (send STOP set-label "Go")
                                         (set! ACTION #f)
                                         (action))
                                  (begin (send STOP set-label "Stop")
                                         (set! ACTION #t)
                                         (action)))))))

;bouton augmentant la vitesse du mobile
(define RAPIDE (new button%
                    (parent HPANEL)
                    (label "+")
                    (callback (lambda (a b)
                                (set! V (+ V 0.2))))))

;pour relancer l'animation
(define RESTART (new button%
                     (parent HPANEL2)
                     (label "Restart")
                     (callback (lambda (a b)
                                 (set! V 1)
                                 (set! X 100)
                                 (set! COURBE (file-vide))
                                 (enfiler! X COURBE)
                                 (set! SIZE 1)
                                 (set! ACTION #f)
                                 (send CANVAS1 on-paint)
                                 (send CANVAS2 on-paint)
                                 (send STOP set-label "Go")))))

;pour fermer la fenêtre en stoppant l'action
(define CLOSE (new button%
                   (parent HPANEL2)
                   (label "Fermer la fenêtre")
                   (callback (lambda (a b)
                               (set! ACTION #f)
                               (send FRAME show #f)))))

;ligne de séparation
(define (separation dc)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line 0 500 500 500))

;création du repère dans le canvas 1
(define (repere dc)
  (define (curseur y)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-line 27 y 33 y))
  (send dc clear)
  (separation dc)
  (send dc set-pen "black" 1 'solid)
  (send dc draw-line 30 50 30 450)
  (send dc draw-line 30 50 35 60)
  (send dc draw-line 30 50 25 60)
  (send dc draw-line 30 450 470 450)
  (send dc draw-line 470 450 460 455)
  (send dc draw-line 470 450 460 445)
  (send dc draw-text "O" 15 460)
  (send dc draw-text "x" 10 40)
  (send dc draw-text "t" 450 460)
  (send dc draw-text "xA" 5 370)
  (curseur 400)
  (send dc draw-text "xB" 5 70)
  (curseur 100))

;création de l'axe dans le canvas 2
(define (axe dc)
  (define (curseur x)
    (send dc set-pen "black" 1 'solid)
    (send dc draw-line x 52 x 48))
  (send dc clear)
  (send dc set-pen "black" 1 'solid)
  (send dc draw-line 30 50 470 50)
  (send dc draw-line 470 50 460 55)
  (send dc draw-line 470 50 460 45)
  (send dc draw-text "O" 20 55)
  (curseur 30)
  (send dc draw-text "x" 460 55)
  (send dc draw-text "A" 100 55)
  (curseur 100)
  (send dc draw-text "B" 400 55)
  (curseur 400))

;curseur de la masse courante sur l'axe
(define (curseur dc)
  (axe dc)
  (send dc set-pen "red" 4 'solid)
  (send dc draw-point (- X 2) 48))

;la courbe est une file contenant les couples (x.y) des coordonnées des points
;la constituant
(define (draw-courbe dc)
  (repere dc)
  (send dc set-pen "red" 1 'solid)
  (do ((C (car COURBE) (cdr C)) (t 30 (+ t 1)) (L (length (car COURBE)) (- L 1)))
    ((<= L 1))
    (send dc draw-line t (- 500 (car C)) (+ t 1) (- 500 (cadr C)))))

;pour faire évoluer le mobile
(define (action)
  (when 
      ;si le monde est en mouvement
      ACTION
    (let ((DC1 BITMAP-DC1) (DC2 BITMAP-DC2))
      (when (= SIZE 400)
        ;si la taille de la courbe excède 400 points, 
        ;on défile avant d'enfiler le nouveau point
        (defiler! COURBE)
        (set! SIZE (- SIZE 1)))
      (cond (;on gère le cas de la collision avec B : fin de l'action
             (> X 400) (set! ACTION #f))
            (;puis le cas de la collision avec A : rebond elastique
             (< X 100) (set! V (- V)) (set! X 100))
            (;sinon, le point est entre A et B, et évolue normalement
             else (enfiler! X COURBE)
                  (set! X (+ X V))))
      (set! SIZE (add1 SIZE))
      (draw-courbe DC1)
      (curseur DC2)
      (send (send CANVAS1 get-dc) draw-bitmap BITMAP1 0 0 'solid)
      (send (send CANVAS2 get-dc) draw-bitmap BITMAP2 0 0 'solid)
      (sleep/yield 0.01)
      (action))))

(send FRAME show #t)
(action)