

;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "desaxe.scm"
;  ce fichier est un des modules du slideshow
;  il ne fait appel à aucun autre fichier pour
;    être totalement indépendant et utilisé
;          indépendemment du projet
;       (difusion internet notamment)

; Ce fichier permet de sensibiliser l'utilisateur 
;aux chocs élastiques quelconques entre deux masses
;http://projetphysics.teria.org/Slideshow/Quantité_de_mouvement_et_chocs.html

;*****************************************************


;*****************************************************

;                   LIBRAIRIE 

;*****************************************************

(require (lib "math.ss" "mzlib"))


;*****************************************************

;                   CONSTANTES 

;*****************************************************

;Hauteur de la fenêtre
(define WIDTH 500)

;Largeur de la fenêtre
(define HEIGHT 500)

;Initialement, les masses ne bougent pas
(define ACTION #f)

;La gravité est nulle dans ce module
(define GRAVITE 0)

;On affiche le centre de masse du système
(define AFFICHE-CENTRE? #t)

;Le monde sera une liste de masses
(define WORLD '())

;La fréquence horloge sera de 1/50
(define CLOCK (/ 1 24.0))

;*****************************************************

;                      MACROS 
; définit les macros for et while utilisées
;                 dans le fichier

;*****************************************************

(define-syntax for
  (syntax-rules (from to by)
    ((for i from a to b by c e1 e2 ...)
     (let ((vb b) (vc c))
       (do ((i a (+ i vc)))
         ((> i vb) (void))
         e1 e2 ...)))
    ((for i from a to b e1 e2 ...) (for i from a to b by 1 e1 e2 ...))))

(define-syntax while          
  (syntax-rules ()
    ((while test e1 e2 ...) (do ()
                              ((not test) (void))
                              e1 e2 ...))))


;*****************************************************

;                     VECTEURS 
;            type abstrait de vecteur

;*****************************************************


;un vecteur est la donnée de ses coordonnées x et y
(define-struct vect (x y))

;vect-affiche : vecteur -> (void)
;affiche les coordonnées d'un vecteur au toplevel

(define (vect-affiche v)
  (printf "(~a , ~a)" (vect-x v) (vect-y v)))

;=vect? : vecteur x vecteur -> bool
;predicat de comparaison de deux vecteurs
(define (=vect? u v)
  (and (= (vect-x u) (vect-x v)) (= (vect-y u) (vect-y v))))

;*vect : float x vecteur -> vecteur
;la multiplication scalaire d'un vecteur
(define *vect
  (lambda (x v)
    (make-vect (* x (vect-x v)) (* x (vect-y v)))))

;+vect : vecteur x vecteur list -> vecteur
;l'addition vectorielle (de deux vecteurs ou plus)
(define (+vect u . L)
  (if (null? L)
      u
      (let ((v (+vect (apply +vect  L))))
        (make-vect (+ (vect-x u) (vect-x v)) (+ (vect-y u) (vect-y v))))))

;-vect : vecteur x vecteur -> vecteur
;soustraction de deux vecteurs
(define (-vect u v)
  (+vect u (*vect -1 v)))

;.scal : vecteur x vecteur -> float
;le produit scalaire usuel de IR^2
(define (.scal u v)
  (+ (* (vect-x u) (vect-x v)) (* (vect-y u) (vect-y v))))

;norme : vecteur -> float
;norme d'un vecteur
(define (norme v)
  (sqrt (.scal v v)))

;vect-angle : vecteur* -> float
;renvoit l'angle entre ]-pi,pi] compris entre (Ox) et le vecteur (non nul)
(define (vect-angle v)
  (cond 
    ;si l'ordonnée du vecteur est nulle
    ((zero? (vect-y v))
     ;il faut regarder le signe de x
     (if (> (vect-x v) 0)
         0
         pi))
    ;sinon, un appel a atan donne le résultat
    (else (atan (vect-y v) (vect-x v)))))



;*****************************************************

;                      CLASSES 
; définit les deux classes utilisées dans le module

;*****************************************************


;On définit une classe point
(define point%
  (class object%
    (init-field 
     ;vecteur position du point (vecteur)
     (position (make-vect 0 0)) 
     ;vecteur-vitesse du point (vecteur)
     (vitesse (make-vect 0 0)) 
     ;marq est un marqueur indiquant si oui ou non
     ;le point a subit un choc durant l'évolution du monde (booleen)
     (marq #f) 
     ;la masse du point (number)
     (masse 0))
    ;accesseur au marq
    (define/public (marqueur) marq)
    ;mutateur de marq
    (define/public (set-marqueur! bool) (set! marq bool))
    ;accesseur de position
    (define/public (r) position)
    ;accesseurs des champs de position
    (define/public (x) (vect-x position))
    (define/public (y) (vect-y position))
    ;accesseur de vitesse
    (define/public (v) vitesse)
    ;accesseurs des champs de vitesse
    (define/public (vx) (vect-x vitesse))
    (define/public (vy) (vect-y vitesse))
    ;acceseur de masse
    (define/public (m) masse)
    ;mutateurs de position et de ses champs
    (define/public (set-r! r) (set! position r))
    (define/public (set-x! x) (set-vect-x! position x))
    (define/public (set-y! y) (set-vect-y! position y))
    ;mutateurs de vitesse et de ses champs
    (define/public (set-v! v) (set! vitesse v))
    (define/public (set-vx! vx) (set-vect-x! vitesse vx))
    (define/public (set-vy! vy) (set-vect-y!  vitesse vy))
    ;evolution du point sans contrainte (pas de vitesse angulaire ici)
    (define/public (move) 
      (set! position (+vect position vitesse)))
    (super-new)))

(define masse%
  (class point%
    ;une masse est un point ayant en plus un rayon (number) et une couleur (color)
    (init-field (ray 0) (couleur "black"))
    (inherit-field position vitesse marq)
    ;accesseur de ray
    (define/public (rayon) ray)
    ;accesseur de couleur
    (define/public (color) couleur)
    ;affichage de la masse dans le canvas de dc
    (define/public (play dc)
      ;le contour sera noir d'épaisseur 1
      (send dc set-pen (make-object pen% "black" 1 'solid))
      ;la boule sera peinte de sa propre couleur
      (send dc set-brush (make-object brush% couleur 'solid))
      ;on demande au dc de peindre la boule à sa position r
      (send dc draw-ellipse (- (vect-x position) ray) (- (vect-y position) ray) (* 2 ray) (* 2 ray)))
    ;en l'abscence de choc avec une autre boule, la masse évolue (en veillant aux chocs entre les parois)
    ;avec la fonction next décrite ci-après
    (define/public (change width height gravite)
      (next this width height gravite))
    ;il y a choc entre deux boules si elles sont trop proches l'une de l'autre :
    (define (choc? A) (< (norme (+vect (+vect vitesse position) (*vect -1 (+vect (send A v) (send A r)))))
                         (+ ray (send A rayon))))
    ;liste-chocs renvoie la liste des masses au contact de la masse courante
    (define/public (liste-chocs LISTE)
      ;itérativement
      (define (iter L acc)
        ;si la liste est vide, on renvoie l'accumulateur (list)
        (if (null? L)
            acc
            (if 
             ;si la masse courante est le premier élément de la liste...
             (eq? (car L) this)
             ;on traite la fin de la liste
             (iter (cdr L) acc)
             ;sinon, s'il y a choc
             (if (<= (norme (+vect position (*vect -1 (send (car L) r))))
                     (+ ray (send (car L) rayon)))
                 ;on ajoute le premier élément de la liste à acc
                 (iter (cdr L) (cons (car L) acc))
                 ;sinon, on itère sur la fin de la liste
                 (iter (cdr L) acc)))))
      (iter LISTE '()))
    ;traitement des chocs entre la boule et le reste du monde
    (define/public (chocs L)
      (let (;BOOL indiquera si la boule courante aura subit ou non un choc
            (BOOL #t))
        (while 
         ;tant que la boule n'a pas subit de choc
         ;et que le monde n'est pas vide
         (and BOOL (not (null? L)))
         (cond (;si le premier élément du monde est la boule courante
                ;on traite le reste du monde
                (eq? (car L) this) (set! L (cdr L)))
               (;s'il n'y a pas de choc entre le premier élément du monde
                ;et la boule courante, on traite le reste du monde
                (not (choc? (car L))) (set! L (cdr L)))
               (else 
                ;s'il y a choc
                (let* (;masse_au_contact est la masse avec laquelle la boule courante 
                       ;entre en contact
                       (masse_au_contact (car L))
                       ;issue_du_choc est la paire contenant les vitesses des deux boules
                       ;après le choc
                       (issue_du_choc (chocx-objet this masse_au_contact)))
                  ;il y a eu choc : modification de BOOL
                  (set! BOOL #f)
                  ;ainsi que les marqueurs des deux masses
                  (set! marq #t)
                  (send masse_au_contact set-marqueur! #t)
                  ;puis modification des vitesses selon issue_du_choc
                  (set! vitesse (car issue_du_choc))
                  (send masse_au_contact set-v! (cdr issue_du_choc))))))))
    ;Lorsque deux masses entrent en collision
    (define (chocx-objet m1 m2)
      (let* (;la normale est colinéaire au vecteur M1M2 liant les deux centre de masses
             (normale (*vect (/ 1 (norme (-vect (send m1 r) (send m2 r)))) (-vect (send m1 r) (send m2 r))))
             ;calcul de j selon la formule du slideshow
             (j (/ (- (* 2 (.scal (-vect (send m1 v) (send m2 v)) normale))) 
                   (+ (/ (send m1 m)) 
                      (/ (send m2 m))))))
        ;modification des nouvelles vitesses
        (cons (+vect (send m1 v) (*vect (/ j (send m1 m)) normale))
              (+vect (send m2 v) (*vect (/ (- j) (send m2 m)) normale)))))
    ;Lorsqu'il n'y a pas contact entre deux boules,  il faut éventuellement gérer les collisions avec les rebords de la fenêtre d'affichage
    (define-syntax next
      (syntax-rules ()
        ((next masse WIDTH HEIGHT GRAVITE)
         (let* ((x (send masse x)) 
                (y (send masse y)) 
                (v (send masse v)) 
                (vx (send masse vx)) 
                (vy (send masse vy))          
                (r (send masse rayon)) 
                (xs (+ x vx)) 
                (ys (+ y vy)))
           (cond (;collision mur droit
                  (>= (+ xs r) WIDTH)
                  (cond (; collision simultanee mur droit et mur bas
                         (>= (+ ys r) HEIGHT)                
                         (send masse set-v! (*vect -1 v))
                         (send masse set-x! (- WIDTH r))
                         (send masse set-y! (- HEIGHT r)))
                        (; collision simultanee mur droit et mur haut
                         (<= (- ys r) 0)                
                         (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))
                         (send masse set-v! (*vect -1 v))
                         (send masse set-x! (- WIDTH  r))
                         (send masse set-y! r))
                        (; collision mur droit seul
                         else               
                         (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))
                         (send masse set-vx! (- vx))
                         (send masse set-r! (make-vect (- WIDTH r) ys)))))
                 (; collision mur gauche
                  (<= (- xs r) 0)       
                  (cond (; collision simultanee mur gauche et mur bas
                         (>= (+ ys r) HEIGHT)              
                         (send masse set-v! (*vect -1 v))
                         (send masse set-x! r)
                         (send masse set-y! (- HEIGHT r)))
                        (; collision simultanee mur gauche et mur haut
                         (<= (- ys r) 0)           
                         (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))
                         (send masse set-v! (*vect -1 v))
                         (send masse set-x! r)
                         (send masse set-y! r))
                        (; collision mur gauche seul
                         else               
                         (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))
                         (send masse set-vx! (- vx))
                         (send masse set-r! (make-vect r ys)))))
                 (; collision mur bas seul
                  (>= (+ ys r) HEIGHT)          
                  (send masse set-vy! (- vy))
                  (send masse set-r! (make-vect xs (- HEIGHT r))))
                 (; collision mur haut seul
                  (<= (- ys r) 0)                       
                  (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))
                  (send masse set-vy! (- vy))
                  (send masse set-r! (make-vect xs  r)))
                 (; aucune collision : la boule n'est soumise qu'à la gravité
                  else                          
                  (send masse move)
                  (send masse set-v! (+vect (send masse v) (make-vect 0 GRAVITE)))))))))
    (super-new)))


;*****************************************************

;                 INTERFACE GRAPHIQUE

;*****************************************************

;fenêtre du module
(define FRAME
  (new frame%
       (label "Choc desaxe")
       (style '(metal hide-menu-bar))))

;panel du CANVAS
(define VPANEL 
  (new horizontal-panel%
       (parent FRAME)))

;panel des boutons de la boule bleue
(define HPANELB 
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center bottom))))

;panel des boutons de la boule verte
(define HPANELV 
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center bottom))))

;panel des boutons de gestion du module
(define HPANEL 
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center bottom))))

;modification de la taille de la boule bleue
(define MB
  (new button%
       (parent HPANELB)
       (label "Change B")
       (callback (lambda (a b)
                   (set! WORLD '())
                   (set! masseb (add1 (modulo (add1 masseb) 3)))
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;modification aléatoire de la vitesse de la boule bleue
(define VB
  (new button%
       (parent HPANELB)
       (label "Change VitesseB")
       (callback (lambda (a b)
                   (set! vb (- (random 20) 10))
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;modification de la vitesse de la boule bleue qui devient nulle
(define VB0
  (new button%
       (parent HPANELB)
       (label "VitesseB=0")
       (callback (lambda (a b)
                   (set! vb 0)
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;modification de la taille de la boule verte
(define MV
  (new button%
       (parent HPANELV)
       (label "Change V")
       (callback (lambda (a b)
                   (set! WORLD '())
                   (set! massev (add1 (modulo (add1 massev) 3)))
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;modification aléatoire de la vitesse de la boule verte
(define VV
  (new button%
       (parent HPANELV)
       (label "Change VitesseV")
       (callback (lambda (a b)
                   (set! vv (- (random 20) 10))
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;modification de la vitesse de la boule bleue qui devient nulle
(define VV0
  (new button%
       (parent HPANELV)
       (label "VitesseV=0")
       (callback (lambda (a b)
                   (set! vv 0)
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;Stop l'animation, ou la met en marche
(define STOP (new button%
                  (parent HPANEL)
                  (label "Go")
                  (callback (lambda (a b)
                              (if ACTION 
                                  (begin (send STOP set-label "Go")
                                         (set! ACTION #f)
                                         (action (send CANVAS get-dc)))
                                  (begin (send STOP set-label "Stop")
                                         (set! ACTION #t)
                                         (action (send CANVAS get-dc))))))))

;Recommence l'animation avec les paramètres courants
(define RESTART
  (new button%
       (parent HPANEL)
       (label "Restart")
       (callback (lambda (x y)
                   (init-world)
                   (set! ACTION #f)
                   (send STOP set-label "Go")
                   (send CANVAS on-paint)))))

;Ferme la fenêtre en cessant l'action
(define BUTTONBACK
  (new button%
       (parent HPANEL)
       (label "Fermer la fenêtre")
       (callback (lambda (x y)
                   (set! ACTION #f)
                   (send FRAME show #f)))))

;CANVAS ou se dessine l'action
(define CANVAS
  (new canvas%
       (parent VPANEL)
       (min-width WIDTH)
       (min-height HEIGHT)
       (style '())
       (paint-callback (lambda (obj dc)
                         (set! WIDTH (send obj get-width))
                         (set! HEIGHT (send obj get-height))
                         (action dc)))))

;Couleur de fond du CANVAS
(send (send CANVAS get-dc) set-background (make-object color% 194 116 31))

;Définition du centre de gravité du monde (barycentre des masses)
(define (centre-de-gravite monde)
  (let ((r (apply +vect (map (lambda (x) (*vect (send x m) (send x r))) WORLD))) (M (apply + (map (lambda (x) (send x m)) monde))))
    (new masse% (position (*vect (/ 1 M) r)) (couleur "red") (ray 2))))

;gestion du monde
(define (action dc)
  (if 
   ;si le monde bouge
   ACTION
   (begin
     ;on efface la scène précédente
     (send (send CANVAS get-dc) clear)
     ;on affiche le nouveau monde
     (for-each (lambda (x) (send x play dc)) WORLD)
     ;si spécifié, on affiche le centre de masse du monde
     (if AFFICHE-CENTRE? (send (centre-de-gravite WORLD) play dc))
     ;pour chaque élément du monde n'ayant pas subit de choc, on gère une éventuelle collision
     ;avec une autre masse du monde
     (for-each 
      (lambda (x) (when (not (send x marqueur)) 
                    (send x chocs (send x liste-chocs WORLD))))
      WORLD)
     ;ce traitement fait, on fait évoluer chacune des boules
     (for-each (lambda (x) 
                 (send x change 500 500 GRAVITE)
                 (send x set-marqueur! #f)) WORLD)
     (sleep/yield CLOCK)
     (action dc))
   ;si le monde ne bouge pas
   (begin
     ;on affiche simplement le monde
     (send (send CANVAS get-dc) clear)
     (if AFFICHE-CENTRE? (send (centre-de-gravite WORLD) play dc)) 
     (for-each (lambda (x) (send x play dc)) WORLD))))

;************************************

;             MODULE

;************************************

(define masseb 3)
(define vb -3)
(define massev 3)
(define vv 0)

(define (masse r x y vx vy c)
  (set! WORLD (cons (new masse% (ray (* 20 r)) (masse r) (couleur c) (position (make-vect x y)) (vitesse (make-vect vx vy))) WORLD)))

(define (init-world)
  (set! WORLD '())
  (masse masseb 400 280 vb 0 "blue")
  (masse massev 200 220 vv 0 "green"))

(init-world)
(send FRAME show #t)
(send CANVAS on-paint)
