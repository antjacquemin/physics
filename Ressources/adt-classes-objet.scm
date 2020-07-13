
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;             "adt-classes-objet.scm"
;   définition des deux classes utilisées dans la
;             partie orientée objet

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
    (define (liste-chocs LISTE)
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
             (if (choc? (car L))
                 ;on ajoute le premier élément de la liste à acc
                 (iter (cdr L) (cons (car L) acc))
                 ;sinon, on itère sur la fin de la liste
                 (iter (cdr L) acc)))))
      (iter LISTE '()))
    ;il y a choc entre deux boules si elles sont trop proches l'une de l'autre (pour l'exemple boules multiples) :
    (define (choc-multiples? A) (< (norme (+vect position (*vect -1 (send A r))))
                         (+ ray (send A rayon))))
    ;liste-chocs renvoie la liste des masses au contact de la masse courante (pour l'exemple boules multiples) :
    (define/public (liste-chocs-multiples LISTE)
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
             (if (choc-multiples? (car L))
                 ;on ajoute le premier élément de la liste à acc
                 (iter (cdr L) (cons (car L) acc))
                 ;sinon, on itère sur la fin de la liste
                 (iter (cdr L) acc)))))
      (iter LISTE '()))
    (define (minimum_liste L)
      (define (iter L_iter d acc)
        (if (null? L_iter)
            acc
            (let ((d_courant (norme (-vect position (send (car L_iter) r)))))
              (if (<= d_courant d)
                  (iter (cdr L_iter) d_courant (car L))
                  (iter (cdr L_iter) d acc)))))
      (iter (cdr L) (norme (-vect position (send (car L) r))) (car L)))
    ;traitement des chocs entre la boule et le reste du monde
    (define/public (chocs L)
      (let (;soit liste_chocs la liste des boules en contact avec la boule courante
            (liste_chocs (liste-chocs L)))
        (when 
            ;si la boule est en collision avec une autre
            (not (null? liste_chocs))
          (let* (;masse_au_contact est la masse avec laquelle la boule courante 
                 ;entre en contact
                 (masse_au_contact (minimum_liste liste_chocs))
                 ;issue_du_choc est la paire contenant les vitesses des deux boules
                 ;après le choc
                 (issue_du_choc (chocx-objet this masse_au_contact)))
            ;ainsi que les marqueurs des deux masses
            (set! marq #t)
            (send masse_au_contact set-marqueur! #t)
            ;puis modification des vitesses selon issue_du_choc
            (set! vitesse (car issue_du_choc))
            (send masse_au_contact set-v! (cdr issue_du_choc))))))
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