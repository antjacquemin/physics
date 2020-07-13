

;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "ev.scm"
;  ce fichier est un des modules du slideshow
;  il ne fait appel à aucun autre fichier pour
;    être totalement indépendant et utilisé
;          indépendemment du projet
;       (difusion internet notamment)

; Ce fichier permet de sensibiliser l'utilisateur 
; à l'interet de raisonner sur l'enveloppe convexe 
; pour détecter les chocs contre les parois de la 
;            fenêtre d'affichage

;http://projetphysics.teria.org/Slideshow/Implementation_du_projet.html

;*****************************************************



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

;                     OUTILS 

;*****************************************************

(define (first L)
  (car L))

(define (second L)
  (cadr L))

;mod2pi : float -> float
;renvoit l'angle x dans l'intervalle [0;2pi[
(define (mod2pi x)
  (let ((2pi (* 2 pi)))
    (cond ((< x 0) (mod2pi (+ x 2pi)))
          ((<= 2pi x) (mod2pi (- x 2pi)))
          (else x))))

;*****************************************************

;                       PILES
;         On définit une structure de pile

;*****************************************************

;une pile posséde un champ size, sa taille, et un champ
;l la liste de ses éléments
(define-struct pile (size l))

;vides? : pile -> bool
;predicat de la pile vide
(define (vide? pile)
  (zero? (pile-size pile)))

;init-pile : () -> pile
;constructeur de pile
(define (init-pile)
  (make-pile 0 '()))

;push! object x pile -> (void)
;effet de bords sur la pile :
;ajout d'un élément à la tête de ses éléments
(define (push! x pile)
  (set-pile-size! pile (add1 (pile-size pile)))
  (set-pile-l! pile (cons x (pile-l pile))))

;pop! : pile* -> object
;effet de bords sur la pile non vide :
;perte du premier élément...
;qui est le résultat de la fonction
(define (pop! pile)
  (if (vide? pile)
      (error "Pile vide !")
      (begin 
        (set-pile-size! pile (- (pile-size pile) 1))
        (let ((copie (car (pile-l pile))))
          (set-pile-l! pile (cdr (pile-l pile)))
          copie))))

;elmt1 : pile* -> object
;renvoit le premier élément d'une pile non vide
(define (elmt1 pile)
  (if (vide? pile)
      (error "Pile vide !")
      (car (pile-l pile))))

;elmt2 : pile** -> object
;renvoit le second élément d'une liste ayant au moins deux éléments
(define (elmt2 pile)
  (if (<= (pile-size pile) 1)
      (error "Pas assez d'elements dans la pile !")
      (cadr (pile-l pile)))) 

;*****************************************************

;                MASSES ET POLYGONES
;          Les deux structures du fichier

;*****************************************************

;MASSES

(define-struct masse 
  (;la masse
   m
   ;le vecteur positions
   r
   ;l'angle formé par l'axe (Ox) avec
   ;le vecteur liant le centre de masse
   ;du polygone avec la masse
   alpha))

;POLYGONES

(define-struct polygone 
  (;taille du polygone (nombre de masses le constituant)
   size
   ;vecteur de toutes les masses du polygone
   poly
   ;centre de masse : barycentre des masses constituant
   ;le polygone (cm est une masse)
   cm
   ;vitesse angulaire du polygone (float)
   vang 
   ;vecteur vitesse de translation (vecteur)
   vtrans 
   ;moment d'inertie du polygone
   i
   ;enveloppe convexe du polygone :
   ;liste des indices des points de l'enveloppe convexe
   ;du polygone
   ev
   ;rayon d'un polygone : le rayon du plus petit cercle de centre
   ;le centre de masse contenant tout le polygone
   rayon))


;*****************************************************

;                ENVELOPPE CONVEXE 
;       Détermination de l'enveloppe convexe 
;                  d'un polygone

;*****************************************************

;indice-depart : polygone -> int
;renvoit l'entier correspondant à l'indice de la masse du polygone
;la plus en bas à droite
(define (indice-depart poly)
  ;liste-min renvoit la liste des points ayant l'ordonnée la plus petite
  (define (liste-min poly)
    (define (iter i liste-indices-min)
      (if (<= (polygone-size poly) i)
          ;si l'on a parcouru tout le polygone, on renvoit la liste cherchée
          liste-indices-min
          ;sinon, on compare les ordonnées du point courant et du premier élément de la liste
          ;en prêtant attention au fait qu'en scheme, l'axe (Oy) est dirigé vers le bas
          (let ((a (vect-y (masse-r (vector-ref (polygone-poly poly) i)))) 
                (b (vect-y (masse-r (vector-ref (polygone-poly poly) (car liste-indices-min))))))
            (cond 
              ;si le point est plus bas, on change la liste
              ((> a b) (iter (add1 i) (list i)))
              ;s'ils sont à la même hauteur, on ajoute le point a la liste
              ((= a b) (iter (add1 i) (cons i liste-indices-min)))
              ;sinon, on ne la modifie pas
              (else (iter (add1 i) liste-indices-min))))))
    (iter 1 (list 0)))
  ;ne reste plus qu'a selectionner dans la liste précédente le point d'abscisse maximale
  (define (select L acc)
    (if (null? L)
        acc
        (if (< (vect-x (masse-r (vector-ref (polygone-poly poly) acc))) 
               (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))))
            (select (cdr L) (car L))
            (select (cdr L) acc))))
  (let ((new (liste-min poly))) 
    (select (cdr new) (car new))))

;create-liste-angles : polygone x int x vecteur -> liste
;renvoit une liste de pair (i,alphai) associant à l'indice i d'une masse
;son angle avec le point initial pinit
(define (create-liste-angles poly init pinit)
  (let* ((L '()))
    ;dans le parcourt des masses du polygone, il faut veiller a ne pas selectionner le point initial
    (for i from 0 to (- init 1)
         (let ((vecteur (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly) i))) (vect-x pinit))
                                   (- (vect-y pinit) (vect-y (masse-r (vector-ref (polygone-poly poly) i)))))))
           (if
            ;si le point courant est à la même hauteur que pinit, il se trouve a sa gauche
            (zero? (vect-y vecteur))
            (set! L (cons (cons i pi) L))
            (set! L (cons (cons i (vect-angle vecteur)) L)))))
    ;on parcourt ensuite les points après pinit
    (for i from (add1 init) to (- (polygone-size poly) 1)
         (let ((vecteur (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly) i))) (vect-x pinit))
                                   (- (vect-y pinit) (vect-y (masse-r (vector-ref (polygone-poly poly) i)))))))
           (if (zero? (vect-y vecteur))
               (set! L (cons (cons i pi) L))
               (set! L (cons (cons i (vect-angle vecteur)) L)))))
    L))

;trier-selon-angles : polygone x list x vecteur -> liste
;renvoit la liste des indices des points différents de pinit
;triés par ordre croissant des angles précédemment calculés
;(il s'agit donc d'un tri sur les cdr de la A-liste précédente.
;on implémente le tri fusion en nlog(n), et, dans le cas de plusieurs masses 
;ayant le même angle, on ne tient compte qu ed ela masse la plus éloignée de pinit
(define (trier-selon-angles poly L pinit)
  ;scission scinde une liste en deux listes tels que
  ;(sizeL1 - sizeL2) = 1 ou 0
  (define (scission L)
    ;iterativement
    (define (iter L L1 L2)
      ;si L est vide
      (if (null? L)
          ;on renvoit la pair (L1.L2)
          (cons L1 L2)
          (if (null? (cdr L))
              ;sinon, si L a un seul élément, auquel cas on ajoute le dernier élément à L1
              (cons (cons (car L) L1) L2)
              (iter (cddr L) (cons (car L) L1) (cons (cadr L) L2)))))
    (iter L '() '()))
  ;fusion est la fonction qui à partir de deux listes triées en construit une, comportant tous les
  ;éléments de la liste 1 et de la liste 2
  (define (fusion L1 L2)
    (define (iter L L1 L2)
      ;si L1 est nulle
      (if (null? L1)
          ;on concatene L et L2 apres un reverse (pour remettre L dans l'ordre croissant)
          (append (reverse L) L2)
          (if (null? L2)
              (append (reverse L) L1)
              ;sinon, il faut comparer les premiers éléments de L1 et L2
              (if (<= (cdar L1) (cdar L2))
                  (iter (cons (car L1) L) (cdr L1) L2)
                  (iter (cons (car L2) L) L1 (cdr L2))))))
    (iter '() L1 L2))
  ;la fonction suivante tri les masses selon leurs angles
  (define (tri L)
    (if (or (null? L) (null? (cdr L)))
        L
        (let* ((sciss (scission L)) (L1 (tri (car sciss))) (L2 (tri (cdr sciss))))
          (fusion L1 L2))))
  ;ne reste qu'à enlever à la liste triée les points de même angle selon le critère précédent 
  (define (kill-bis L)
    ;soit P une pile vide qui contiendra finalement la liste triée ne contenant que des points
    ;d'angles différents
    (let ((P (init-pile)))
      (define (iter L)
        (if (null? L)
            (pile-l P)
            (if (not (= (cdr (elmt1 P)) (cdar L)))
                ;si l'angle du premier élément de la pile est différent
                ;de celui de la première masse de la liste, on insére la masse dans P
                (begin (push! (car L) P)
                       (iter (cdr L)))
                ;sinon, on compare les distances entre les masses et pinit
                (if (< (norme (-vect (masse-r (vector-ref (polygone-poly poly) (car (elmt1 P)))) pinit))
                       (norme (-vect (masse-r (vector-ref (polygone-poly poly) (caar L))) pinit)))
                    ;si le point courant est plus éloigné, on enlève le premier élément de P
                    ;que l'on remplace par ce point
                    (begin (pop! P) (push! (car L) P) (iter (cdr L)))
                    ;sinon, on poursuit simplement l'itération
                    (iter (cdr L))))))
      (push! (car L) P)
      (iter L)))
  ;la nouvelle liste obtenue, on ne conserve que les indices, et non les angles
  (define (just-indices L)
    (define (iter L acc)
      (if (null? L)
          acc
          (iter (cdr L) (cons (caar L) acc))))
    (iter L '()))
  ;finalement, la fonction trier-selon-anglesse résume à la ligne :
  (just-indices (kill-bis (tri L))))

;enveloppe-convexe : polygone -> liste
;renvoit la liste des indices des masses du polygone formant son enveloppe convexe
(define (enveloppe-convexe poly)
  ;on utilise le prédicat a_gauche renvoyant #t si le point d'indice P est à droite de la droite 
  ;passant par les points d'indice A et B
  (define (a_gauche P A B)
    (let* ((PA (-vect (masse-r (vector-ref (polygone-poly poly) A))
                      (masse-r (vector-ref (polygone-poly poly) P))))
           (PB (-vect (masse-r (vector-ref (polygone-poly poly) B))
                      (masse-r (vector-ref (polygone-poly poly) P))))
           (d (- (* (vect-x PA) (vect-y PB)) (* (vect-x PB) (vect-y PA)))))
      (< 0 d)))
  ;si la taille du polygone est 1
  (if (= (polygone-size poly) 1)
      ;son enveloppe convexe est réduite à ce point
      (list 0)
      (let* ((init (indice-depart poly)) 
             (pinit (masse-r (vector-ref (polygone-poly poly) init))) 
             (EV (init-pile))
             (lpts (trier-selon-angles poly (create-liste-angles poly init pinit) pinit))
             (m (length lpts))
             (ppts (make-pile m lpts)))
        ;on initialise EV
        (push! init EV)
        (push! (pop! ppts) EV)
        ;tant que tous les points triés par angles n'ont pas été parcourus
        (while (not (vide? ppts))
               (let ((A (elmt1 EV)) (B (elmt2 EV)))
                 ;si le point courant ne perturbe aps la convexité de la chaine formée
                 (if (a_gauche (elmt1 ppts) A B)
                     ;on insére ce point dans la pile
                     (push! (pop! ppts) EV)
                     ;sinon, on dépile le premier élément
                     (pop! EV))))
        (pile-l EV))))


;*****************************************************

;                   POLYGONES
;   Constructeur, et fonctions sur les polygones

;*****************************************************

;make-poly : list x float x vecteur -> polygone
;renvoit le polygone formé de toutes les masses de la liste, 
;de vitesse angulaire vang et de vitesse de translation vtrans
(define (make-poly list-of-masses v-ang v-trans)
  ;initialisation du polygone sans son moment d'inertie ni son enveloppe convexe ni son rayon
  ;x et y sont des accumulateurs cummulant les coordonnées des masses du polygone
  ;masse est un accumulateur des masses des points du polygone
  ;taille compte le nombre de masses du polygone
  (define (iter L x y taille masse-totale)
    ;si l'on a parcouru toute la liste
    (if (null? L)
        (if 
         ;le cas de la masse infinie n'est a traité que pour la modélisation des chocs contre les parois
         (= +inf.0 masse-totale)
         (make-polygone taille 
                        (list->vector list-of-masses) 
                        ;ainsi, la position du centre de masse importe peu
                        (make-masse masse-totale (masse-r (car list-of-masses)) 0)
                        v-ang v-trans '? '? '?)
         ;si la masse est finie, on construit le polygone
         (make-polygone 
          ;de taille : taille
          taille 
          ;de poly : le vecteur obtenu directement à partir de la liste des masses
          (list->vector list-of-masses) 
          ;de centre de masse : le barycentre des (Pi,m_Pi), de masse la masse totale du polygone
          (make-masse masse-totale (make-vect (/ x masse-totale) (/ y masse-totale)) 0)
          v-ang v-trans '? '? '?))
        ;si l'on est en rtina de parcourir la liste
        (let ((m (masse-m (car L))))
          (iter (cdr L)
                ;on incrémente les accumulateurs en pondérant les coordonnées par la masse du point courant
                (+ x (* m (vect-x (masse-r (car L)))))
                (+ y (* m (vect-y (masse-r (car L)))))
                ;la taille augmente de 1
                (add1 taille) 
                ;et la masse de m
                (+ masse-totale m)))))
  ;soit new-poly le polygone construit par la fonction précédente
  (let* ((new-poly (iter list-of-masses 0. 0. 0 0.)) (centre (masse-r (polygone-cm new-poly))))
    ;on calcule le moment d'inertie du polygone :
    (define (calculi L i)
      (if (null? L)
          (set-polygone-i! new-poly i)
          (if (equal? +inf.0 (masse-m (car L)))
              ;on prête attention au cas d'une masse infinie (de moment infini)
              (begin (set-polygone-i! new-poly +inf.0) new-poly)
              ;utilisation de la formule i=sum(k=1..n) (m_k * r_k * r_k)
              (calculi (cdr L) (+ i (* 0.5 (sqr (norme (-vect centre (masse-r (car L))))) (masse-m (car L))))))))
    ;on initialise les champs alpha des masses
    (define (calculalpha poly)
      (for i from 0 to (- (polygone-size poly) 1)
           (set-masse-alpha! (vector-ref (polygone-poly poly) i)
                             ;en veillant à tenir compte de l'orientation de (Oy)
                             (mod2pi (vect-angle (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly) i)))
                                                               (vect-x (masse-r (polygone-cm poly))))
                                                            (- (vect-y (masse-r (polygone-cm poly)))
                                                               (vect-y (masse-r (vector-ref (polygone-poly poly) i))))))))))
    ;on calcule le rayon du polygone
    (define (rayon poly)
      (define (maxnorme i j)
        (let ((ri (norme (-vect (masse-r (polygone-cm poly)) (masse-r (vector-ref (polygone-poly poly) i)))))
              (rj (norme (-vect (masse-r (polygone-cm poly)) (masse-r (vector-ref (polygone-poly poly) j))))))
          (if (< ri rj)
              j
              i)))
      (do ((k 1 (add1 k)) (m 0 (maxnorme m k)))
        ((= k (polygone-size poly)) 
         (set-polygone-rayon! new-poly (norme (-vect (masse-r (polygone-cm poly)) (masse-r (vector-ref (polygone-poly poly) m))))))))
    (calculi list-of-masses 0)
    (calculalpha new-poly)
    (rayon new-poly)
    (set-polygone-ev! new-poly (enveloppe-convexe new-poly))
    ;ainsi, le polygone est construit
    new-poly))

;affichage-polygone : polygone x dc x color -> void
;demande au dc d'un canvas de dessiner dans ce dernier un polygone dans la couleur précisée
(define (affiche-polygone poly dc color type_polygone)
  (send dc set-pen color 1 'solid)
  ;dessine tous les segments de deux points consécutifs
  (for i from 0 to (- (polygone-size poly) 2)
       (send dc draw-line (vect-x (masse-r (vector-ref (polygone-poly poly) i)))
             (vect-y (masse-r (vector-ref (polygone-poly poly) i)))
             (vect-x (masse-r (vector-ref (polygone-poly poly) (add1 i))))
             (vect-y (masse-r (vector-ref (polygone-poly poly) (add1 i))))))
  ;liaison du premier et du dernier point du polygone
  (when (equal? type_polygone 'exemples)
    (send dc draw-line (vect-x (masse-r (vector-ref (polygone-poly poly) 0)))
          (vect-y (masse-r (vector-ref (polygone-poly poly) 0)))
          (vect-x (masse-r (vector-ref (polygone-poly poly) (- (polygone-size poly) 1))))
          (vect-y (masse-r (vector-ref (polygone-poly poly) (- (polygone-size poly) 1)))))))

;affiche-centre : polygone x dc x color -> void
;demande au dc d'un canvas de dessiner dans ce dernier le centre de masse d'un polygone dans la couleur précisée
(define (affiche-centre poly dc color)
  (send dc set-pen color 2 'solid)
  (send dc draw-point (vect-x (masse-r (polygone-cm poly))) (vect-y (masse-r (polygone-cm poly)))))

;affiche-ev : polygone x dc x color -> void
;demande au dc d'un canvas de dessiner dans ce dernier l'enveloppe convexe d'un polygone dans la couleur précisée
(define (affiche-ev poly dc color)
  (send dc set-pen color 1 'solid)
  (let* ((L  (polygone-ev poly)) (i (car L)) (l (car L)))
    (while (not (null? (cdr L)))
           (send dc draw-line (vect-x (masse-r (vector-ref (polygone-poly poly) (car L))))
                 (vect-y (masse-r (vector-ref (polygone-poly poly) (car L))))
                 (vect-x (masse-r (vector-ref (polygone-poly poly) (cadr L))))
                 (vect-y (masse-r (vector-ref (polygone-poly poly) (cadr L)))))
           (set! L (cdr L))
           (set! l (car L)))
    (send dc draw-line (vect-x (masse-r (vector-ref (polygone-poly poly) i)))
          (vect-y (masse-r (vector-ref (polygone-poly poly) i)))
          (vect-x (masse-r (vector-ref (polygone-poly poly) l)))
          (vect-y (masse-r (vector-ref (polygone-poly poly) l))))))

;affiche-cercle : polygone x dc x color -> void
;demande au dc d'un canvas de dessiner dans ce dernier le cercle de rayon (polygone-rayon poly) d'un polygone dans la couleur précisée
;(son centre est le centre de masse du polygone)
(define (affiche-cercle poly dc color)
  (send dc set-pen color 1 'solid)
  (send dc draw-ellipse 
        (- (vect-x (masse-r (polygone-cm poly))) (polygone-rayon poly))
        (- (vect-y (masse-r (polygone-cm poly))) (polygone-rayon poly))
        (* 2 (polygone-rayon poly))
        (* 2 (polygone-rayon poly))))

;minx : polygone -> int
;renvoit l'indice de la masse du polygone dont l'abscisse est minimale
;complexite lineaire
(define (minx poly)
  (define (iterev L indice-min)
    (if (null? L)
        indice-min
        (if (< (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))) 
               (vect-x (masse-r (vector-ref (polygone-poly poly) indice-min))))
            (iterev (cdr L) (car L))
            (iterev (cdr L) indice-min))))
  (iterev (polygone-ev poly) 0))

;maxx : polygone -> int
;renvoit l'indice de la masse du polygone dont l'abscisse est maximale
;complexite lineaire
(define (maxx poly)
  (define (iterev L indice-max)
    (if (null? L)
        indice-max
        (if (> (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))) 
               (vect-x (masse-r (vector-ref (polygone-poly poly) indice-max))))
            (iterev (cdr L) (car L))
            (iterev (cdr L) indice-max))))
  (iterev (polygone-ev poly) 0))

;miny : polygone -> int
;renvoit l'indice de la masse du polygone dont l'ordonnée est minimale
;complexite lineaire
(define (miny poly)
  (define (iterev L indice-min)
    (if (null? L)
        indice-min
        (if (< (vect-y (masse-r (vector-ref (polygone-poly poly) (car L)))) 
               (vect-y (masse-r (vector-ref (polygone-poly poly) indice-min))))
            (iterev (cdr L) (car L))
            (iterev (cdr L) indice-min))))
  (iterev (polygone-ev poly) 0))

;maxy : polygone -> int
;renvoit l'indice de la masse du polygone dont l'ordonnée est maximale
;complexite lineaire
(define (maxy poly )
  (define (iterev L indice-max)
    (if (null? L)
        indice-max
        (if (> (vect-y (masse-r (vector-ref (polygone-poly poly) (car L)))) 
               (vect-y (masse-r (vector-ref (polygone-poly poly) indice-max))))
            (iterev (cdr L) (car L))
            (iterev (cdr L) indice-max))))
  (iterev (polygone-ev poly) 0))


;*****************************************************

;                   MODULE

;*****************************************************

;par défaut, l'action est arrêtée
(define ACTION #f)

(define (action dc)
  (send dc clear)
  (for-each (lambda (x) (affiche-polygone x dc "blue" 'ligne_polygonale)
              (when AFFICHE-EV (affiche-ev x dc "magenta")))
            POLYGONE)
  (when (not (null? POLYGONE))
    (send (send CANVAS get-dc) draw-text (string-append "Nombre de points du polygone : " 
                                                        (number->string (polygone-size (car POLYGONE))))
          10 10)
    (send (send CANVAS get-dc) draw-text (string-append "Nombre de points de l'ev : " 
                                                        (number->string (length (polygone-ev (car POLYGONE)))))
          10 30))
  (sleep/yield (/ 1 1.))
  (when ACTION (action dc)))

;par défaut, on n'affiche pas l'enveloppe convexe
(define AFFICHE-EV #f)

;marqueur de l'enfoncement du bouton de la souris
(define V #f)

;pile pour la formation du polygone
(define L (init-pile))

;initialisation du polygone
(define POLYGONE '())

;affichage de la pile lors de la formation du polygone
(define (affiche-liste pile dc)
  (send dc set-pen "blue" 1 'solid)
  (let ((size (pile-size pile)) (liste (pile-l pile)))
    (if (= size 1)
        (send dc draw-point (vect-x (masse-r (car liste))) (vect-y (masse-r (car liste))))
        (do ((i 0 (add1 size)) (l liste (cdr l)))
          ((>= i size))
          (send dc draw-line (vect-x (masse-r (first l))) (vect-y (masse-r (first l)))
                (vect-x (masse-r (second l))) (vect-y (masse-r (second l))))))))

;création d'une nouvelle classe canvas prenant en compte
;le clic de la souris pour dessiner à main levée un polygone
(define my-canvas%
  (class canvas%
    (define/override (on-event evt)
      (case (send evt get-event-type)
        ((left-down) 
         ;lorsqu'il y a un clic, que le polygone n'est pas encore défini
         (when (null? POLYGONE)
                       (let ((x (max 10 (min 490 (send evt get-x))))
                             (y (max 50 (min 490 (send evt get-y)))))
                         ;on ajoute le point courant (éventuellement corrigé pour rester dans la fenêtre)
                         ;à la liste de la pile
                         (push! (make-masse 1 (make-vect x y) 0) L)
                         ;on affiche la pile
                         (affiche-liste L (send this get-dc))
                         ;et l'on marque le témoin de clic
                         (set! V #t))))
        ((left-up) (when
                       ;lorsque l'on lève le clic, et que le polygone n'a pas été défini
                       (and (null? POLYGONE) 
                            (not (vide? L)))
                     ;on annule le témoin de clic
                     (set! V #f)
                     ;on lance l'action
                     (set! ACTION #t)
                     ;on crée un polygone à partir de la pile
                     (set! POLYGONE (cons (make-poly (pile-l L) 0 (make-vect 0 0)) POLYGONE))
                     ;on réinitialise la pile
                     (set! L (init-pile))
                     (action (send this get-dc))))
        (else (when V
                ;lorsque l'utilisateur ajoute des points en maintenant le clic enfoncé
                ;il faut éventuellement corrigé la position des points et les ajouter à la pile
                (set! ACTION #f)
                (let ((x (max 10 (min 490 (send evt get-x))))
                      (y (max 50 (min 490 (send evt get-y)))))
                  (push! (make-masse 1 (make-vect x y) 0) L)
                  (affiche-liste L (send this get-dc)))))))
    (super-new)))

;*****************************************************

;              INTERFACE GRAPHIQUE

;*****************************************************

;frame principale du module
(define FRAME
  (new frame%
       (label "Polygones")
       (style '(hide-menu-bar metal))))

;panel du canvas
(define VPANEL
  (new vertical-panel%
       (parent FRAME)))

;panel des boutons
(define HPANEL
  (new horizontal-panel%
       (parent FRAME)
       (alignment '(center center))))

;cancas du dessin
(define CANVAS
  (new my-canvas% 
       (parent VPANEL)
       (min-width 500)
       (min-height 500)
       (paint-callback
        (lambda (obj dc)
          (when ACTION (action dc))))))

;gestion du double buffer
(define BITMAP (make-object bitmap% (send CANVAS get-width) (send CANVAS get-height))) 
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP))) 

;boutton pour activer l'affichage de l'enveloppe convexe (irréversible)
(define AFFICHEEV
  (new button%
       (parent HPANEL)
       (label "Affiche EV")
       (callback
        (lambda (obj evt)
          (set! AFFICHE-EV #t)
          (send CANVAS on-paint)))))

;bonton pour recommencer une figure
(define CLEAR
  (new button%
       (parent HPANEL)
       (label "Clear")
       (callback
        (lambda (obj evt)
          (set! POLYGONE '())
          (set! AFFICHE-EV #f)
          (send CANVAS on-paint)))))

;bouton pour quitter le module
(define STOP
  (new button%
       (parent HPANEL)
       (label "Fermer la fenêtre")
       (callback
        (lambda (obj evt)
          (set! ACTION #f)
          (send FRAME show #f)))))


(send FRAME show #t)
