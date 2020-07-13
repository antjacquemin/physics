
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "collisions.scm"
;ce fichier gère les partie "dessin a main levée" et
;      "exemples paramétrés" du fichier principal

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

;determinant : vecteur x vecteur -> float
;le déterminant de deux vecteurs
(define (determinant u v)
  (+ (* (vect-x u) (vect-y v)) (* (vect-y u) (vect-x v))))

;rotation : vecteur x vecteur x float ->vecteur
;renvoit les coordonnées de l'image du point uM par la rotation d'angle theta et de centre uA
(define (rotation uA uM theta)
  (let* (;on utilise la notation complexe :
         ;soit zA l'affixe du point uA
         (zA (+ (vect-x uA) (* +i (vect-y uA))))
         ;et zM l'affixe du point uM
         (zM (+ (vect-x uM) (* +i (vect-y uM)))) 
         ;alors z l'affixe du point image est donnée par la formule 
         (z (+ zA (* (exp (* theta +i)) (- zM zA)))))
    (make-vect (real-part z) (imag-part z))))

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
;     Les deux dernières structures du fichier

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
                 ;si le point courant ne perturbe pas la convexité de la chaine formée
                 (if (a_gauche (elmt1 ppts) A B)
                     ;on insére ce point dans la pile
                     (push! (pop! ppts) EV)
                     ;sinon, on dépile le premier élément
                     (pop! EV))))
        (pile-l EV))))

;*****************************************************

;                   COLLISIONS
;       Étude des collisions entre deux convexes

;*****************************************************

;mod2pi : float -> float
;renvoit l'angle x dans l'intervalle [0;2pi[
(define (mod2pi x)
  (let ((2pi (* 2 pi)))
    (cond ((< x 0) (mod2pi (+ x 2pi)))
          ((<= 2pi x) (mod2pi (- x 2pi)))
          (else x))))

;alphaext : polygone -> pair
;renvoit les deux entiers consécutifs (modulo la taille du polygone)
;qui forment un angle minimum et maximum avec (Ox)
(define (alphaext poly)
  ;min renvoit l'indice de la masse de plus petit angle parmis les deux indices soumis
  (define (min i j)
    (if (< (masse-alpha (vector-ref (polygone-poly poly) i)) 
           (masse-alpha (vector-ref (polygone-poly poly) j)))
        i
        j))
  ;max réalise le contraire
  (define (max i j)
    (if (> (masse-alpha (vector-ref (polygone-poly poly) i)) 
           (masse-alpha (vector-ref (polygone-poly poly) j)))
        i
        j))
  ;alphaext se déduit en parcourant tous les points du polygones 
  (do ((k 0 (add1 k)) (mini 0 (min mini k)) (maxi 0 (max maxi k)))
    ((= k (polygone-size poly)) (cons mini maxi))))

;secteur : int x polygone x polygone x pair -> pair
;i est l'indice de la masse de poly1 pour laquelle on cherche le secteur de poly2 auquel elle appartient
;renvoyé sous forme de pair
(define (secteur i poly1 poly2 alphaext2)
  (let* (;soit alpha l'angle entre (Ox) et le vecteur partant du centre de masse de poly2 et pointant
         ;vers le point de poly1 étudié (attention au sens de (Oy))
         (alpha (mod2pi (vect-angle (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly1) i)))
                                                  (vect-x (masse-r (polygone-cm poly2))))
                                               (- (vect-y (masse-r (polygone-cm poly2)))
                                                  (vect-y (masse-r (vector-ref (polygone-poly poly1) i)))))))) 
         (size2 (polygone-size poly2)) 
         ;soit alphamin et alphamax les indices des points de plus petit (respectivement plus grand)
         ;angles du poly2
         (alphamin (car alphaext2))
         (alphamax (cdr alphaext2))
         ;on définit un sens de construction de poly2 (qui aurait pu être déterminé à la construction de poly2
         ;mais pour ne pas surcharger la structure, nous préférons effectuer une comparaison simple en O(1)) :
         ;il est positif si, modulo size2, les angles augmentent avec l'indexation des masses de poly2, négatif sinon.
         ;comme pour un convexe, alphamin et alphamax sont consécutifs (modulo size2) nous avons le test :
         (sens (if (= (modulo (add1 alphamax) size2) alphamin) 'positif 'negatif)))
    ;on repère le secteur par dichotomie pour ne pas avoir une complexité en size1*size2
    ;a tout moment, j>=i
    (define (dicho i j)
      ;dicho dépend du sens de poly2
      (if (equal? sens 'positif)
          (cond ((= (- j i) 1)
                 ;si l'on est dans le cas de deux indices consécutifs on se demande si le point appartient à ce secteur
                 (if (and (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamin) size2))))
                          (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamin) size2)))))
                     (cons (modulo (+ i alphamin) size2) (modulo (+ j alphamin) size2))
                     #f))
                ;si les deux indices sont égaux, ce n'est pas un secteur
                ((= j i)
                 #f)
                ;sinon, soit le point appartient au secteur définit par i et j
                ((and (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamin) size2))))
                      (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamin) size2)))))
                 ;auquel cas on lance dicho sur i, 0.5(i+j), j
                 (let ((new (quotient (+ i j) 2)))
                   (or (dicho i new) (dicho new j))))
                ;sinon, le point considéré n'est pas dans ce secteur
                (else #f))
          ;on tient le même raisonnement lorsque le sens est négatif
          (cond ((<= (- j i) 1)
                 (if (and (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamax) size2))))
                          (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamax) size2)))))
                     (cons (modulo (+ i alphamax) size2) (modulo (+ j alphamax) size2))
                     #f))
                ((and (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamax) size2))))
                      (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamax) size2)))))
                 (let ((new (quotient (+ i j) 2)))
                   (or (dicho i new) (dicho new j))))
                (else #f))))
    ;finalement, soit le point appartient à la zone 1 (voir le slideshow)
    (if (or (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) alphamin)))
            (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) alphamax))))
        (cons alphamin alphamax) 
        ;ou alors on lance l'algorithme dicho
        (dicho 0 (- size2 1)))))

;caracterisation : pair x polygone -> list
;a partir du secteur (i,j) du polygone poly,
;caracterisation renvoit la liste (a b c) de telle sorte que
;ax+by+c=0 soit l'equation de la droite passant par les points d'indices i et j
(define (caracterisation secteur poly)
  (let ((P1 (masse-r (vector-ref (polygone-poly poly) (car secteur)))) 
        (P2 (masse-r (vector-ref (polygone-poly poly) (cdr secteur)))))
    (if 
     ;si les points ont même abscisses, l'équation de la droite est du type x=constante
     ;on prend donc a=1, b=0, et c= - x_P[i]
     (= (vect-x P1) (vect-x P2))
     (list 1 0 (- (vect-x P1)))
     ;sinon, l'équation est du type y=-ax-c
     ;on prend donc b=1, a l'opposé du coefficient directeur (yA-yB)/(xA-xB)
     ;et c l'opposé de l'ordonnée à l'origine
     (let* 
         (;coefficient directeur
          (a (/ (- (vect-y P1) (vect-y P2)) (- (vect-x P1) (vect-x P2)))) 
          ;ordonnée à l'origine
          (b (- (vect-y P1) (* a (vect-x P1)))))
       (list (- a) 1 (- b))))))

;distance : int x polygone x polygone x pair -> list
;distance renvoit une liste composée de la distance entre le ieme point de poly1
;au poly2, la caracterisation du secteur auquel appartient ce point (pour faire ensuite le calcul de la normale)
;ainsi que ce secteur, connaissant les indices des angles extrema de poly2
(define (distance i poly1 poly2 alphamax2)
  (let* (;soit r le vecteur position du ieme point de poly1
         (r (masse-r (vector-ref (polygone-poly poly1) i)))
         ;soit sect le secteur auquel il appartient
         (sect (secteur i poly1 poly2 alphamax2))
         ;soit caract les coefficients de la droite définit par le secteur de poly2
         (caract (caracterisation sect poly2)))
    (list 
     ;distance d'un point (xM,yM) a une droite d'équation ax+by+c=0 : d = |(axM+byM+c) / ((a*a+b*b)^(1/2))|
     (abs (/ (+ (* (first caract) (vect-x r)) (* (second caract) (vect-y r)) (third caract))
             (norme (make-vect (first caract) (second caract)))))
     caract
     sect)))

;normale : int x polygone x polygone x list x pair -> vecteur
;si le ieme point est en collision avec le segment définit par secteur du poly2
;normale renvoit le vecteur normale au secteur, de norme 1, pointant vers le polygone A
(define (normale i poly1 poly2 caract secteur)
  ;on réutilise le prédicat a-gauche, qui renvoit 1 lorsqu'il est vrai, -1 sinon
  (define (a-gauche P A B)
    (let* ((PA (-vect A P))
           (PB (-vect B P))
           (d (- (* (vect-x PA) (vect-y PB)) (* (vect-x PB) (vect-y PA)))))
      (cond ((< 0 d) 1)
            ((> 0 d) -1)
            (else 0))))
  (let* (;soit r la position du point en contact
         (r (masse-r (vector-ref (polygone-poly poly1) i)))
         ;v un vecteur normale à la droite définit par caract :
         ;si D : ax+by+c=0, alors n(a,b) est normale à D
         (v (make-vect (first caract) (second caract))) 
         ;soient u et w les vecteurs positions du secteur de poly2
         (u (masse-r (vector-ref (polygone-poly poly2) (car secteur))))
         (w (masse-r (vector-ref (polygone-poly poly2) (cdr secteur))))
         ;soit mil le milieu du segment [u,w]
         (mil (*vect 1/2 (+vect u w))))
    (if
     ;si le centre de masse de poly1 et mil+v sont du même côté du secteur
     (>= (* (a-gauche (masse-r (polygone-cm poly1)) u w) (a-gauche (+vect u v) u w)) 0)
     ;alors v pointe vers poly1 : on le retourne en rendant sa norme unitaire
     (*vect (/ 1 (norme v)) v)
     ;sinon, -v/||v|| convient
     (*vect (/ -1 (norme v)) v))))

;intersection : polygone x polygone x float -> bool U list
;si les deux polygones ne se rencontrent pas, intersection renvoit #f
;sinon, elle renvoit une liste (i polygone1 polygone2 n) telle que :
;i soit l'indice du point du polygone1 suffisemment proche de polygone2
;et n soit la normale au secteur de polygone2 pointant vers polygone1
(define (intersection poly1 poly2 h)
  (let ((RESULT #f))
    ;on parcourt tous les points de poly1
    (for i from 0 to (- (polygone-size poly1) 1)
         (let ((d (distance i poly1 poly2 (alphaext poly2))))
           ;si l'un des points est suffisemment proche de poly2
           (when (<= (first d) h)
             ;on modifie le BOOL en liste
             (set! RESULT (list i poly1 poly2 (normale i poly1 poly2 (second d) (third d)))))))
    ;on recommence pour les points du polygone2
    (for i from 0 to (- (polygone-size poly2) 1)
         (let ((d (distance i poly2 poly1 (alphaext poly1))))
           (when (<= (first d) h)
             (set! RESULT (list i poly2 poly1 (normale i poly2 poly1 (second d) (third d)))))))
    ;puis on retourne le résultat
    RESULT))

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

;lecture-polygone : polygone -> (void)
;fonction de debugage pour afficher les caractéristiques principales d'un polygone
(define (lecture-polygone poly)
  (printf "\nNombre de points : ~a\n" (polygone-size poly))
  (for i from 0 to (- (polygone-size poly) 1)
       (printf "P~a : " i)
       (vect-affiche (masse-r (vector-ref (polygone-poly poly) i)))
       (printf " de masse ~a et d'angle ~a\n" (masse-m (vector-ref (polygone-poly poly) i)) (masse-alpha (vector-ref (polygone-poly poly) i))))
  (printf "Le centre de masse est : ")
  (vect-affiche (masse-r (polygone-cm poly)))
  (printf "\nMasse : ~a" (masse-m (polygone-cm poly)))
  (printf "\nI : ~a\n" (polygone-i poly))
  (printf "vang : ~a\n" (polygone-vang poly))
  (printf "vtrans : ") (vect-affiche (polygone-vtrans poly))
  (newline) (newline))

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

;              EVOLUTION DES POLYGONES

;*****************************************************

;rotation-polygone : polygone -> (void)
;effet de bords sur le polygone
;fait pivoter toutes les masses du polygone de l'angle (polygone-poly vang)
(define (rotation-polygone poly)
  ;si la vitesse angulaire est nulle, rien n'est fait
  (when (not (zero? (polygone-vang poly)))
    ;pour chacune des masses
    (for i from 0 to (- (polygone-size poly) 1)
         ;on effectue une rotation
         (set-masse-r! (vector-ref (polygone-poly poly) i)
                       (rotation (masse-r (polygone-cm poly)) (masse-r (vector-ref (polygone-poly poly) i)) (polygone-vang poly)))
         ;et l'on met à jour l'angle alpha
         (set-masse-alpha! (vector-ref (polygone-poly poly) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly poly) i)) (polygone-vang poly)))))))

;translation-polygone : polygone -> (void)
;effet de bords sur le polygone
;fait translater toutes les masses du polygone du vecteur vtrans
(define (translation-polygone poly)
  ;si la vitesse de translation est nulle, il n'y  a rien à faire
  (when (not (and (zero? (vect-x (polygone-vtrans poly))) (zero? (vect-y (polygone-vtrans poly)))))
    ;sinon, on doit mettre a chaque chaque masse
    (for i from 0 to (- (polygone-size poly) 1)
         (set-masse-r! (vector-ref (polygone-poly poly) i)
                       (+vect (masse-r (vector-ref (polygone-poly poly) i)) (polygone-vtrans poly))))
    ;ainsi que le centre de masse
    (set-masse-r! (polygone-cm poly)
                  (+vect (masse-r (polygone-cm poly)) (polygone-vtrans poly)))))

;translation-polygone : polygone -> (void)
;effet de bords sur le polygone
;effet de la gravite (vecteur) sur le polygone
(define (gravite poly)
  (set-polygone-vtrans! poly (+vect (polygone-vtrans poly) GRAVITE)))

;choc : polygone x polygone x vecteur x vecteur x float -> (void)
;effet de bords sur poly1 et poly2
;modifie les vitesses angulaires et de translation des polygones suite a un choc
;au niveau à la position contact, avec une normale au choc pointant vers poly1
(define (choc poly1 poly2 contact normale e)
  (let* ((pi/2 (/ pi 2.))
         ;soient r1 et r2 les vecteurs du centre de masse (du poly1 et poly2 respectivement) au point de contact
         (r1 (-vect contact (masse-r (polygone-cm poly1)))) (r2 (-vect contact (masse-r (polygone-cm poly2))))
         ;soient r1ortho et r2ortho les vecteurs orthogonaux à r1 et r2
         (r1ortho (rotation (make-vect 0 0) r1 pi/2)) (r2ortho (rotation (make-vect 0 0) r2 pi/2))
         ;calcul de j selon la formule donnée dans le slideshow
         (j (/ (- (* (+ 1 e) (.scal (-vect (+vect (polygone-vtrans poly1) (*vect (polygone-vang poly1) r1ortho))
                                           (+vect (polygone-vtrans poly2) (*vect (polygone-vang poly2) r2ortho))) normale))) 
               (+ (/ (masse-m (polygone-cm poly1))) 
                  (/ (masse-m (polygone-cm poly2))) 
                  (/ (sqr (.scal normale r1ortho)) (polygone-i poly1)) 
                  (/ (sqr (.scal normale r2ortho)) (polygone-i poly2))))))
    ;modification des vitesses en conséquence
    (set-polygone-vang! poly1 (+ (polygone-vang poly1) (/ (* j (.scal normale r1ortho)) (polygone-i poly1))))
    (set-polygone-vang! poly2 (+ (polygone-vang poly2) (/ (* (- j) (.scal normale r2ortho)) (polygone-i poly2))))
    (set-polygone-vtrans! poly1 (+vect (polygone-vtrans poly1) (*vect (/ j (masse-m (polygone-cm poly1))) normale)))
    (set-polygone-vtrans! poly2 (+vect (polygone-vtrans poly2) (*vect (/ (- j) (masse-m (polygone-cm poly2))) normale)))
    (when (= +inf.0 (masse-m (polygone-cm poly2)))
      (if (zero? (vect-x normale))
          (begin 
            (set-polygone-vtrans! poly1 (make-vect (* e (vect-x (polygone-vtrans poly1))) (vect-y (polygone-vtrans poly1))))
            (set-polygone-vtrans! poly2 (make-vect (* e (vect-x (polygone-vtrans poly2))) (vect-y (polygone-vtrans poly2)))))
          (begin 
            (set-polygone-vtrans! poly1 (make-vect (vect-x (polygone-vtrans poly1)) (* e (vect-y (polygone-vtrans poly1)))))
            (set-polygone-vtrans! poly2 (make-vect (vect-x (polygone-vtrans poly2)) (* e (vect-y (polygone-vtrans poly2))))))))))

;comeback : polygone -> (void)
;effet de bords sur poly
;fait revenir le polygone poly à l'état précédent
(define (comeback poly)
  (set-polygone-vang! poly (* (polygone-vang poly) -1))
  (set-polygone-vtrans! poly (*vect -1 (polygone-vtrans poly)))
  (rotation-polygone poly)
  (translation-polygone poly)
  (set-polygone-vang! poly (* (polygone-vang poly) -1))
  (set-polygone-vtrans! poly (*vect -1 (polygone-vtrans poly))))

;lower : polygone -> (void)
;effet de bords sur poly
;ralentit l'évolution de poly d'un facteur 1/50
(define (lower poly)
  (set-polygone-vang! poly (* (polygone-vang poly) 1/50))
  (set-polygone-vtrans! poly (*vect 1/50 (polygone-vtrans poly))))

;faster : polygone -> (void)
;effet de bords sur poly
;accelere l'évolution de poly d'un facteur 50
(define (faster poly)
  (set-polygone-vang! poly (* (polygone-vang poly) 50))
  (set-polygone-vtrans! poly (*vect 50 (polygone-vtrans poly))))

;next : polygone x float -> (void)
;effet de bords sur poly
;gère l'évolution d'un polygone seul, le paramètre e est le coefficient d'élasticité
(define (next poly e)
  ;on calcule les indices de ses points extrema
  (let ((ymax (maxy poly)) (ymin (miny poly)) (xmax (maxx poly)) (xmin (minx poly)) (etat 'mystere))
    (cond (;collision mur bas
           (>= (vect-y (masse-r (vector-ref (polygone-poly poly) ymax))) 500)
           ;on annule l'évolution du polygone, puis on le ralentit
           (comeback poly)
           (lower poly)
           ;tant que le polygone n'est pas suffisemment proche du mur bas
           (while (< (vect-y (masse-r (vector-ref (polygone-poly poly) (maxy poly)))) 499)
                  ;on le fait évoluer
                  (rotation-polygone poly)
                  (translation-polygone poly))
           ;on rétablit sa vitesse réelle
           (faster poly)
           (let* ((indymax (maxy poly)) 
                  ;on définit le point de contact comme étant le point d'ordonnée maximale du polygone
                  (ptcontact (make-vect (vect-x (masse-r (vector-ref (polygone-poly poly) indymax)))
                                        (vect-y (masse-r (vector-ref (polygone-poly poly) indymax)))))
                  (correction (abs (- 500 (vect-y (masse-r (vector-ref (polygone-poly poly) indymax)))))))  
             ;pour traiter le choc contre le mur, on utilise la fonction choc entre poly et un polygone de masse
             ;infinie située au point de contact
             (choc poly 
                   (make-poly (list (make-masse +inf.0 ptcontact 0)) 0 (make-vect 0 0))
                   ptcontact
                   (make-vect 0 -1)
                   e)
             (for i from 0 to (- (polygone-size poly) 1)
                  (set-masse-r! (vector-ref (polygone-poly poly) i)
                                (+vect (masse-r (vector-ref (polygone-poly poly) i)) (make-vect 0 (- correction)))))
             ;ainsi que le centre de masse
             (set-masse-r! (polygone-cm poly)
                           (+vect (masse-r (polygone-cm poly)) (make-vect 0 (- correction))))))
          ;on traite de même le mur haut
          (;collision mur haut
           (<= (vect-y (masse-r (vector-ref (polygone-poly poly) ymin))) 0)
           ;on annule l'évolution du polygone, puis on le ralentit
           (comeback poly)
           (lower poly)
           ;tant que le polygone n'est pas suffisemment proche du mur haut
           (while (> (vect-y (masse-r (vector-ref (polygone-poly poly) (miny poly)))) 1)
                  ;on le fait évoluer
                  (rotation-polygone poly)
                  (translation-polygone poly))
           ;on rétablit sa vitesse réelle
           (faster poly)
           (let* ((indymin (miny poly)) 
                  ;on définit le point de contact comme étant le point d'ordonnée minimale du polygone
                  (ptcontact (make-vect (vect-x (masse-r (vector-ref (polygone-poly poly) indymin)))
                                        (vect-y (masse-r (vector-ref (polygone-poly poly) indymin)))))
                  (correction (abs (vect-y (masse-r (vector-ref (polygone-poly poly) indymin))))))  
             ;pour traiter le choc contre le mur, on utilise la fonction choc entre poly et un polygone de masse
             ;infinie située au point de contact
             (choc poly 
                   (make-poly (list (make-masse +inf.0 ptcontact 0)) 0 (make-vect 0 0))
                   ptcontact
                   (make-vect 0 1)
                   e)
             (for i from 0 to (- (polygone-size poly) 1)
                  (set-masse-r! (vector-ref (polygone-poly poly) i)
                                (+vect (masse-r (vector-ref (polygone-poly poly) i)) (make-vect 0 correction))))
             ;ainsi que le centre de masse
             (set-masse-r! (polygone-cm poly)
                           (+vect (masse-r (polygone-cm poly)) (make-vect 0 correction)))))
          ;on traite de même le mur droit
          (;collision mur droit
           (>= (vect-x (masse-r (vector-ref (polygone-poly poly) xmax))) 500)
           ;on annule l'évolution du polygone, puis on le ralentit
           (comeback poly)
           (lower poly)
           ;tant que le polygone n'est pas suffisemment proche du mur droit
           (while (< (vect-x (masse-r (vector-ref (polygone-poly poly) (maxx poly)))) 499)
                  ;on le fait évoluer
                  (rotation-polygone poly)
                  (translation-polygone poly))
           ;on rétablit sa vitesse réelle
           (faster poly)
           (let* ((indxmax (maxx poly)) 
                  ;on définit le point de contact comme étant le point d'abscisse maximale du polygone
                  (ptcontact (make-vect (vect-x (masse-r (vector-ref (polygone-poly poly) indxmax)))
                                        (vect-y (masse-r (vector-ref (polygone-poly poly) indxmax)))))
                  (correction (abs (- 500 (vect-x (masse-r (vector-ref (polygone-poly poly) indxmax)))))))  
             ;pour traiter le choc contre le mur, on utilise la fonction choc entre poly et un polygone de masse
             ;infinie située au point de contact
             (choc poly 
                   (make-poly (list (make-masse +inf.0 ptcontact 0)) 0 (make-vect 0 0))
                   ptcontact
                   (make-vect -1 0)
                   e)
             (for i from 0 to (- (polygone-size poly) 1)
                  (set-masse-r! (vector-ref (polygone-poly poly) i)
                                (+vect (masse-r (vector-ref (polygone-poly poly) i)) (make-vect (- correction) 0))))
             ;ainsi que le centre de masse
             (set-masse-r! (polygone-cm poly)
                           (+vect (masse-r (polygone-cm poly)) (make-vect (- correction) 0)))))
          ;on traite de même le mur gauche
          (;collision mur gauche
           (<= (vect-x (masse-r (vector-ref (polygone-poly poly) xmin))) 0)
           ;on annule l'évolution du polygone, puis on le ralentit
           (comeback poly)
           (lower poly)
           ;tant que le polygone n'est pas suffisemment proche du mur gauche
           (while (> (vect-x (masse-r (vector-ref (polygone-poly poly) (minx poly)))) 1)
                  ;on le fait évoluer
                  (rotation-polygone poly)
                  (translation-polygone poly))
           ;on rétablit sa vitesse réelle
           (faster poly)
           (let* ((indxmin (minx poly)) 
                  ;on définit le point de contact comme étant le point d'abscisse minimale du polygone
                  (ptcontact (make-vect (vect-x (masse-r (vector-ref (polygone-poly poly) indxmin)))
                                        (vect-y (masse-r (vector-ref (polygone-poly poly) indxmin)))))
                  (correction (abs (vect-x (masse-r (vector-ref (polygone-poly poly) indxmin))))))  
             ;pour traiter le choc contre le mur, on utilise la fonction choc entre poly et un polygone de masse
             ;infinie située au point de contact
             (choc poly 
                   (make-poly (list (make-masse +inf.0 ptcontact 0)) 0 (make-vect 0 0))
                   ptcontact
                   (make-vect 1 0)
                   e)
             (for i from 0 to (- (polygone-size poly) 1)
                  (set-masse-r! (vector-ref (polygone-poly poly) i)
                                (+vect (masse-r (vector-ref (polygone-poly poly) i)) (make-vect correction 0))))
             ;ainsi que le centre de masse
             (set-masse-r! (polygone-cm poly)
                           (+vect (masse-r (polygone-cm poly)) (make-vect correction 0)))))
          ;s'il n'y a pas de choc, le polygone est soumis à la gravité
          (else 
           ;on fait evoluer le polygone
           (rotation-polygone poly)
           (translation-polygone poly)
           ;on modifie la vitesse du polygone
           (gravite poly)))))

;evolution : list -> void
;effet de bords sur les deux polygones de la liste
;on rappelle que la fonction intersection renvoit une liste (i polygone1 polygone2 n) telle que :
;i soit l'indice du point du polygone1 suffisemment proche de polygone2
;et n soit la normale au secteur de polygone2 pointant vers polygone1
(define (evolution d)
  (let ((v (vector-ref (polygone-poly (second d)) (first d))))
    (while (intersection (second d) (third d) 0.75)
           (comeback (second d))
           (comeback (third d)))
    (choc (second d) (third d) (masse-r v) (fourth d) 1)))

;*****************************************************

;                      CONSTANTES

;*****************************************************

(define GRAVITE (make-vect 0 0.01))
(define E 1)


;*****************************************************

;        CREATIONS DES POLYGONES A MAIN LEVEE

;*****************************************************


;Soit L une pile vide
(define L (init-pile))

;Soit (polygone-nul) un générateur de polygone nul
(define (polygone-nul) 
  (make-poly (list (make-masse 1 (make-vect -10 -10) 0))
             0 (make-vect 0 0)))

;Soit (polygone-nul? P) un prédicat disant si oui ou non P est nul
(define (polygone-nul? P) 
  (and (= (polygone-size P) 1) 
       (= -10 (vect-x (masse-r (polygone-cm P))))
       (= -10 (vect-y (masse-r (polygone-cm P))))))

;affiche-liste : pile x dc x color -> (void)
;demande au dc de dessiner la ligne polygonale contenu dans la lsite de la pile passée en argument
;dans la couleur spécifiée
(define (affiche-liste pile dc color)
  (send dc set-pen color 1 'solid)
  (let ((size (pile-size pile)) (liste (pile-l pile)))
    (if (= size 1)
        (send dc draw-point (vect-x (masse-r (car liste))) (vect-y (masse-r (car liste))))
        (do ((i 0 (add1 size)) (l liste (cdr l)))
          ((>= i size))
          (send dc draw-line (vect-x (masse-r (first l))) (vect-y (masse-r (first l)))
                (vect-x (masse-r (second l))) (vect-y (masse-r (second l))))))))

;ajouter-point : int x int -> (void)
;effet de bords sur la pile L
;ajoute au sommet de la pile la masse de masse 1 de position (x,y)
(define (ajouter-point x y)
  (push! (make-masse 1 (make-vect x y) 0) L))

;creer-poly : () -> polygone
;creer le polygone avec les masses contenues dans la liste de la pile L
(define (creer-poly)
  (set! POLYGONE  (make-poly (pile-l L) 0 (make-vect 0 0)))
  (set! L (init-pile)))

