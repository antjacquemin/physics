
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                "interface.scm"
;ce fichier gère toute l'interface graphique du soft

;*****************************************************



;*****************************************************

;              LIBRAIRIES NECESSAIRES 

;*****************************************************

(require (lib "texpict.ss" "texpict") ; pour les images
         (lib "sendurl.ss" "net")
         (lib "process.ss")) ; pour le lien Internet

;*****************************************************

;                 FICHIERS A CHARGER

;*****************************************************


(load (build-path (current-directory) "Ressources" "collisions.scm"))
(load (build-path (current-directory) "Ressources" "objet.scm"))

;*****************************************************

;                  METHODES PRIVEES

;*****************************************************

; change-fils : area-container% x list (of subwindows%) -> void
; Remplace les fils courants du père conteneur par la nouvelle liste de fils 
(define (change-fils father Lchildren)
  (for-each (lambda (child)
              (send father delete-child child))
            (send father get-children))
  (for-each (lambda (child)
              (send father add-child child))
            Lchildren))

; couleur : string -> color%
; Crée une couleur
(define (couleur str)
  (make-object color% str))

;*****************************************************

;                  VARIABLES PRIVEES

;*****************************************************

(define PI/2 (/ pi 2))

;; Pour le dessin

(define POLYGONE (polygone-nul)) ; Polygone pour le dessin
(define AFFICHE_EC #f) ; Indicateur de l'affichage de l'enveloppe convexe
(define AFFICHE_CM #f) ; Indicateur de l'affichage du centre de masse
(define COLOR_POLY (couleur "black")) ; Couleur du polygone
(define COLOR_EC (couleur "blue")) ; Couleur de l'enveloppe convexe
(define COLOR_CM (couleur "red")) ; Couleur du centre de masse
(define COLOR_BACK (couleur "azure")) ; Couleur de fond du canvas
(define IND_CLIC #f) ; Indicateur de clic enfoncé dans le canvas
(define COMPT 0) ; Compteur de points (modulo 2) pour le tracé
(define IND_ACTION #f) ; Indicateur d'animation (via le bouton d'activation)

;; Pour les exemples

(define POLYGONE_EXS (polygone-nul)) ; Polygone pour les exemples
(define ANGLE PI/2) ; Angle de l'exemple en radians
(define LONG_BAR 150) ; Longueur de la barre
(define TAILLE_INF 4) ; Taille de la boucle infinie
(define TAILLE_ELL 100) ; Taille de l'ellipse
(define TORSADE_ESC 8) ; Torsade de l'escargot
(define NB_BRAN_STAR 12) ; Nombre de branches de l'étoile
(define TAILLE_HALT 3) ; Taille de la boule du bas de l'haltère
(define NB_COTES_POLYREG 5) ; Nombre de côtés du polygone régulier
(define COURB_QUA 50) ; Courbure du quadrillage

(define CART_X "(expt (sin t) 3)") ; Paramétrisation cartésienne selon x en t
(define CART_Y "(- (cos t) (expt (cos t) 4))") ; Paramétrisation cartésienne selon y en t
(define CART_INF "(- pi)") ; Borne inférieure de la paramétrisation cartésienne
(define CART_SUP "pi") ; Borne supérieure de la paramétrisation cartésienne
(define POL_RHO "(+ (- (exp (cos t)) (* 2 (cos (* 4 t)))) (expt (sin (/ t 12)) 5))") ; Paramétrisation polaire selon rho en t
(define POL_INF "(- pi)") ; Borne inférieure de la paramétrisation polaire
(define POL_SUP "pi") ; Borne supérieure de la paramétrisation polaire

;; Pour les objets

(define AFFICHE_OBJ_CM #f) ; Indicateur de l'affichage du centre de masse
(define COLOR_OBJ_BACK (make-object color% 194 116 31)) ; Couleur de fond du canvas objet
(define M1_2B 1) ; Masse de la première boule dans Deux boules
(define M2_2B 1) ; Masse de la seconde boule dans Deux boules
(define V1_2B (make-vect -5 0)) ; Vitesse de la première boule dans Deux boules
(define V2_2B (make-vect 0 0)) ; Vitesse de la seconde boule dans Deux boules
(define R1_2B 60) ; Rayon de la première boule dans Deux boules
(define R2_2B 60) ; Rayon de la seconde boule dans Deux boules
(define NB_BAL 20) ; Nombre de boules dans Boules multiples
(define M_GRAV 1) ; Masse de la boule dans Gravite
(define V_GRAV (make-vect -1 0)) ; Vitesse de la boule dans Gravite
(define R_GRAV 50) ; Rayon de la boule dans Gravite
(define NB_PEND 5) ; Nombre de boules dans Pendule

;*****************************************************

;        STRUCTURE DE L'INTERFACE GRAPHIQUE

;*****************************************************

; Fenêtre principale
(define FRAME1 (new frame% 
                    (label "PHYSICS") 
                    (style '(metal)))) ; affichage métallisé (seulement sur MAC OS X)
;(stretchable-height #f)))

; Panneau horizontal
(define HPANEL1 (new horizontal-panel%
                     (parent FRAME1)))

; Panneau vertical contenant les règlages
(define VPANEL1 (new vertical-panel%
                     (parent HPANEL1)
                     (alignment '(center center))))

; Panneau vertical contenant le canvas
(define VPANEL2 (new vertical-panel%
                     (parent HPANEL1)
                     (alignment '(center center))))


;*****************************************************

;                 GESTION DES MENUS

;*****************************************************

;;;; Barre des menus
(define BARRE_MENU (new menu-bar% (parent FRAME1)))

;;; Menus

;;; Menu Fichier
(define MENU_FICH (new menu% 
                       (label "Fichier")
                       (parent BARRE_MENU)
                       (help-string "Lance une démo")))

;;; Menu Affichage
(define MENU_AFF (new menu% 
                      (label "Affichage")
                      (parent BARRE_MENU)))

;;; Menu Aide
(define MENU_AIDE (new menu% 
                       (label "Aide")
                       (parent BARRE_MENU)))

;; Sous-menus du menu Fichier

;;  Sous-menu Menu
(define FICH_MENU (new menu-item%
                       (label "Menu")
                       (parent MENU_FICH)
                       (shortcut #\m)
                       (callback (lambda (item evt)
                                   (send BITMAP-DC set-background COLOR_BACK)
                                   (set! IND_ACTION #f)
                                   (send AFF_TRA enable #t)
                                   (send AFF_CM enable #t)
                                   (send AFF_EC enable #t)
                                   (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                   (change-fils VPANEL1 (list GPANEL_INIT_SELECT GPANEL_INIT_DOC))
                                   (change-fils VPANEL2 (list CANVAS_INIT))))
                       (help-string "Retourne au menu")))

;;  Sous-menu Dessin
(define FICH_DRAW (new menu-item%
                       (label "Dessin")
                       (parent MENU_FICH)
                       (shortcut #\d)
                       (callback (lambda (item evt)
                                   (send BITMAP-DC set-background COLOR_BACK)
                                   (set! GRAVITE (make-vect 0 0.01))
                                   (send SLIDER_DRAW_G set-value 100)
                                   (set! E 0.95)
                                   (send SLIDER_DRAW_E set-value 95)
                                   (set! POLYGONE (polygone-nul))
                                   (set! AFFICHE_EC #f)
                                   (send CBOX_DRAW_EC set-value #f)
                                   (set! AFFICHE_CM #f)
                                   (send CBOX_DRAW_CM set-value #f)
                                   (set! COMPT 0)
                                   (set! IND_ACTION #f)
                                   (send BUTTON_DRAW_ACTION set-label "Go")
                                   (send CANVAS_DRAW on-paint)
                                   (send AFF_TRA enable #t)
                                   (send AFF_CM enable #t)
                                   (send AFF_EC enable #t)
                                   (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                   (change-fils VPANEL1 (list GPANEL_DRAW_VISU GPANEL_DRAW_REG VPANEL_DRAW_BUTS))
                                   (change-fils VPANEL2 (list CANVAS_DRAW))))
                       (help-string "Dessine ta propre figure et regarde la réagir!")))

;; Sous-menu Exemples
(define FICH_EXS (new menu%
                      (label "Exemples")
                      (parent MENU_FICH)
                      (help-string "Observe différents exemples déjà préparés")))

; Sous-menus du menu Exemples

; Sous-menu Barre
(define EXS_BAR (new menu-item%
                     (label "Barre")
                     (parent FICH_EXS)
                     (shortcut #\e)
                     (callback (lambda (item evt)  
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! LONG_BAR 150)
                                 (send SLIDER_EXS_BAR_LONG set-value 150)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 0)
                                 (load (build-path "Exemples" "Barre.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_BAR_LONG enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                   SLIDER_EXS_ANGLE SLIDER_EXS_BAR_LONG))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Boucle infinie
(define EXS_INF (new menu-item%
                     (label "Boucle infinie")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! TAILLE_INF 4)
                                 (send SLIDER_EXS_INF_TAILLE set-value 4)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 1)
                                 (load (build-path "Exemples" "Boucle infinie.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_INF_TAILLE enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                   SLIDER_EXS_ANGLE SLIDER_EXS_INF_TAILLE))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Ellipse
(define EXS_ELL (new menu-item%
                     (label "Ellipse")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! TAILLE_ELL 100)
                                 (send SLIDER_EXS_ELL_TAILLE set-value 100)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 2)
                                 (load (build-path "Exemples" "Ellipse.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_ELL_TAILLE enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                   SLIDER_EXS_ANGLE SLIDER_EXS_ELL_TAILLE))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Escargot
(define EXS_ESC (new menu-item%
                     (label "Escargot")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! TORSADE_ESC 8)
                                 (send SLIDER_EXS_ESC_TORSADE set-value 8)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 3)
                                 (load (build-path "Exemples" "Escargot.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_ESC_TORSADE enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                   SLIDER_EXS_ANGLE SLIDER_EXS_ESC_TORSADE))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Etoile
(define EXS_STAR (new menu-item%
                      (label "Etoile")
                      (parent FICH_EXS)
                      (callback (lambda (item evt)
                                  (send BITMAP-DC set-background COLOR_BACK)
                                  (set! GRAVITE (make-vect 0 0.01))
                                  (send SLIDER_EXS_G set-value 100)
                                  (set! E 0.95)
                                  (send SLIDER_EXS_E set-value 95)
                                  (set! ANGLE PI/2)
                                  (send SLIDER_EXS_ANGLE set-value 90)
                                  (set! NB_BRAN_STAR 12)
                                  (send SLIDER_EXS_STAR_NB_BRAN set-value 6)
                                  (set! AFFICHE_EC #f)
                                  (send CBOX_EXS_EC set-value #f)
                                  (set! AFFICHE_CM #f)
                                  (send CBOX_EXS_CM set-value #f)
                                  (set! IND_ACTION #f)
                                  (send BUTTON_EXS_ACTION set-label "Go")
                                  (send LBOX_EXS set-selection 0)
                                  (send RBOX_EXS_GRAV set-selection 4)
                                  (load (build-path "Exemples" "Etoile.scm"))
                                  (send CANVAS_EXS on-paint)
                                  (send AFF_TRA enable #t)
                                  (send AFF_CM enable #t)
                                  (send AFF_EC enable #t)
                                  (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                  (send BUTTON_EXS_ACTION enable #t)
                                  (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                  (send SLIDER_EXS_STAR_NB_BRAN enable #t)
                                  (send SLIDER_EXS_ANGLE enable #t)
                                  (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                    SLIDER_EXS_ANGLE SLIDER_EXS_STAR_NB_BRAN))
                                  (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                             GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                  (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Fleur
(define EXS_FLO (new menu-item%
                     (label "Fleur")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 5)
                                 (load (build-path "Exemples" "Fleur.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E SLIDER_EXS_ANGLE))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Haltere
(define EXS_HALT (new menu-item%
                      (label "Haltere")
                      (parent FICH_EXS)
                      (callback (lambda (item evt)
                                  (send BITMAP-DC set-background COLOR_BACK)
                                  (set! GRAVITE (make-vect 0 0.01))
                                  (send SLIDER_EXS_G set-value 100)
                                  (set! E 0.95)
                                  (send SLIDER_EXS_E set-value 95)
                                  (set! ANGLE PI/2)
                                  (send SLIDER_EXS_ANGLE set-value 90)
                                  (set! TAILLE_HALT 3)
                                  (send SLIDER_EXS_HALT_TAILLE set-value 3)
                                  (set! AFFICHE_EC #f)
                                  (send CBOX_EXS_EC set-value #f)
                                  (set! AFFICHE_CM #f)
                                  (send CBOX_EXS_CM set-value #f)
                                  (set! IND_ACTION #f)
                                  (send BUTTON_EXS_ACTION set-label "Go")
                                  (send LBOX_EXS set-selection 0)
                                  (send RBOX_EXS_GRAV set-selection 6)
                                  (load (build-path "Exemples" "Haltere.scm"))
                                  (send CANVAS_EXS on-paint)
                                  (send AFF_TRA enable #t)
                                  (send AFF_CM enable #t)
                                  (send AFF_EC enable #t)
                                  (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                  (send BUTTON_EXS_ACTION enable #t)
                                  (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                  (send SLIDER_EXS_ANGLE enable #t)
                                  (send SLIDER_EXS_HALT_TAILLE enable #t)
                                  (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E
                                                                    SLIDER_EXS_ANGLE SLIDER_EXS_HALT_TAILLE))
                                  (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                             GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                  (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Polygone régulier
(define EXS_POLYREG (new menu-item%
                         (label "Polygone regulier")
                         (parent FICH_EXS)
                         (callback (lambda (item evt)
                                     (send BITMAP-DC set-background COLOR_BACK)
                                     (set! GRAVITE (make-vect 0 0.01))
                                     (send SLIDER_EXS_G set-value 100)
                                     (set! E 0.95)
                                     (send SLIDER_EXS_E set-value 95)
                                     (set! ANGLE PI/2)
                                     (send SLIDER_EXS_ANGLE set-value 90)
                                     (set! NB_COTES_POLYREG 5)
                                     (send SLIDER_EXS_POLYREG_NB_COTES set-value 5)
                                     (set! AFFICHE_EC #f)
                                     (send CBOX_EXS_EC set-value #f)
                                     (set! AFFICHE_CM #f)
                                     (send CBOX_EXS_CM set-value #f)
                                     (set! IND_ACTION #f)
                                     (send BUTTON_EXS_ACTION set-label "Go")
                                     (send LBOX_EXS set-selection 0)
                                     (send RBOX_EXS_GRAV set-selection 7)
                                     (load (build-path "Exemples" "Polygone regulier.scm"))
                                     (send CANVAS_EXS on-paint)
                                     (send AFF_TRA enable #t)
                                     (send AFF_CM enable #t)
                                     (send AFF_EC enable #t)
                                     (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                     (send BUTTON_EXS_ACTION enable #t)
                                     (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                     (send SLIDER_EXS_POLYREG_NB_COTES enable #t)
                                     (send SLIDER_EXS_ANGLE enable #t)
                                     (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                       SLIDER_EXS_ANGLE SLIDER_EXS_POLYREG_NB_COTES))
                                     (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                                GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                     (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Quadrillage
(define EXS_QUA (new menu-item%
                     (label "Quadrillage")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0.01))
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE PI/2)
                                 (send SLIDER_EXS_ANGLE set-value 90)
                                 (set! COURB_QUA 50)
                                 (send SLIDER_EXS_QUA_COURB set-value 50)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 0)
                                 (send RBOX_EXS_GRAV set-selection 8)
                                 (load (build-path "Exemples" "Quadrillage.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                 (send SLIDER_EXS_QUA_COURB enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                   SLIDER_EXS_ANGLE SLIDER_EXS_QUA_COURB))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU 
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Séparateur entre les sous-menus relevant de la gravitation et ceux relevant des chocs
(define SEPARATOR_EXS (new separator-menu-item% (parent FICH_EXS)))

; Sous-menu Cercle et ellipse
(define EXS_CERELL (new menu-item%
                        (label "Cercle et ellipse")
                        (parent FICH_EXS)
                        (callback (lambda (item evt)
                                    (send BITMAP-DC set-background COLOR_BACK)
                                    (set! GRAVITE (make-vect 0 0)) 
                                    (set! E 0.95)
                                    (send SLIDER_EXS_E set-value 95)
                                    (set! AFFICHE_EC #f)
                                    (send CBOX_EXS_EC set-value #f)
                                    (set! AFFICHE_CM #f)
                                    (send CBOX_EXS_CM set-value #f)
                                    (set! IND_ACTION #f)
                                    (send BUTTON_EXS_ACTION set-label "Go")
                                    (send LBOX_EXS set-selection 1)
                                    (send RBOX_EXS_CHOC set-selection 0)
                                    (load (build-path "Exemples" "Cercle et ellipse.scm"))
                                    (send CANVAS_EXS on-paint)
                                    (send AFF_TRA enable #t)
                                    (send AFF_CM enable #t)
                                    (send AFF_EC enable #t)
                                    (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                    (send BUTTON_EXS_ACTION enable #t)
                                    (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                    (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                    (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                               GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                    (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Formes diverses
(define EXS_FORMDIV (new menu-item%
                         (label "Formes diverses")
                         (parent FICH_EXS)
                         (callback (lambda (item evt)
                                     (send BITMAP-DC set-background COLOR_BACK)
                                     (set! GRAVITE (make-vect 0 0)) 
                                     (set! E 0.95)
                                     (send SLIDER_EXS_E set-value 95)
                                     (set! AFFICHE_EC #f)
                                     (send CBOX_EXS_EC set-value #f)
                                     (set! AFFICHE_CM #f)
                                     (send CBOX_EXS_CM set-value #f)
                                     (set! IND_ACTION #f)
                                     (send BUTTON_EXS_ACTION set-label "Go")
                                     (send LBOX_EXS set-selection 1)
                                     (send RBOX_EXS_CHOC set-selection 1)
                                     (load (build-path "Exemples" "Formes diverses.scm"))
                                     (send CANVAS_EXS on-paint)
                                     (send AFF_TRA enable #t)
                                     (send AFF_CM enable #t)
                                     (send AFF_EC enable #t)
                                     (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                     (send BUTTON_EXS_ACTION enable #t)
                                     (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                     (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                     (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                                GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                     (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Oeil de hibou
(define EXS_HIB (new menu-item%
                     (label "Oeil de hibou")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! GRAVITE (make-vect 0 0)) 
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 1)
                                 (send RBOX_EXS_CHOC set-selection 2)
                                 (load (build-path "Exemples" "Oeil de hibou.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                 (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Oeil de Sauron
(define EXS_SAURON (new menu-item%
                        (label "Oeil de Sauron")
                        (parent FICH_EXS)
                        (callback (lambda (item evt)
                                    (send BITMAP-DC set-background COLOR_BACK)
                                    (set! GRAVITE (make-vect 0 0)) 
                                    (set! E 0.95)
                                    (send SLIDER_EXS_E set-value 95)
                                    (set! AFFICHE_EC #f)
                                    (send CBOX_EXS_EC set-value #f)
                                    (set! AFFICHE_CM #f)
                                    (send CBOX_EXS_CM set-value #f)
                                    (set! IND_ACTION #f)
                                    (send BUTTON_EXS_ACTION set-label "Go")
                                    (send LBOX_EXS set-selection 1)
                                    (send RBOX_EXS_CHOC set-selection 3)
                                    (load (build-path "Exemples" "Oeil de Sauron.scm"))
                                    (send CANVAS_EXS on-paint)
                                    (send AFF_TRA enable #t)
                                    (send AFF_CM enable #t)
                                    (send AFF_EC enable #t)
                                    (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                    (send BUTTON_EXS_ACTION enable #t)
                                    (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                    (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                    (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                               GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                    (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Polygones imbriques
(define EXS_POLYIMB (new menu-item%
                         (label "Polygones imbriques")
                         (parent FICH_EXS)
                         (callback (lambda (item evt)
                                     (send BITMAP-DC set-background COLOR_BACK)
                                     (set! GRAVITE (make-vect 0 0)) 
                                     (set! E 0.95)
                                     (send SLIDER_EXS_E set-value 95)
                                     (set! AFFICHE_EC #f)
                                     (send CBOX_EXS_EC set-value #f)
                                     (set! AFFICHE_CM #f)
                                     (send CBOX_EXS_CM set-value #f)
                                     (set! IND_ACTION #f)
                                     (send BUTTON_EXS_ACTION set-label "Go")
                                     (send LBOX_EXS set-selection 1)
                                     (send RBOX_EXS_CHOC set-selection 4)
                                     (load (build-path "Exemples" "Polygones imbriques.scm"))
                                     (send CANVAS_EXS on-paint)
                                     (send AFF_TRA enable #t)
                                     (send AFF_CM enable #t)
                                     (send AFF_EC enable #t)
                                     (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                     (send BUTTON_EXS_ACTION enable #t)
                                     (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                     (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                     (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                                GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                     (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Triangles
(define EXS_TRIS (new menu-item%
                      (label "Triangles")
                      (parent FICH_EXS)
                      (callback (lambda (item evt)
                                  (send BITMAP-DC set-background COLOR_BACK)
                                  (set! GRAVITE (make-vect 0 0))
                                  (set! E 0.95)
                                  (send SLIDER_EXS_E set-value 95)
                                  (set! AFFICHE_EC #f)
                                  (send CBOX_EXS_EC set-value #f)
                                  (set! AFFICHE_CM #f)
                                  (send CBOX_EXS_CM set-value #f)
                                  (set! IND_ACTION #f)
                                  (send BUTTON_EXS_ACTION set-label "Go")
                                  (send LBOX_EXS set-selection 1)
                                  (send RBOX_EXS_CHOC set-selection 5)
                                  (load (build-path "Exemples" "Triangles.scm"))
                                  (send CANVAS_EXS on-paint)
                                  (send AFF_TRA enable #t)
                                  (send AFF_CM enable #t)
                                  (send AFF_EC enable #t)
                                  (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                  (send BUTTON_EXS_ACTION enable #t)
                                  (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                  (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                  (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                             GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                  (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Triangles necklace (ou Collier de triangles)
(define EXS_TRINECK (new menu-item%
                         (label "Triangles necklace")
                         (parent FICH_EXS)
                         (callback (lambda (item evt)
                                     (send BITMAP-DC set-background COLOR_BACK)
                                     (set! GRAVITE (make-vect 0 0)) 
                                     (set! E 0.95)
                                     (send SLIDER_EXS_E set-value 95)
                                     (set! AFFICHE_EC #f)
                                     (send CBOX_EXS_EC set-value #f)
                                     (set! AFFICHE_CM #f)
                                     (send CBOX_EXS_CM set-value #f)
                                     (set! IND_ACTION #f)
                                     (send BUTTON_EXS_ACTION set-label "Go")
                                     (send LBOX_EXS set-selection 1)
                                     (send RBOX_EXS_CHOC set-selection 6)
                                     (load (build-path "Exemples" "Triangles necklace.scm"))
                                     (send CANVAS_EXS on-paint)
                                     (send AFF_TRA enable #t)
                                     (send AFF_CM enable #t)
                                     (send AFF_EC enable #t)
                                     (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                     (send BUTTON_EXS_ACTION enable #t)
                                     (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                     (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E))
                                     (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                                GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                     (change-fils VPANEL2 (list CANVAS_EXS))))))

; Séparateur entre les sous-menus relevant des chocs et ceux relevant des parametrisations
(define SEPARATOR_EXS2 (new separator-menu-item% (parent FICH_EXS)))

; Sous-menu Courbe cartesienne
(define EXS_CART (new menu-item%
                      (label "Courbe cartesienne")
                      (parent FICH_EXS)
                      (callback (lambda (item evt)
                                  (send BITMAP-DC set-background COLOR_BACK)
                                  (set! POLYGONE_EXS (polygone-nul))
                                  (set! GRAVITE (make-vect 0 0.01)) 
                                  (send SLIDER_EXS_G set-value 100)
                                  (set! E 0.95)
                                  (send SLIDER_EXS_E set-value 95)
                                  (set! ANGLE 0)
                                  (send SLIDER_EXS_ANGLE set-value 0)
                                  (set! AFFICHE_EC #f)
                                  (send CBOX_EXS_EC set-value #f)
                                  (set! AFFICHE_CM #f)
                                  (send CBOX_EXS_CM set-value #f)
                                  (set! CART_X "(expt (sin t) 3)")
                                  (send TFIELD_EXS_CART_X set-value "(expt (sin t) 3)")
                                  (set! CART_Y "(- (cos t) (expt (cos t) 4))")
                                  (send TFIELD_EXS_CART_Y set-value "(- (cos t) (expt (cos t) 4))")
                                  (set! CART_INF "(- pi)")
                                  (send TFIELD_EXS_CART_INF set-value "(- pi)")
                                  (set! CART_SUP "pi")
                                  (send TFIELD_EXS_CART_SUP set-value "pi")
                                  (send MSG_EXS_CART set-label "") 
                                  (set! IND_ACTION #f)
                                  (send BUTTON_EXS_ACTION set-label "Go")
                                  (send LBOX_EXS set-selection 2)
                                  (send RBOX_EXS_CHOC set-selection 0)
                                  (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                  (send CANVAS_EXS on-paint)
                                  (send AFF_TRA enable #t)
                                  (send AFF_CM enable #t)
                                  (send AFF_EC enable #t)
                                  (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                  (send BUTTON_EXS_ACTION enable #t)
                                  (send MSG_EXS_CART set-label "")
                                  (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_PARAM))
                                  (send TFIELD_EXS_CART_X enable #t)
                                  (send TFIELD_EXS_CART_Y enable #t)
                                  (send TFIELD_EXS_CART_INF enable #t)
                                  (send TFIELD_EXS_CART_SUP enable #t)
                                  (send BUTTON_EXS_CART enable #t)
                                  (send SLIDER_EXS_ANGLE enable #t)
                                  (change-fils GPANEL_EXS_REG (list TFIELD_EXS_CART_X TFIELD_EXS_CART_Y 
                                                                    TFIELD_EXS_CART_INF TFIELD_EXS_CART_SUP
                                                                    MSG_EXS_CART BUTTON_EXS_CART SLIDER_EXS_G SLIDER_EXS_E
                                                                    SLIDER_EXS_ANGLE))
                                  (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                             GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                  (change-fils VPANEL2 (list CANVAS_EXS))))))

; Sous-menu Courbe polaire
(define EXS_POL (new menu-item%
                     (label "Courbe polaire")
                     (parent FICH_EXS)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_BACK)
                                 (set! POLYGONE_EXS (polygone-nul))
                                 (set! GRAVITE (make-vect 0 0.01)) 
                                 (send SLIDER_EXS_G set-value 100)
                                 (set! E 0.95)
                                 (send SLIDER_EXS_E set-value 95)
                                 (set! ANGLE 0)
                                 (send SLIDER_EXS_ANGLE set-value 0)
                                 (set! AFFICHE_EC #f)
                                 (send CBOX_EXS_EC set-value #f)
                                 (set! AFFICHE_CM #f)
                                 (send CBOX_EXS_CM set-value #f)
                                 (set! POL_RHO "(+ (- (exp (cos t)) (* 2 (cos (* 4 t)))) (expt (sin (/ t 12)) 5))")
                                 (send TFIELD_EXS_POL_RHO set-value 
                                       "(+ (- (exp (cos t)) (* 2 (cos (* 4 t)))) (expt (sin (/ t 12)) 5))")
                                 (set! POL_INF "(- pi)")
                                 (send TFIELD_EXS_POL_INF set-value "(- pi)")
                                 (set! POL_SUP "pi")
                                 (send TFIELD_EXS_POL_SUP set-value "pi")
                                 (send MSG_EXS_POL set-label "")
                                 (set! IND_ACTION #f)
                                 (send BUTTON_EXS_ACTION set-label "Go")
                                 (send LBOX_EXS set-selection 2)
                                 (send RBOX_EXS_CHOC set-selection 1)
                                 (load (build-path "Exemples" "Courbe polaire.scm"))
                                 (send CANVAS_EXS on-paint)
                                 (send AFF_TRA enable #t)
                                 (send AFF_CM enable #t)
                                 (send AFF_EC enable #t)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
                                 (send BUTTON_EXS_ACTION enable #t)
                                 (send MSG_EXS_POL set-label "")
                                 (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_PARAM))
                                 (send TFIELD_EXS_POL_RHO enable #t)
                                 (send TFIELD_EXS_POL_INF enable #t)
                                 (send TFIELD_EXS_POL_SUP enable #t)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (send BUTTON_EXS_POL enable #t)
                                 (change-fils GPANEL_EXS_REG (list TFIELD_EXS_POL_RHO TFIELD_EXS_POL_INF TFIELD_EXS_POL_SUP
                                                                   MSG_EXS_POL BUTTON_EXS_POL SLIDER_EXS_G SLIDER_EXS_E
                                                                   SLIDER_EXS_ANGLE))
                                 (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                            GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_EXS))))))

;; Sous-menu Objets
(define FICH_OBJ (new menu%
                      (label "Objets")
                      (parent MENU_FICH)
                      (help-string "Observe différents exemples et modifie certains paramètres")))

; Sous-menus du menu Objets

; Sous-menu Deux boules
(define OBJ_2BAL (new menu-item%
                      (label "Deux boules")
                      (parent FICH_OBJ)
                      (shortcut #\o)
                      (callback (lambda (item evt)
                                  (send BITMAP-DC set-background COLOR_OBJ_BACK)
                                  (init-objet)        
                                  (set! AFFICHE_OBJ_CM #f)
                                  (send CBOX_OBJ_CM set-value #f)
                                  (set! IND_ACTION #f)
                                  (send BUTTON_OBJ_ACTION set-label "Go")
                                  (send RBOX_OBJ set-selection 0)
                                  (set! M1_2B 1)
                                  (send SLIDER_OBJ_2B_M1 set-value 1)
                                  (send SLIDER_OBJ_2B_M1 enable #t)
                                  (set! M2_2B 1)
                                  (send SLIDER_OBJ_2B_M2 set-value 1)
                                  (send SLIDER_OBJ_2B_M2 enable #t)
                                  (set! V1_2B (make-vect -5 0))
                                  (send SLIDER_OBJ_2B_V1 set-value -5)
                                  (send SLIDER_OBJ_2B_V1 enable #t)
                                  (set! V2_2B (make-vect 0 0))
                                  (send SLIDER_OBJ_2B_V2 set-value 0)
                                  (send SLIDER_OBJ_2B_V2 enable #t)
                                  (set! R1_2B 60)
                                  (send SLIDER_OBJ_2B_R1 set-value 60)
                                  (send SLIDER_OBJ_2B_R1 enable #t)
                                  (set! R2_2B 60)
                                  (send SLIDER_OBJ_2B_R2 set-value 60)
                                  (send SLIDER_OBJ_2B_R2 enable #t)
                                  (load (build-path "Objets" "Deux boules.scm"))
                                  (send CANVAS_OBJ on-paint)
                                  (send AFF_TRA enable #f)
                                  (send AFF_CM enable #f)
                                  (send AFF_EC enable #f)
                                  (send AFF_FOND set-label "Changer la couleur de fond pour la section Objets")
                                  (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_2B_M1 SLIDER_OBJ_2B_M2 SLIDER_OBJ_2B_R1 
                                                                    SLIDER_OBJ_2B_R2 SLIDER_OBJ_2B_V1 SLIDER_OBJ_2B_V2))
                                  (change-fils VPANEL1 (list GPANEL_OBJ_SELECT GPANEL_OBJ_VISU 
                                                             GPANEL_OBJ_REG VPANEL_OBJ_BUTS))
                                  (change-fils VPANEL2 (list CANVAS_OBJ))))))

; Sous-menu Boules multiples
(define OBJ_BAL (new menu-item%
                     (label "Boules multiples")
                     (parent FICH_OBJ)
                     (callback (lambda (item evt) 
                                 (send BITMAP-DC set-background COLOR_OBJ_BACK)
                                 (init-objet)
                                 (set! AFFICHE_OBJ_CM #f)
                                 (send CBOX_OBJ_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_OBJ_ACTION set-label "Go")
                                 (send RBOX_OBJ set-selection 1)
                                 (set! NB_BAL 20)
                                 (send SLIDER_OBJ_BAL_NB set-value 20)
                                 (send SLIDER_OBJ_BAL_NB enable #t)
                                 (load (build-path "Objets" "Boules multiples.scm"))
                                 (send CANVAS_OBJ on-paint)
                                 (send AFF_TRA enable #f)
                                 (send AFF_CM enable #f)
                                 (send AFF_EC enable #f)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour la section Objets")
                                 (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_BAL_NB))
                                 (change-fils VPANEL1 (list GPANEL_OBJ_SELECT GPANEL_OBJ_VISU
                                                            GPANEL_OBJ_REG VPANEL_OBJ_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_OBJ))))))

; Sous-menu Gravite
(define OBJ_GRA (new menu-item%
                     (label "Gravite")
                     (parent FICH_OBJ)
                     (callback (lambda (item evt)   
                                 (send BITMAP-DC set-background COLOR_OBJ_BACK)
                                 (init-objet)
                                 (set! AFFICHE_OBJ_CM #f)
                                 (send CBOX_OBJ_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_OBJ_ACTION set-label "Go")
                                 (send RBOX_OBJ set-selection 2)
                                 (set! GRAVITE 0.1)
                                 (send SLIDER_OBJ_GRAV_G set-value 10)
                                 (send SLIDER_OBJ_GRAV_G enable #t)
                                 (set! M_GRAV 1)
                                 (send SLIDER_OBJ_GRAV_M set-value 1)
                                 (send SLIDER_OBJ_GRAV_M enable #t)
                                 (set! V_GRAV (make-vect -1 0))
                                 (send SLIDER_OBJ_GRAV_V set-value -1)
                                 (send SLIDER_OBJ_GRAV_V enable #t)
                                 (set! R_GRAV 50)
                                 (send SLIDER_OBJ_GRAV_R set-value 50)
                                 (send SLIDER_OBJ_GRAV_R enable #t) 
                                 (load (build-path "Objets" "Gravite.scm"))
                                 (send CANVAS_OBJ on-paint)
                                 (send AFF_TRA enable #f)
                                 (send AFF_CM enable #f)
                                 (send AFF_EC enable #f)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour la section Objets")
                                 (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_GRAV_G SLIDER_OBJ_GRAV_M 
                                                                   SLIDER_OBJ_GRAV_R SLIDER_OBJ_GRAV_V))
                                 (change-fils VPANEL1 (list GPANEL_OBJ_SELECT GPANEL_OBJ_VISU
                                                            GPANEL_OBJ_REG VPANEL_OBJ_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_OBJ))))))

; Sous-menu Pendule
(define OBJ_PEN (new menu-item%
                     (label "Pendule")
                     (parent FICH_OBJ)
                     (callback (lambda (item evt)
                                 (send BITMAP-DC set-background COLOR_OBJ_BACK)
                                 (init-objet)
                                 (set! AFFICHE_OBJ_CM #f)
                                 (send CBOX_OBJ_CM set-value #f)
                                 (set! IND_ACTION #f)
                                 (send BUTTON_OBJ_ACTION set-label "Go")
                                 (send RBOX_OBJ set-selection 3)
                                 (set! NB_PEND 5)
                                 (send SLIDER_OBJ_PEND_NB set-value 5)
                                 (send SLIDER_OBJ_PEND_NB enable #t)
                                 (load (build-path "Objets" "Pendule.scm"))
                                 (send CANVAS_OBJ on-paint)
                                 (send AFF_TRA enable #f)
                                 (send AFF_CM enable #f)
                                 (send AFF_EC enable #f)
                                 (send AFF_FOND set-label "Changer la couleur de fond pour la section Objets")
                                 (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_PEND_NB))
                                 (change-fils VPANEL1 (list GPANEL_OBJ_SELECT GPANEL_OBJ_VISU
                                                            GPANEL_OBJ_REG VPANEL_OBJ_BUTS))
                                 (change-fils VPANEL2 (list CANVAS_OBJ))))))

;; Sous-menus du menu Affichage

;; Sous-menu Couleur du fond
(define AFF_FOND (new menu-item%
                      (label "Changer la couleur de fond pour les sections Dessin et Exemples")
                      (parent MENU_AFF)
                      (callback (lambda (item evt)
                                  (let ((current-dc (car (send VPANEL2 get-children))) (color (get-color-from-user)))
                                    (if color ; au cas où l'utilisateur aurait fermé la fenêtre de choix ou appuyé sur annuler
                                        (begin (if (equal? current-dc CANVAS_OBJ)
                                                   (set! COLOR_OBJ_BACK color)
                                                   (set! COLOR_BACK color))
                                               (send BITMAP-DC set-background color)
                                               (send current-dc on-paint))))))))

;; Sous-menu Couleur du tracé
(define AFF_TRA (new menu-item%
                     (label "Changer la couleur du tracé")
                     (parent MENU_AFF)
                     (callback (lambda (item evt)
                                 (let ((current-dc (first (send VPANEL2 get-children))) (color (get-color-from-user)))
                                   (if color
                                       (set! COLOR_POLY color))
                                   (send current-dc on-paint))))))

;; Sous-menu Couleur du centre de masse
(define AFF_CM (new menu-item%
                    (label "Changer la couleur du centre de masse")
                    (parent MENU_AFF) 
                    (callback
                     (lambda (item evt)
                       (let ((current-dc (first (send VPANEL2 get-children))) (color (get-color-from-user)))
                         (if color
                             (set! COLOR_CM color))
                         (send current-dc on-paint))))))

;; Sous-menu Couleur de l'enveloppe convexe
(define AFF_EC (new menu-item%
                    (label "Changer la couleur de l'enveloppe convexe")
                    (parent MENU_AFF) 
                    (callback
                     (lambda (item evt)
                       (let ((current-dc (first (send VPANEL2 get-children))) (color (get-color-from-user)))
                         (if color
                             (set! COLOR_EC color))
                         (send current-dc on-paint))))))

;; Sous-menus du menu Aide

;; Sous-menu A props du projet
(define AIDE_PROPOS (new menu-item%                         
                         (label "A propos du Projet")
                         (parent MENU_AIDE)
                         (shortcut #\s)
                         (callback (lambda (item evt)
                                     (if (equal? (system-type 'os) 'windows)
                                         (system (path->string (build-path (current-directory) "slideshow.scm")))
                                         (system (string-append "open " (path->string (build-path (current-directory) "slideshow.scm")))))))))

;; Sous-menu Documentation technique
(define AIDE_DOCTEC (new menu-item%                         
                         (label "Documentation technique")
                         (parent MENU_AIDE)
                         (shortcut #\t)
                         (callback (lambda (item evt)
                                     (if (equal? (system-type 'os) 'windows)
                                         (system (path->string (build-path (current-directory) "documentation_technique.pdf")))
                                         (system (string-append "open " (path->string (build-path (current-directory) "documentation_technique.pdf")))))))))

;; Sous-menu Aide en ligne
(define AIDE_SITE (new menu-item%                         
                       (label "Aide en ligne")
                       (parent MENU_AIDE)
                       (shortcut #\?)
                       (callback (lambda (item evt)
                                   (send-url "http://projetphysics.teria.org")))))

;*****************************************************

;          GESTION DES PANEAUX DE REGLAGES

;*****************************************************

;;;; Panneau initial 

;;; Panneau de sélection de la section
(define GPANEL_INIT_SELECT (new group-box-panel%
                                (label "Sélection")
                                (parent VPANEL1)
                                (vert-margin 5)
                                (horiz-margin 10)
                                (spacing 30)
                                (border 5)
                                (alignment '(center center))))

;; Bouton Dessin
(define BUTTON_DRAW (new button%
                         (label (make-object bitmap% 
                                  (build-path 'same "Images" "Icones" "dessin.png")
                                  'png          
                                  #f))
                         (parent GPANEL_INIT_SELECT)
                         (callback (lambda (b evt)
                                     (set! GRAVITE (make-vect 0 0.01))
                                     (send SLIDER_DRAW_G set-value 100)
                                     (set! E 0.95)
                                     (send SLIDER_DRAW_E set-value 95)
                                     (set! POLYGONE (polygone-nul))
                                     (set! AFFICHE_EC #f)
                                     (send CBOX_DRAW_EC set-value #f)
                                     (set! AFFICHE_CM #f)
                                     (send CBOX_DRAW_CM set-value #f)
                                     (set! COMPT 0)
                                     (set! IND_ACTION #f)
                                     (send BUTTON_DRAW_ACTION set-label "Go")
                                     (send CANVAS_DRAW on-paint)
                                     (change-fils VPANEL1 (list GPANEL_DRAW_VISU GPANEL_DRAW_REG VPANEL_DRAW_BUTS))
                                     (change-fils VPANEL2 (list CANVAS_DRAW))))))

;; Panneau horizontal contenant les boutons Exemples et Objets
(define HPANEL_BUTS1 (new horizontal-panel%
                          (parent GPANEL_INIT_SELECT)
                          (spacing 50)
                          (alignment '(center center))))

; Bouton Exemples
(define BUTTON_EXS (new button%
                        (label (make-object bitmap% 
                                 (build-path 'same "Images" "Icones" "dossier.png")
                                 'png           
                                 #f))      
                        (parent HPANEL_BUTS1)
                        (callback (lambda (b evt)
                                    (set! GRAVITE (make-vect 0 0.01))
                                    (send SLIDER_EXS_G set-value 100)
                                    (set! E 0.95)
                                    (send SLIDER_EXS_E set-value 95)
                                    (set! ANGLE PI/2)
                                    (send SLIDER_EXS_ANGLE set-value 90)
                                    (set! LONG_BAR 150)
                                    (send SLIDER_EXS_BAR_LONG set-value 150)
                                    (set! AFFICHE_EC #f)
                                    (send CBOX_EXS_EC set-value #f)
                                    (set! AFFICHE_CM #f)
                                    (send CBOX_EXS_CM set-value #f)
                                    (set! IND_ACTION #f)
                                    (send BUTTON_EXS_ACTION set-label "Go")
                                    (send LBOX_EXS set-selection 0)
                                    (send RBOX_EXS_GRAV set-selection 0)
                                    (load (build-path "Exemples" "Barre.scm"))
                                    (send CANVAS_EXS on-paint)
                                    (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                    (send SLIDER_EXS_BAR_LONG enable #t)
                                    (send SLIDER_EXS_ANGLE enable #t)
                                    (send BUTTON_EXS_ACTION enable #t)
                                    (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                      SLIDER_EXS_ANGLE SLIDER_EXS_BAR_LONG))
                                    (change-fils VPANEL1 (list GPANEL_EXS_SELECT GPANEL_EXS_VISU
                                                               GPANEL_EXS_REG VPANEL_EXS_BUTS))
                                    (change-fils VPANEL2 (list CANVAS_EXS))))))


; Bouton Objets
(define BUTTON_OBJ (new button%
                        (label (make-object bitmap% 
                                 (build-path 'same "Images" "Icones" "panier.png")
                                 'png          
                                 #f))
                        (parent HPANEL_BUTS1)
                        (callback (lambda (b evt)
                                    (send BITMAP-DC set-background COLOR_OBJ_BACK)
                                    (init-objet)        
                                    (set! AFFICHE_OBJ_CM #f)
                                    (send CBOX_OBJ_CM set-value #f)
                                    (set! IND_ACTION #f)
                                    (send BUTTON_OBJ_ACTION set-label "Go")
                                    (send RBOX_OBJ set-selection 0)
                                    (set! M1_2B 1)
                                    (send SLIDER_OBJ_2B_M1 set-value 1)
                                    (send SLIDER_OBJ_2B_M1 enable #t)
                                    (set! M2_2B 1)
                                    (send SLIDER_OBJ_2B_M2 set-value 1)
                                    (send SLIDER_OBJ_2B_M2 enable #t)
                                    (set! V1_2B (make-vect -5 0))
                                    (send SLIDER_OBJ_2B_V1 set-value -5)
                                    (send SLIDER_OBJ_2B_V1 enable #t)
                                    (set! V2_2B (make-vect 0 0))
                                    (send SLIDER_OBJ_2B_V2 set-value 0)
                                    (send SLIDER_OBJ_2B_V2 enable #t)
                                    (set! R1_2B 60)
                                    (send SLIDER_OBJ_2B_R1 set-value 60)
                                    (send SLIDER_OBJ_2B_R1 enable #t)
                                    (set! R2_2B 60)
                                    (send SLIDER_OBJ_2B_R2 set-value 60)
                                    (send SLIDER_OBJ_2B_R2 enable #t)
                                    (load (build-path "Objets" "Deux boules.scm"))
                                    (send CANVAS_OBJ on-paint)
                                    (send AFF_TRA enable #f)
                                    (send AFF_CM enable #f)
                                    (send AFF_EC enable #f)
                                    (send AFF_FOND set-label "Changer la couleur de fond pour la section Objets")
                                    (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_2B_M1 SLIDER_OBJ_2B_M2 SLIDER_OBJ_2B_R1 
                                                                      SLIDER_OBJ_2B_R2 SLIDER_OBJ_2B_V1 SLIDER_OBJ_2B_V2))
                                    (change-fils VPANEL1 (list GPANEL_OBJ_SELECT GPANEL_OBJ_VISU
                                                               GPANEL_OBJ_REG VPANEL_OBJ_BUTS))
                                    (change-fils VPANEL2 (list CANVAS_OBJ))))))

;;; Panneau de sélection de la documentation
(define GPANEL_INIT_DOC (new group-box-panel%
                             (label "Documentation") 
                             (parent VPANEL1)
                             (vert-margin 5)
                             (horiz-margin 10)
                             (spacing 30)
                             (border 5)))

;; Panneau horizontal contenant les boutons Slide et Lien
(define HPANEL_BUTS2 (new horizontal-panel%
                          (parent GPANEL_INIT_DOC)
                          (spacing 50)
                          (alignment '(center center))))

; Bouton Slide
(define BUTTON_SLIDE (new button%
                          (label (make-object bitmap% 
                                   (build-path 'same "Images" "Icones" "boussole.png")
                                   'png          
                                   #f))
                          (parent HPANEL_BUTS2)
                          (callback (lambda (item evt)
                                      (if (equal? (system-type 'os) 'windows)
                                          (system (path->string (build-path (current-directory) "slideshow.scm")))
                                          (system (string-append "open " (path->string (build-path (current-directory) "slideshow.scm")))))))))

; Bouton Lien
(define BUTTON_LINK (new button%
                         (label (make-object bitmap% 
                                  (build-path 'same "Images" "Icones" "lien.png")
                                  'png          
                                  #f))
                         (parent HPANEL_BUTS2)
                         (callback (lambda (b evt)
                                     (send-url "http://projetphysics.teria.org")))))

;;;; Panneaux pour le dessin

;;; Panneau de visualisation pour le dessin
(define GPANEL_DRAW_VISU (new group-box-panel%
                              (label "Visualisation")
                              (parent VPANEL1)
                              (style '(deleted))
                              (vert-margin 5)
                              (horiz-margin 10)
                              (spacing 10)
                              (border 5)
                              (alignment '(center center))))

;; Case à cocher pour l'enveloppe convexe
(define CBOX_DRAW_EC (new check-box% 
                          (label "Enveloppe convexe")
                          (parent GPANEL_DRAW_VISU)
                          (callback (lambda (c evt)
                                      (case AFFICHE_EC
                                        ((#t) (set! AFFICHE_EC #f)
                                              (send CANVAS_DRAW on-paint))
                                        (else 
                                         (set! AFFICHE_EC #t)
                                         (send CANVAS_DRAW on-paint)))))))

;; Case à cocher pour le centre de masse
(define CBOX_DRAW_CM (new check-box% 
                          (label "Centre de masse")
                          (parent GPANEL_DRAW_VISU)
                          (callback (lambda (c evt)
                                      (case AFFICHE_CM
                                        ((#t) (set! AFFICHE_CM #f)
                                              (send CANVAS_DRAW on-paint))
                                        (else 
                                         (set! AFFICHE_CM #t)
                                         (send CANVAS_DRAW on-paint)))))))

;;; Panneau de réglages pour le dessin 
(define GPANEL_DRAW_REG (new group-box-panel%
                             (label "Réglages")
                             (parent VPANEL1)
                             (style '(deleted))
                             (vert-margin 5)
                             (horiz-margin 10)
                             (spacing 10)
                             (border 5)
                             (alignment '(center center))))

;; Slider pour régler l'effet de la gravité (pourcentage)
(define SLIDER_DRAW_G (new slider%
                           (label "Coefficient de gravitation (%)")
                           (min-value 0)
                           (max-value 100)
                           (init-value 100)
                           (parent GPANEL_DRAW_REG)
                           (callback 
                            (lambda (sl evt)
                              (set! GRAVITE (make-vect 0 (/ (send sl get-value) 10000)))))))

;; Slider pour régler le coefficient d'elasticite (pourcentage)
(define SLIDER_DRAW_E (new slider%
                           (label "Coefficient d'élasticité (%)")
                           (min-value 50)
                           (max-value 100)
                           (init-value 100)
                           (parent GPANEL_DRAW_REG)
                           (callback 
                            (lambda (sl evt)
                              (set! E (/ (send sl get-value) 100))))))

;;; Panneau de boutons pour le dessin
(define VPANEL_DRAW_BUTS (new vertical-panel%
                              (parent VPANEL1)
                              (style '(deleted))
                              (vert-margin 5)
                              (horiz-margin 10)
                              (spacing 10)
                              (border 5)
                              (alignment '(center center))
                              (stretchable-height #f)))

;; Bouton pour effacer le dessin
(define BUTTON_DRAW_CLEAR
  (new button%
       (label "Clear")
       (parent VPANEL_DRAW_BUTS)       
       (callback
        (lambda (b evt)
          (set! IND_ACTION #f)
          (send BUTTON_DRAW_ACTION set-label "Go")
          (set! POLYGONE (polygone-nul))
          (send CANVAS_DRAW on-paint)))))

;; Bouton pour enclencher l'animation
(define BUTTON_DRAW_ACTION
  (new button%
       (label "Go")
       (parent VPANEL_DRAW_BUTS)       
       (callback
        (lambda (b evt)
          (case IND_ACTION
            ((#t) (set! IND_ACTION #f)
                  (send BUTTON_DRAW_ACTION set-label "Go")
                  (send CANVAS_DRAW on-paint))
            (else (set! IND_ACTION #t)
                  (send BUTTON_DRAW_ACTION set-label "Stop") 
                  (send CANVAS_DRAW on-paint)))))))

;; Bouton de retour au menu initial
(define BUTTON_DRAW_MENU
  (new button%
       (label "Menu")
       (parent VPANEL_DRAW_BUTS)       
       (callback
        (lambda (b evt)
          (set! IND_ACTION #f)
          (change-fils VPANEL1 (list GPANEL_INIT_SELECT GPANEL_INIT_DOC))
          (change-fils VPANEL2 (list CANVAS_INIT))))))

;;;; Panneaux pour les exemples

;;; Panneau de sélection des exemples
(define GPANEL_EXS_SELECT (new group-box-panel%
                               (label "Exemples")
                               (parent VPANEL1)
                               (style '(deleted))
                               (horiz-margin 10)
                               (alignment '(center center))
                               (stretchable-height #f)))

;; Label-box pour choisir parmi les deux radio-box
(define LBOX_EXS (new list-box%
                      (label "Catégorie")
                      (choices (list "Gravitation" "Chocs" "Parametrisation")) 
                      (parent GPANEL_EXS_SELECT) 
                      (callback (lambda (lb evt)
                                  (set! E 0.95)
                                  (send SLIDER_EXS_E set-value 95)
                                  (set! IND_ACTION #f)
                                  (send BUTTON_EXS_ACTION set-label "Go")
                                  (send BUTTON_EXS_ACTION enable #t)
                                  (case (first (send lb get-selections))
                                    ((0) (send RBOX_EXS_GRAV set-selection 0)
                                         (set! GRAVITE (make-vect 0 0.01))
                                         (send SLIDER_EXS_G set-value 100)
                                         (set! ANGLE PI/2)
                                         (send SLIDER_EXS_ANGLE set-value 90)
                                         (set! LONG_BAR 150)
                                         (send SLIDER_EXS_BAR_LONG set-value 150)     
                                         (send RBOX_EXS_GRAV set-selection 0)
                                         (load (build-path "Exemples" "Barre.scm"))  
                                         (send CANVAS_EXS on-paint)
                                         (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_GRAV))
                                         (send SLIDER_EXS_BAR_LONG enable #t)
                                         (send SLIDER_EXS_ANGLE enable #t)
                                         (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E 
                                                                           SLIDER_EXS_ANGLE SLIDER_EXS_BAR_LONG)))
                                    ((1) (set! GRAVITE (make-vect 0 0))
                                         (send RBOX_EXS_CHOC set-selection 0)
                                         (load (build-path "Exemples" "Cercle et ellipse.scm"))
                                         (send CANVAS_EXS on-paint)
                                         (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_CHOC))
                                         (change-fils GPANEL_EXS_REG (list SLIDER_EXS_E)))
                                    ((2) 
                                     
                                     ;; ¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡ ATTENTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                     
                                     ;; En passant de la paramétrisation a une autre section de la catégorie
                                     ;; exemple, la fenêtre s'agrandit alors que celle de paramétrisation est la plus grande
                                     ;; L'utilisateur doit alors la redimensionner
                                     
                                     (set! POLYGONE_EXS (polygone-nul))
                                         (send RBOX_EXS_PARAM set-selection 0)
                                         (set! GRAVITE (make-vect 0 0.01))
                                         (send SLIDER_EXS_G set-value 100)
                                         (set! ANGLE 0)
                                         (send SLIDER_EXS_ANGLE set-value 0)
                                         (set! CART_X "(expt (sin t) 3)")
                                         (send TFIELD_EXS_CART_X set-value "(expt (sin t) 3)")
                                         (set! CART_Y "(- (cos t) (expt (cos t) 4))")
                                         (send TFIELD_EXS_CART_Y set-value "(- (cos t) (expt (cos t) 4))")
                                         (set! CART_INF "(- pi)")
                                         (send TFIELD_EXS_CART_INF set-value "(- pi)")
                                         (set! CART_SUP "pi")
                                         (send TFIELD_EXS_CART_SUP set-value "pi")
                                         (send MSG_EXS_CART set-label "")
                                         (send RBOX_EXS_CHOC set-selection 0)
                                         (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                         (send CANVAS_EXS on-paint)
                                         (change-fils GPANEL_EXS_SELECT (list LBOX_EXS RBOX_EXS_PARAM))
                                         (send TFIELD_EXS_CART_X enable #t)
                                         (send TFIELD_EXS_CART_Y enable #t)
                                         (send TFIELD_EXS_CART_INF enable #t)
                                         (send TFIELD_EXS_CART_SUP enable #t)
                                         (send SLIDER_EXS_ANGLE enable #t)
                                         (send BUTTON_EXS_CART enable #t)
                                         (change-fils GPANEL_EXS_REG (list TFIELD_EXS_CART_X TFIELD_EXS_CART_Y 
                                                                           TFIELD_EXS_CART_INF TFIELD_EXS_CART_SUP 
                                                                           MSG_EXS_CART BUTTON_EXS_CART SLIDER_EXS_G 
                                                                           SLIDER_EXS_E SLIDER_EXS_ANGLE))))))
                      (style '(single vertical-label))))

;; Définition d'une nouvelle classe radio-box dotée d'une nouvelle méthode get-label-item
(define my-radio-box%
  (class radio-box%
    (define/public (get-label-item str) ; retourne la position du bouton de label str
      (do ((i 0 (+ i 1)))
        ((equal? (send this get-item-label i) str) i)))
    (super-new)))

;; Radio-box pour les exemples de gravitation
(define RBOX_EXS_GRAV (new my-radio-box%
                           (label "Gravitation")
                           (choices (list "Barre" "Boucle infinie" "Ellipse" "Escargot" "Etoile" 
                                          "Fleur" "Haltere" "Polygone regulier" "Quadrillage"))
                           (parent GPANEL_EXS_SELECT)       
                           (callback
                            (lambda (rb evt)
                              (let ((str (send rb get-item-label (send rb get-selection))))      
                                (set! GRAVITE (make-vect 0 0.01))
                                (send SLIDER_EXS_G set-value 100)
                                (set! E 0.95)
                                (send SLIDER_EXS_E set-value 95)
                                (set! ANGLE PI/2)
                                (send SLIDER_EXS_ANGLE set-value 90)
                                (set! IND_ACTION #f)
                                (send BUTTON_EXS_ACTION set-label "Go")
                                (change-fils GPANEL_EXS_REG (list SLIDER_EXS_G SLIDER_EXS_E SLIDER_EXS_ANGLE))
                                (send SLIDER_EXS_ANGLE enable #t)
                                (cond 
                                  ((string=? str "Barre") (set! LONG_BAR 150)
                                                          (send SLIDER_EXS_BAR_LONG set-value 150)
                                                          (send SLIDER_EXS_BAR_LONG enable #t)
                                                          (send GPANEL_EXS_REG add-child SLIDER_EXS_BAR_LONG))
                                  ((string=? str "Boucle infinie") (set! TAILLE_INF 4)
                                                                   (send SLIDER_EXS_INF_TAILLE set-value 4)
                                                                   (send SLIDER_EXS_INF_TAILLE enable #t)
                                                                   (send GPANEL_EXS_REG add-child SLIDER_EXS_INF_TAILLE))
                                  ((string=? str "Ellipse") (set! TAILLE_ELL 100)
                                                            (send SLIDER_EXS_ELL_TAILLE set-value 100)
                                                            (send SLIDER_EXS_ELL_TAILLE enable #t)
                                                            (send GPANEL_EXS_REG add-child SLIDER_EXS_ELL_TAILLE))
                                  ((string=? str "Escargot") (set! TORSADE_ESC 8)
                                                             (send SLIDER_EXS_ESC_TORSADE set-value 8)
                                                             (send SLIDER_EXS_ESC_TORSADE enable #t)
                                                             (send GPANEL_EXS_REG add-child SLIDER_EXS_ESC_TORSADE))
                                  ((string=? str "Etoile") (set! NB_BRAN_STAR 12)
                                                           (send SLIDER_EXS_STAR_NB_BRAN set-value 6)
                                                           (send SLIDER_EXS_STAR_NB_BRAN enable #t)
                                                           (send GPANEL_EXS_REG add-child SLIDER_EXS_STAR_NB_BRAN))
                                  ((string=? str "Haltere") (set! TAILLE_HALT 3)
                                                            (send SLIDER_EXS_HALT_TAILLE set-value 3)
                                                            (send SLIDER_EXS_HALT_TAILLE enable #t)
                                                            (send GPANEL_EXS_REG add-child SLIDER_EXS_HALT_TAILLE))
                                  ((string=? str "Polygone regulier") (set! NB_COTES_POLYREG 5)
                                                                      (send SLIDER_EXS_POLYREG_NB_COTES set-value 5)
                                                                      (send SLIDER_EXS_POLYREG_NB_COTES enable #t)
                                                                      (send GPANEL_EXS_REG add-child 
                                                                            SLIDER_EXS_POLYREG_NB_COTES))
                                  ((string=? str "Quadrillage") (set! COURB_QUA 50)
                                                                (send SLIDER_EXS_QUA_COURB set-value 50)
                                                                (send SLIDER_EXS_QUA_COURB enable #t)
                                                                (send GPANEL_EXS_REG add-child SLIDER_EXS_QUA_COURB)))
                                (load (build-path "Exemples" (string-append str ".scm")))
                                (send CANVAS_EXS on-paint))))               
                           (style '(vertical vertical-label))))

;; Radio-box pour les exemples de chocs
(define RBOX_EXS_CHOC (new my-radio-box%
                           (label "Chocs")
                           (choices (list "Cercle et ellipse" "Formes diverses" "Oeil de hibou" "Oeil de Sauron" 
                                          "Polygones imbriques" "Triangles" "Triangles necklace"))
                           (parent GPANEL_EXS_SELECT)       
                           (callback
                            (lambda (rb evt)
                              (set! GRAVITE (make-vect 0 0))
                              (set! E 0.95)
                              (send SLIDER_EXS_E set-value 95)
                              (set! IND_ACTION #f)
                              (send BUTTON_EXS_ACTION set-label "Go")
                              (load (build-path "Exemples" (string-append 
                                                            (send rb get-item-label (send rb get-selection)) ".scm")))
                              (send CANVAS_EXS on-paint)))
                           (style '(vertical vertical-label deleted))))

;; Radio-box pour les exemples de paramétrisation
(define RBOX_EXS_PARAM (new my-radio-box%
                            (label "Parametrisation")
                            (choices (list "Courbe cartesienne" "Courbe polaire"))
                            (parent GPANEL_EXS_SELECT)
                            (callback
                             (lambda (rb evt)
                               (set! POLYGONE_EXS (polygone-nul))
                               (set! GRAVITE (make-vect 0 0.01))
                               (send SLIDER_EXS_G set-value 100)
                               (set! E 0.95)
                               (send SLIDER_EXS_E set-value 95)
                               (set! ANGLE 0)
                               (send SLIDER_EXS_ANGLE set-value 0)
                               (set! IND_ACTION #f)
                               (send BUTTON_EXS_ACTION set-label "Go")
                               (send BUTTON_EXS_ACTION enable #t)
                               (send SLIDER_EXS_ANGLE enable #t)
                               (if (string=? (send rb get-item-label (send rb get-selection)) "Courbe cartesienne")
                                   (begin (set! CART_X "(expt (sin t) 3)")
                                          (send TFIELD_EXS_CART_X set-value "(expt (sin t) 3)")
                                          (send TFIELD_EXS_CART_X enable #t)
                                          (set! CART_Y "(- (cos t) (expt (cos t) 4))")
                                          (send TFIELD_EXS_CART_Y set-value "(- (cos t) (expt (cos t) 4))")
                                          (send TFIELD_EXS_CART_Y enable #t)
                                          (set! CART_INF "(- pi)")
                                          (send TFIELD_EXS_CART_INF set-value "(- pi)")
                                          (send TFIELD_EXS_CART_INF enable #t)
                                          (set! CART_SUP "pi")
                                          (send TFIELD_EXS_CART_SUP set-value "pi")
                                          (send TFIELD_EXS_CART_SUP enable #t)
                                          (send MSG_EXS_CART set-label "")
                                          (send BUTTON_EXS_CART enable #t)
                                          (change-fils GPANEL_EXS_REG (list TFIELD_EXS_CART_X TFIELD_EXS_CART_Y 
                                                                            TFIELD_EXS_CART_INF TFIELD_EXS_CART_SUP
                                                                            MSG_EXS_CART BUTTON_EXS_CART SLIDER_EXS_G 
                                                                            SLIDER_EXS_E SLIDER_EXS_ANGLE))
                                          (load (build-path "Exemples" "Courbe cartesienne.scm")))
                                   (begin (set! POL_RHO 
                                                "(+ (- (exp (cos t)) (* 2 (cos (* 4 t)))) (expt (sin (/ t 12)) 5))")
                                          (send TFIELD_EXS_POL_RHO set-value 
                                                "(+ (- (exp (cos t)) (* 2 (cos (* 4 t)))) (expt (sin (/ t 12)) 5))")
                                          (send TFIELD_EXS_POL_RHO enable #t)
                                          (set! POL_INF "(- pi)")
                                          (send TFIELD_EXS_POL_INF set-value "(- pi)")
                                          (send TFIELD_EXS_POL_INF enable #t)
                                          (set! POL_SUP "pi")
                                          (send TFIELD_EXS_POL_SUP set-value "pi")
                                          (send TFIELD_EXS_POL_SUP enable #t)
                                          (send MSG_EXS_POL set-label "")
                                          (send BUTTON_EXS_POL enable #t)
                                          (change-fils GPANEL_EXS_REG (list TFIELD_EXS_POL_RHO TFIELD_EXS_POL_INF 
                                                                            TFIELD_EXS_POL_SUP MSG_EXS_POL BUTTON_EXS_POL 
                                                                            SLIDER_EXS_G SLIDER_EXS_E SLIDER_EXS_ANGLE))
                                          (load (build-path "Exemples" "Courbe polaire.scm"))))
                               (send CANVAS_EXS on-paint)))
                            (style '(vertical vertical-label deleted))))



;;; Panneau de visualisation pour les exemples
(define GPANEL_EXS_VISU (new group-box-panel%
                             (label "Visualisation")
                             (parent VPANEL1)
                             (style '(deleted))
                             (horiz-margin 10)
                             (border 5)
                             (alignment '(center center))))

;; Case à cocher pour l'enveloppe convexe
(define CBOX_EXS_EC (new check-box% 
                         (label "Enveloppe convexe")
                         (parent GPANEL_EXS_VISU)
                         (callback (lambda (c evt)
                                     (case AFFICHE_EC
                                       ((#t) (set! AFFICHE_EC #f)
                                             (send CANVAS_EXS on-paint))
                                       (else 
                                        (set! AFFICHE_EC #t)
                                        (send CANVAS_EXS on-paint)))))))

;; Case à cocher pour le centre de masse
(define CBOX_EXS_CM (new check-box% 
                         (label "Centre de masse")
                         (parent GPANEL_EXS_VISU)
                         (callback (lambda (c evt)
                                     (case AFFICHE_CM
                                       ((#t) (set! AFFICHE_CM #f)
                                             (send CANVAS_EXS on-paint))
                                       (else 
                                        (set! AFFICHE_CM #t)
                                        (send CANVAS_EXS on-paint)))))))

;;; Panneau de réglages des exemples
(define GPANEL_EXS_REG (new group-box-panel%
                            (label "Réglages")
                            (parent VPANEL1)
                            (style '(deleted))
                            (horiz-margin 10)
                            (alignment '(center center))
                            (stretchable-height #f)))

;; Slider pour régler l'effet de la gravité (pourcentage) pour les parties Gravitation et Parametrisation
(define SLIDER_EXS_G (new slider%
                          (label "Coefficient de gravitation (%)")
                          (min-value 0)
                          (max-value 100)
                          (init-value 100)
                          (parent GPANEL_EXS_REG)
                          (callback 
                           (lambda (sl evt)
                             (set! GRAVITE (make-vect 0 (/ (send sl get-value) 10000)))))
                          (style '(horizontal vertical-label))))

;; Slider pour régler le coefficient d'elasticite (pourcentage) pour tous les exemples
(define SLIDER_EXS_E (new slider%
                          (label "Coefficient d'élasticité (%)")
                          (min-value 50)
                          (max-value 100)
                          (init-value 100)
                          (parent GPANEL_EXS_REG)
                          (callback 
                           (lambda (sl evt)
                             (set! E (/ (send sl get-value) 100))))
                          (style '(horizontal vertical-label))))

;; Slider pour régler l'angle de l'exemple dans Gravitation et Parametrisation
(define SLIDER_EXS_ANGLE (new slider%
                              (label "Angle (en degré)")
                              (min-value 0)
                              (max-value 360)
                              (init-value 90)
                              (parent GPANEL_EXS_REG)
                              (callback 
                               (lambda (sl evt)
                                 (let ((rbox (cadr (send GPANEL_EXS_SELECT get-children))))
                                   (set! ANGLE (/ (* pi (send sl get-value)) 180))
                                   (load (build-path "Exemples" 
                                                     (string-append 
                                                      (send rbox get-item-label (send rbox get-selection))
                                                      ".scm")))
                                   (send CANVAS_EXS on-paint))))
                              (style '(horizontal vertical-label))))

;; Sliders pour la partie Chocs

;; Slider pour régler la longueur de la barre
(define SLIDER_EXS_BAR_LONG (new slider%
                                 (label "Longueur")
                                 (min-value 50)
                                 (max-value 250)
                                 (init-value 150)
                                 (parent GPANEL_EXS_REG)
                                 (callback 
                                  (lambda (sl evt)
                                    (set! LONG_BAR (send sl get-value))
                                    (load (build-path "Exemples" "Barre.scm"))
                                    (send CANVAS_EXS on-paint)))
                                 (style '(horizontal vertical-label))))

;; Slider pour régler la taille de la boucle infinie
(define SLIDER_EXS_INF_TAILLE (new slider%
                                   (label "Taille")
                                   (min-value 1)
                                   (max-value 8)
                                   (init-value 4)
                                   (parent GPANEL_EXS_REG)
                                   (callback 
                                    (lambda (sl evt)
                                      (set! TAILLE_INF (send sl get-value))
                                      (load (build-path "Exemples" "Boucle infinie.scm"))
                                      (send CANVAS_EXS on-paint)))
                                   (style '(horizontal vertical-label deleted))))

;; Slider pour régler la taille de l'ellipse
(define SLIDER_EXS_ELL_TAILLE (new slider%
                                   (label "Taille")
                                   (min-value 50)
                                   (max-value 150)
                                   (init-value 100)
                                   (parent GPANEL_EXS_REG)
                                   (callback 
                                    (lambda (sl evt)
                                      (set! TAILLE_ELL (send sl get-value))
                                      (load (build-path "Exemples" "Ellipse.scm"))
                                      (send CANVAS_EXS on-paint)))
                                   (style '(horizontal vertical-label deleted))))

;; Slider pour régler la torsade de l'escargot
(define SLIDER_EXS_ESC_TORSADE (new slider%
                                    (label "Torsade")
                                    (min-value 1)
                                    (max-value 10)
                                    (init-value 8)
                                    (parent GPANEL_EXS_REG)
                                    (callback 
                                     (lambda (sl evt)
                                       (set! TORSADE_ESC (send sl get-value))
                                       (load (build-path "Exemples" "Escargot.scm"))
                                       (send CANVAS_EXS on-paint)))
                                    (style '(horizontal vertical-label deleted))))

;; Slider pour régler le nombre de branches de l'étoile
(define SLIDER_EXS_STAR_NB_BRAN (new slider%
                                     (label "Nombre de branches")
                                     (min-value 2)
                                     (max-value 25)
                                     (init-value 6)
                                     (parent GPANEL_EXS_REG)
                                     (callback 
                                      (lambda (sl evt)
                                        (set! NB_BRAN_STAR (* (send sl get-value) 2))
                                        (load (build-path "Exemples" "Etoile.scm"))
                                        (send CANVAS_EXS on-paint)))
                                     (style '(horizontal vertical-label deleted))))

;; Slider pour régler le nombre de branches de l'étoile
(define SLIDER_EXS_HALT_TAILLE (new slider%
                                    (label "Taille de la boule du bas")
                                    (min-value 1)
                                    (max-value 6)
                                    (init-value 3)
                                    (parent GPANEL_EXS_REG)
                                    (callback 
                                     (lambda (sl evt)
                                       (set! TAILLE_HALT (send sl get-value))
                                       (load (build-path "Exemples" "Haltere.scm"))
                                       (send CANVAS_EXS on-paint)))
                                    (style '(horizontal vertical-label deleted))))

;; Slider pour régler le nombre de côtés du polygone régulier
(define SLIDER_EXS_POLYREG_NB_COTES (new slider%
                                         (label "Nombre de côtés")
                                         (min-value 3)
                                         (max-value 10)
                                         (init-value 5)
                                         (parent GPANEL_EXS_REG)
                                         (callback 
                                          (lambda (sl evt)
                                            (set! NB_COTES_POLYREG (send sl get-value))
                                            (load (build-path "Exemples" "Polygone regulier.scm"))
                                            (send CANVAS_EXS on-paint)))
                                         (style '(horizontal vertical-label deleted))))

;; Slider pour régler la courbure du quadrillage
(define SLIDER_EXS_QUA_COURB (new slider%
                                  (label "Courbure")
                                  (min-value 1)
                                  (max-value 100)
                                  (init-value 50)
                                  (parent GPANEL_EXS_REG)
                                  (callback 
                                   (lambda (sl evt)
                                     (set! COURB_QUA (send sl get-value))
                                     (load (build-path "Exemples" "Quadrillage.scm"))
                                     (send CANVAS_EXS on-paint)))
                                  (style '(horizontal vertical-label deleted))))




;; Zones de texte, boutons et messages pour la partie Parametrisation

;; ¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡ ATTENTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;les zones de texte lorsqu'elles sont rendues inactives ne sont plus éditables sur Windows mais le sont sur MAC OS X



;; Zone de texte pour déterminer x en fonction de t dans la paramétrisation cartésienne
(define TFIELD_EXS_CART_X (new text-field%
                               (label "x(t) = ")
                               (parent GPANEL_EXS_REG)
                               (callback 
                                (lambda (tf evt)
                                  (if (equal? (send evt get-event-type) 'text-field-enter)
                                      (begin (set! POLYGONE_EXS (polygone-nul))
                                             (send MSG_EXS_CART set-label "")
                                             (set! CART_X (send tf get-value))
                                             (set! CART_Y (send TFIELD_EXS_CART_Y get-value))
                                             (set! CART_INF (send TFIELD_EXS_CART_INF get-value))
                                             (set! CART_SUP (send TFIELD_EXS_CART_SUP get-value))
                                             (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                             (when (not (polygone-nul? POLYGONE_EXS))
                                               (send CANVAS_EXS on-paint)
                                               (send SLIDER_EXS_ANGLE enable #t)
                                               (send BUTTON_EXS_ACTION enable #t)))
                                      (begin (send BUTTON_EXS_ACTION enable #f)
                                             (send SLIDER_EXS_ANGLE enable #f)))))
                               (style '(single vertical-label deleted))))

;; Zone de texte pour déterminer y en fonction de t dans la paramétrisation cartésienne
(define TFIELD_EXS_CART_Y (new text-field%
                               (label "y(t) = ")
                               (parent GPANEL_EXS_REG)
                               (callback 
                                (lambda (tf evt)
                                  (if (equal? (send evt get-event-type) 'text-field-enter)
                                      (begin (set! POLYGONE_EXS (polygone-nul))
                                             (send MSG_EXS_CART set-label "")
                                             (set! CART_X (send TFIELD_EXS_CART_X get-value))
                                             (set! CART_Y (send tf get-value))
                                             (set! CART_INF (send TFIELD_EXS_CART_INF get-value))
                                             (set! CART_SUP (send TFIELD_EXS_CART_SUP get-value))
                                             (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                             (when (not (polygone-nul? POLYGONE_EXS))
                                               (send CANVAS_EXS on-paint)
                                               (send SLIDER_EXS_ANGLE enable #t)
                                               (send BUTTON_EXS_ACTION enable #t)))
                                      (begin (send BUTTON_EXS_ACTION enable #f)
                                             (send SLIDER_EXS_ANGLE enable #f)))))
                               (style '(single vertical-label deleted))))

;; Zone de texte pour déterminer la borne inférieure dans la paramétrisation cartésienne
(define TFIELD_EXS_CART_INF (new text-field%
                                 (label "Borne inférieure = ")
                                 (parent GPANEL_EXS_REG)
                                 (callback 
                                  (lambda (tf evt)
                                    (if (equal? (send evt get-event-type) 'text-field-enter)
                                        (begin (set! POLYGONE_EXS (polygone-nul))
                                               (send MSG_EXS_CART set-label "")
                                               (set! CART_X (send TFIELD_EXS_CART_X get-value))
                                               (set! CART_Y (send TFIELD_EXS_CART_Y get-value))
                                               (set! CART_INF (send tf get-value))
                                               (set! CART_SUP (send TFIELD_EXS_CART_SUP get-value))
                                               (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                               (when (not (polygone-nul? POLYGONE_EXS))
                                                 (send CANVAS_EXS on-paint)
                                                 (send SLIDER_EXS_ANGLE enable #t)
                                                 (send BUTTON_EXS_ACTION enable #t)))
                                        (begin (send BUTTON_EXS_ACTION enable #f)
                                               (send SLIDER_EXS_ANGLE enable #f)))))
                                 (style '(single vertical-label deleted))))

;; Zone de texte pour déterminer la borne supérieure dans la paramétrisation cartésienne
(define TFIELD_EXS_CART_SUP (new text-field%
                                 (label "Borne supérieure = ")
                                 (parent GPANEL_EXS_REG)
                                 (callback 
                                  (lambda (tf evt)
                                    (if (equal? (send evt get-event-type) 'text-field-enter)
                                        (begin (set! POLYGONE_EXS (polygone-nul))
                                               (send MSG_EXS_CART set-label "")
                                               (set! CART_X (send TFIELD_EXS_CART_X get-value))
                                               (set! CART_Y (send TFIELD_EXS_CART_Y get-value))
                                               (set! CART_INF (send TFIELD_EXS_CART_INF get-value))
                                               (set! CART_SUP (send tf get-value))
                                               (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                               (when (not (polygone-nul? POLYGONE_EXS))
                                                 (send CANVAS_EXS on-paint)
                                                 (send SLIDER_EXS_ANGLE enable #t)
                                                 (send BUTTON_EXS_ACTION enable #t)))
                                        (begin (send BUTTON_EXS_ACTION enable #f)
                                               (send SLIDER_EXS_ANGLE enable #f)))))
                                 (style '(single vertical-label deleted))))

;; Bouton pour créer le polygone selon la paramétrisation cartésienne donnée au-dessus
(define BUTTON_EXS_CART (new button%
                             (label "Créer")
                             (parent GPANEL_EXS_REG)
                             (callback
                              (lambda (obj evt)
                                (set! POLYGONE_EXS (polygone-nul))
                                (send MSG_EXS_CART set-label "")
                                (set! CART_X (send TFIELD_EXS_CART_X get-value))
                                (set! CART_Y (send TFIELD_EXS_CART_Y get-value))
                                (set! CART_INF (send TFIELD_EXS_CART_INF get-value))
                                (set! CART_SUP (send TFIELD_EXS_CART_SUP get-value))
                                (load (build-path "Exemples" "Courbe cartesienne.scm"))
                                (when (not (polygone-nul? POLYGONE_EXS))
                                  (send CANVAS_EXS on-paint)
                                  (send SLIDER_EXS_ANGLE enable #t)
                                  (send BUTTON_EXS_ACTION enable #t))))
                             (style '(deleted))))

;; Zone de message pour signaler les erreurs de la paramétrisation cartésienne de l'utilisateur
(define MSG_EXS_CART (new message%
                          (label "                                                                                                                                          ")
                          ; Les espaces dans le label ci-desus permettent de contenir les (éventuels) messages d'erreur
                          (parent GPANEL_EXS_REG)
                          (style '(deleted))))

;; Zone de texte pour déterminer rho en fonction de t dans la paramétrisation polaire
(define TFIELD_EXS_POL_RHO (new text-field%
                                (label "\u03c1(t) = ")
                                (parent GPANEL_EXS_REG)
                                (callback 
                                 (lambda (tf evt)
                                   (if (equal? (send evt get-event-type) 'text-field-enter)
                                       (begin (set! POLYGONE_EXS (polygone-nul))
                                              (send MSG_EXS_POL set-label "")
                                              (set! POL_RHO (send tf get-value))
                                              (set! POL_INF (send TFIELD_EXS_POL_INF get-value))
                                              (set! POL_SUP (send TFIELD_EXS_POL_SUP get-value))
                                              (load (build-path "Exemples" "Courbe polaire.scm"))
                                              (when (not (polygone-nul? POLYGONE_EXS))
                                                (send CANVAS_EXS on-paint)
                                                (send SLIDER_EXS_ANGLE enable #t)
                                                (send BUTTON_EXS_ACTION enable #t)))
                                       (begin (send BUTTON_EXS_ACTION enable #f)
                                              (send SLIDER_EXS_ANGLE enable #f)))))
                                (style '(single vertical-label deleted))))

;; Zone de texte pour déterminer la borne inférieure dans la paramétrisation polaire
(define TFIELD_EXS_POL_INF (new text-field%
                                (label "Borne inférieure = ")
                                (parent GPANEL_EXS_REG)
                                (callback 
                                 (lambda (tf evt)
                                   (if (equal? (send evt get-event-type) 'text-field-enter)
                                       (begin (set! POLYGONE_EXS (polygone-nul))
                                              (send MSG_EXS_POL set-label "")
                                              (set! POL_RHO (send TFIELD_EXS_POL_RHO get-value))
                                              (set! POL_INF (send tf get-value))
                                              (set! POL_SUP (send TFIELD_EXS_POL_SUP get-value))
                                              (load (build-path "Exemples" "Courbe polaire.scm"))
                                              (when (not (polygone-nul? POLYGONE_EXS))
                                                (send CANVAS_EXS on-paint)
                                                (send SLIDER_EXS_ANGLE enable #t)
                                                (send BUTTON_EXS_ACTION enable #t)))
                                       (begin (send BUTTON_EXS_ACTION enable #f)
                                              (send SLIDER_EXS_ANGLE enable #f)))))
                                (style '(single vertical-label deleted))))

;; Zone de texte pour déterminer la borne supérieure dans la paramétrisation polaire
(define TFIELD_EXS_POL_SUP (new text-field%
                                (label "Borne supérieure = ")
                                (parent GPANEL_EXS_REG)
                                (callback 
                                 (lambda (tf evt)
                                   (if (equal? (send evt get-event-type) 'text-field-enter)
                                       (begin (set! POLYGONE_EXS (polygone-nul))
                                              (send MSG_EXS_POL set-label "")
                                              (set! POL_RHO (send TFIELD_EXS_POL_RHO get-value))
                                              (set! POL_INF (send TFIELD_EXS_POL_INF get-value))
                                              (set! POL_SUP (send tf get-value))
                                              (load (build-path "Exemples" "Courbe polaire.scm"))
                                              (when (not (polygone-nul? POLYGONE_EXS))
                                                (send CANVAS_EXS on-paint)
                                                (send SLIDER_EXS_ANGLE enable #t)
                                                (send BUTTON_EXS_ACTION enable #t)))
                                       (begin (send BUTTON_EXS_ACTION enable #f)
                                              (send SLIDER_EXS_ANGLE enable #f)))))
                                (style '(single vertical-label deleted))))

;; Bouton pour créer le polygone selon la paramétrisation polaire donnée au-dessus
(define BUTTON_EXS_POL (new button%
                            (label "Créer")
                            (parent GPANEL_EXS_REG)
                            (callback
                             (lambda (obj evt)
                               (set! POLYGONE_EXS (polygone-nul))
                               (send MSG_EXS_POL set-label "")
                               (set! POL_RHO (send TFIELD_EXS_POL_RHO get-value))
                               (set! POL_INF (send TFIELD_EXS_POL_INF get-value))
                               (set! POL_SUP (send TFIELD_EXS_POL_SUP get-value))
                               (load (build-path "Exemples" "Courbe polaire.scm"))
                               (when (not (polygone-nul? POLYGONE_EXS))
                                 (send CANVAS_EXS on-paint)
                                 (send SLIDER_EXS_ANGLE enable #t)
                                 (send BUTTON_EXS_ACTION enable #t))))
                            (style '(deleted))))

;; Zone de message pour signaler les erreurs de la paramétrisation polaire de l'utilisateur
(define MSG_EXS_POL (new message%
                         (label "                                                                                                                                          ")
                         (parent GPANEL_EXS_REG)
                         (style '(deleted))))

;;; Panneau de boutons
(define VPANEL_EXS_BUTS (new vertical-panel%
                             (parent VPANEL1)
                             (style '(deleted))
                             (horiz-margin 10)
                             (border 5)
                             (alignment '(center center))
                             (stretchable-height #f)))

;; Bouton pour enclencher l'animation
(define BUTTON_EXS_ACTION
  (new button%
       (label "Go")
       (parent VPANEL_EXS_BUTS)       
       (callback
        (lambda (b evt)
          (case IND_ACTION
            ((#t) (set! IND_ACTION #f)
                  (send BUTTON_EXS_ACTION set-label "Go")
                  (case (car (send LBOX_EXS get-selections))
                    ((0) (send SLIDER_EXS_ANGLE enable #t)
                         (let ((str (send RBOX_EXS_GRAV get-item-label (send RBOX_EXS_GRAV get-selection))))
                           (cond
                             ((string=? str "Barre") (send SLIDER_EXS_BAR_LONG enable #t))
                             ((string=? str "Boucle infinie") (send SLIDER_EXS_INF_TAILLE enable #t))
                             ((string=? str "Ellipse") (send SLIDER_EXS_ELL_TAILLE enable #t))
                             ((string=? str "Escargot") (send SLIDER_EXS_ESC_TORSADE enable #t))
                             ((string=? str "Etoile") (send SLIDER_EXS_STAR_NB_BRAN enable #t))
                             ((string=? str "Haltere") (send SLIDER_EXS_HALT_TAILLE enable #t))
                             ((string=? str "Polygone regulier") (send SLIDER_EXS_POLYREG_NB_COTES enable #t))
                             ((string=? str "Quadrillage") (send SLIDER_EXS_QUA_COURB enable #t)))))
                    ((1) ())
                    ((2) (send SLIDER_EXS_ANGLE enable #t)
                         (if (string=? (send RBOX_EXS_PARAM get-item-label (send RBOX_EXS_PARAM get-selection))
                                       "Courbe cartesienne")
                             (begin (send TFIELD_EXS_CART_X enable #t)
                                    (send TFIELD_EXS_CART_Y enable #t)
                                    (send TFIELD_EXS_CART_INF enable #t)
                                    (send TFIELD_EXS_CART_SUP enable #t)
                                    (send BUTTON_EXS_CART enable #t))
                             (begin (send TFIELD_EXS_POL_RHO enable #t)
                                    (send TFIELD_EXS_POL_INF enable #t)
                                    (send TFIELD_EXS_POL_SUP enable #t)
                                    (send BUTTON_EXS_POL enable #t)))))
                  (send CANVAS_EXS on-paint))
            (else (set! IND_ACTION #t)
                  (send BUTTON_EXS_ACTION set-label "Stop")
                  (case (car (send LBOX_EXS get-selections))
                    ((0) (send SLIDER_EXS_ANGLE enable #f)
                         (let ((str (send RBOX_EXS_GRAV get-item-label (send RBOX_EXS_GRAV get-selection))))
                           (cond
                             ((string=? str "Barre") (send SLIDER_EXS_BAR_LONG enable #f))
                             ((string=? str "Boucle infinie") (send SLIDER_EXS_INF_TAILLE enable #f))
                             ((string=? str "Ellipse") (send SLIDER_EXS_ELL_TAILLE enable #f))
                             ((string=? str "Escargot") (send SLIDER_EXS_ESC_TORSADE enable #f))
                             ((string=? str "Etoile") (send SLIDER_EXS_STAR_NB_BRAN enable #f))
                             ((string=? str "Haltere") (send SLIDER_EXS_HALT_TAILLE enable #f))
                             ((string=? str "Polygone regulier") (send SLIDER_EXS_POLYREG_NB_COTES enable #f))
                             ((string=? str "Quadrillage") (send SLIDER_EXS_QUA_COURB enable #f)))))
                    ((1) ())
                    ((2) (send SLIDER_EXS_ANGLE enable #f)
                         (if (string=? (send RBOX_EXS_PARAM get-item-label (send RBOX_EXS_PARAM get-selection))
                                       "Courbe cartesienne")
                             (begin (send TFIELD_EXS_CART_X enable #f)
                                    (send TFIELD_EXS_CART_Y enable #f)
                                    (send TFIELD_EXS_CART_INF enable #f)
                                    (send TFIELD_EXS_CART_SUP enable #f)
                                    (send BUTTON_EXS_CART enable #f))
                             (begin (send TFIELD_EXS_POL_RHO enable #f)
                                    (send TFIELD_EXS_POL_INF enable #f)
                                    (send TFIELD_EXS_POL_SUP enable #f)
                                    (send BUTTON_EXS_POL enable #f)))))
                  (send CANVAS_EXS on-paint)))))))

;; Bouton de retour au menu initial
(define BUTTON_EXS_MENU
  (new button%
       (label "Menu")
       (parent VPANEL_EXS_BUTS)       
       (callback
        (lambda (b evt)
          (set! IND_ACTION #f)
          (change-fils VPANEL1 (list GPANEL_INIT_SELECT GPANEL_INIT_DOC))
          (change-fils VPANEL2 (list CANVAS_INIT))))))

;;;; Panneaux pour la partie objet

;;; Panneau de sélection des exemples objet
(define GPANEL_OBJ_SELECT (new group-box-panel%
                               (label "Exemples Objet")
                               (parent VPANEL1)
                               (style '(deleted))
                               (vert-margin 5)
                               (horiz-margin 10)
                               (spacing 10)
                               (border 5)
                               (alignment '(center center))
                               (stretchable-height #f)))

;; Radio-box pour les phénomènes physiques
(define RBOX_OBJ (new my-radio-box%
                      (label "Phénomènes physiques")
                      (choices (list "Deux boules" "Boules multiples" "Gravite" "Pendule"))
                      (parent GPANEL_OBJ_SELECT)       
                      (callback
                       (lambda (rb evt)
                         (let ((str (send rb get-item-label (send rb get-selection))))
                           (init-objet)
                           (set! IND_ACTION #f)
                           (send BUTTON_OBJ_ACTION set-label "Go")
                           (cond 
                             ((string=? str "Deux boules") (set! M1_2B 1)
                                                           (send SLIDER_OBJ_2B_M1 set-value 1)
                                                           (send SLIDER_OBJ_2B_M1 enable #t)
                                                           (set! M2_2B 1)
                                                           (send SLIDER_OBJ_2B_M2 set-value 1)
                                                           (send SLIDER_OBJ_2B_M2 enable #t)
                                                           (set! V1_2B (make-vect -5 0))
                                                           (send SLIDER_OBJ_2B_V1 set-value -5)
                                                           (send SLIDER_OBJ_2B_V1 enable #t)
                                                           (set! V2_2B (make-vect 0 0))
                                                           (send SLIDER_OBJ_2B_V2 set-value 0)
                                                           (send SLIDER_OBJ_2B_V2 enable #t)
                                                           (set! R1_2B 60)
                                                           (send SLIDER_OBJ_2B_R1 set-value 60)
                                                           (send SLIDER_OBJ_2B_R1 enable #t)
                                                           (set! R2_2B 60)
                                                           (send SLIDER_OBJ_2B_R2 set-value 60)
                                                           (send SLIDER_OBJ_2B_R2 enable #t)
                                                           (change-fils GPANEL_OBJ_REG 
                                                                        (list SLIDER_OBJ_2B_M1 SLIDER_OBJ_2B_M2 
                                                                              SLIDER_OBJ_2B_R1 SLIDER_OBJ_2B_R2 
                                                                              SLIDER_OBJ_2B_V1 SLIDER_OBJ_2B_V2)))
                             ((string=? str "Boules multiples") (set! NB_BAL 20)
                                                                (send SLIDER_OBJ_BAL_NB set-value 20)
                                                                (send SLIDER_OBJ_BAL_NB enable #t)
                                                                (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_BAL_NB)))
                             ((string=? str "Gravite") (set! GRAVITE 0.1)
                                                       (send SLIDER_OBJ_GRAV_G set-value 10)
                                                       (send SLIDER_OBJ_GRAV_G enable #t)
                                                       (set! M_GRAV 1)
                                                       (send SLIDER_OBJ_GRAV_M set-value 1)
                                                       (send SLIDER_OBJ_GRAV_M enable #t)
                                                       (set! V_GRAV (make-vect -1 0))
                                                       (send SLIDER_OBJ_GRAV_V set-value -1)
                                                       (send SLIDER_OBJ_GRAV_V enable #t)
                                                       (set! R_GRAV 50)
                                                       (send SLIDER_OBJ_GRAV_R set-value 50)
                                                       (send SLIDER_OBJ_GRAV_R enable #t)
                                                       (change-fils GPANEL_OBJ_REG 
                                                                    (list SLIDER_OBJ_GRAV_G SLIDER_OBJ_GRAV_M
                                                                          SLIDER_OBJ_GRAV_R SLIDER_OBJ_GRAV_V)))
                             ((string=? str "Pendule") (set! NB_PEND 5)
                                                       (send SLIDER_OBJ_PEND_NB set-value 5)
                                                       (send SLIDER_OBJ_PEND_NB enable #t)
                                                       (change-fils GPANEL_OBJ_REG (list SLIDER_OBJ_PEND_NB))))
                           (load (build-path "Objets" (string-append 
                                                       (send rb get-item-label (send rb get-selection)) ".scm")))
                           (send CANVAS_OBJ on-paint))))
                      (style '(vertical vertical-label))))

;;; Panneau de visualisation des exemples objet
(define GPANEL_OBJ_VISU (new group-box-panel%
                             (label "Visualisation")
                             (parent VPANEL1)
                             (style '(deleted))
                             (vert-margin 5)
                             (horiz-margin 10)
                             (spacing 10)
                             (border 5)
                             (alignment '(center center))))

;; Case à cocher pour le centre de masse
(define CBOX_OBJ_CM (new check-box% 
                         (label "Centre de masse du système")
                         (parent GPANEL_OBJ_VISU)
                         (callback (lambda (c evt)
                                     (case AFFICHE_OBJ_CM
                                       ((#t) (set! AFFICHE_OBJ_CM #f)
                                             (send CANVAS_OBJ on-paint))
                                       (else 
                                        (set! AFFICHE_OBJ_CM #t)
                                        (send CANVAS_OBJ on-paint))))) ))

;;; Panneau de réglages des exemples objet
(define GPANEL_OBJ_REG (new group-box-panel%
                            (label "Réglages")
                            (parent VPANEL1)
                            (style '(deleted))
                            (vert-margin 5)
                            (horiz-margin 10)
                            (spacing 10)
                            (border 5)
                            (alignment '(center center))
                            (stretchable-height #f)))

;; Slider pour régler la masse de la première boule dans Deux Boules
(define SLIDER_OBJ_2B_M1 (new slider%
                              (label "Masse de la 1ère boule")
                              (min-value 1)
                              (max-value 7)
                              (init-value 1)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! M1_2B (send sl get-value))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler la masse de la deuxième boule dans Deux Boules
(define SLIDER_OBJ_2B_M2 (new slider%
                              (label "Masse de la 2ème boule")
                              (min-value 1)
                              (max-value 7)
                              (init-value 1)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! M2_2B (send sl get-value))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler le rayon de la première boule dans Deux Boules
(define SLIDER_OBJ_2B_R1 (new slider%
                              (label "Rayon de la 1ère boule")
                              (min-value 10)
                              (max-value 100)
                              (init-value 60)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! R1_2B (send sl get-value))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler le rayon de la deuxième boule dans Deux Boules
(define SLIDER_OBJ_2B_R2 (new slider%
                              (label "Rayon de la 2éme boule")
                              (min-value 10)
                              (max-value 100)
                              (init-value 60)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! R2_2B (send sl get-value))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler la vitesse de la première boule dans Deux Boules
(define SLIDER_OBJ_2B_V1 (new slider%
                              (label "Vitesse de la 1ère boule")
                              (min-value -5)
                              (max-value 5)
                              (init-value 0)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! V1_2B (make-vect (send sl get-value) 0))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler la vitesse de la deuxième boule dans Deux Boules
(define SLIDER_OBJ_2B_V2 (new slider%
                              (label "Vitesse de la 2éme boule")
                              (min-value -5)
                              (max-value 5)
                              (init-value -5)
                              (parent GPANEL_OBJ_REG)
                              (callback 
                               (lambda (sl evt)
                                 (init-objet)
                                 (set! V2_2B (make-vect (send sl get-value) 0))
                                 (load (build-path "Objets" "Deux boules.scm"))
                                 (send CANVAS_OBJ on-paint)))
                              (style '(horizontal vertical-label))))

;; Slider pour régler le nombre de boules dans Boules Multiples
(define SLIDER_OBJ_BAL_NB (new slider%
                               (label "Nombre de boules")
                               (min-value 10)
                               (max-value 30)
                               (init-value 20)
                               (parent GPANEL_OBJ_REG)
                               (callback 
                                (lambda (sl evt)
                                  (init-objet)
                                  (set! NB_BAL (send sl get-value))
                                  (actualiser_nombre)
                                  (send CANVAS_OBJ on-paint)))
                               (style '(horizontal vertical-label deleted))))

;; Slider pour régler la gravitation dans Gravité
(define SLIDER_OBJ_GRAV_G (new slider%
                               (label "Coefficient de gravitation (%)")
                               (min-value 0)
                               (max-value 100)
                               (init-value 10)
                               (parent GPANEL_OBJ_REG)
                               (callback 
                                (lambda (sl evt)
                                  (if (not IND_ACTION)
                                      (begin (init-objet)
                                             (load (build-path "Objets" "Gravite.scm"))))
                                  (set! GRAVITE (/ (send sl get-value) 100))
                                  (send CANVAS_OBJ on-paint)))
                               (style '(horizontal vertical-label deleted))))

;; Slider pour régler la masse dans Gravité
(define SLIDER_OBJ_GRAV_M (new slider%
                               (label "Masse")
                               (min-value 1)
                               (max-value 7)
                               (init-value 1)
                               (parent GPANEL_OBJ_REG)
                               (callback 
                                (lambda (sl evt)
                                  (init-objet)
                                  (set! GRAVITE (/ (send SLIDER_OBJ_GRAV_G get-value) 100))
                                  (set! M_GRAV (send sl get-value))
                                  (load (build-path "Objets" "Gravite.scm"))
                                  (send CANVAS_OBJ on-paint)))
                               (style '(horizontal vertical-label deleted))))

;; Slider pour régler le rayon dans Gravité
(define SLIDER_OBJ_GRAV_R (new slider%
                               (label "Rayon")
                               (min-value 10)
                               (max-value 100)
                               (init-value 50)
                               (parent GPANEL_OBJ_REG)
                               (callback 
                                (lambda (sl evt)
                                  (init-objet)
                                  (set! GRAVITE (/ (send SLIDER_OBJ_GRAV_G get-value) 100))
                                  (set! R_GRAV (send sl get-value))
                                  (load (build-path "Objets" "Gravite.scm"))
                                  (send CANVAS_OBJ on-paint)))
                               (style '(horizontal vertical-label))))

;; Slider pour régler la vitesse dans Gravité
(define SLIDER_OBJ_GRAV_V (new slider%
                               (label "Vitesse")
                               (min-value -10)
                               (max-value 10)
                               (init-value -1)
                               (parent GPANEL_OBJ_REG)
                               (callback 
                                (lambda (sl evt)
                                  (init-objet)
                                  (set! GRAVITE (/ (send SLIDER_OBJ_GRAV_G get-value) 100))
                                  (set! V_GRAV (make-vect (send sl get-value) 0))
                                  (load (build-path "Objets" "Gravite.scm"))
                                  (send CANVAS_OBJ on-paint)))
                               (style '(horizontal vertical-label))))

;; Slider pour régler le nombre de boules dans Pendule
(define SLIDER_OBJ_PEND_NB (new slider%
                                (label "Nombre de boules")
                                (min-value 3)
                                (max-value 7)
                                (init-value 5)
                                (parent GPANEL_OBJ_REG)
                                (callback 
                                 (lambda (sl evt)
                                   (init-objet)
                                   (set! NB_PEND (send sl get-value))
                                   (load (build-path "Objets" "Pendule.scm"))
                                   (send CANVAS_OBJ on-paint)))
                                (style '(horizontal vertical-label deleted))))

;;; Panneau de boutons
(define VPANEL_OBJ_BUTS (new vertical-panel%
                             (parent VPANEL1)
                             (style '(deleted))
                             (vert-margin 5)
                             (horiz-margin 10)
                             (spacing 10)
                             (border 5)
                             (alignment '(center center))
                             (stretchable-height #f)))

;; Bouton pour enclencher l'animation
(define BUTTON_OBJ_ACTION
  (new button%
       (label "Go")
       (parent VPANEL_OBJ_BUTS)       
       (callback
        (lambda (b evt)
          (let ((str (send RBOX_OBJ get-item-label (send RBOX_OBJ get-selection))))
            (case IND_ACTION
              ((#t) (set! IND_ACTION #f)
                    (send BUTTON_OBJ_ACTION set-label "Go")
                    (cond
                      ((string=? str "Deux boules") (send SLIDER_OBJ_2B_M1 enable #t)
                                                    (send SLIDER_OBJ_2B_M2 enable #t)
                                                    (send SLIDER_OBJ_2B_R1 enable #t)
                                                    (send SLIDER_OBJ_2B_R2 enable #t)
                                                    (send SLIDER_OBJ_2B_V1 enable #t)
                                                    (send SLIDER_OBJ_2B_V2 enable #t))
                      ((string=? str "Boules multiples") (void))
                      ((string=? str "Gravite") (send SLIDER_OBJ_GRAV_M enable #t)
                                                (send SLIDER_OBJ_GRAV_R enable #t)
                                                (send SLIDER_OBJ_GRAV_V enable #t))
                      ((string=? str "Pendule") (send SLIDER_OBJ_PEND_NB enable #t)))
                    (send CANVAS_OBJ on-paint))
              (else (set! IND_ACTION #t)
                    (send BUTTON_OBJ_ACTION set-label "Stop") 
                    (cond
                      ((string=? str "Deux boules") (send SLIDER_OBJ_2B_M1 enable #f)
                                                    (send SLIDER_OBJ_2B_M2 enable #f)
                                                    (send SLIDER_OBJ_2B_R1 enable #f)
                                                    (send SLIDER_OBJ_2B_R2 enable #f)
                                                    (send SLIDER_OBJ_2B_V1 enable #f)
                                                    (send SLIDER_OBJ_2B_V2 enable #f))
                      ((string=? str "Boules multiples") (send SLIDER_OBJ_BAL_NB enable #f))
                      ((string=? str "Gravite") (send SLIDER_OBJ_GRAV_M enable #f)
                                                (send SLIDER_OBJ_GRAV_R enable #f)
                                                (send SLIDER_OBJ_GRAV_V enable #f))
                      ((string=? str "Pendule") (send SLIDER_OBJ_PEND_NB enable #f)))
                    (send CANVAS_OBJ on-paint))))))))

;; Bouton de retour au menu initial
(define BUTTON_OBJ_MENU
  (new button%
       (label "Menu")
       (parent VPANEL_OBJ_BUTS)       
       (callback
        (lambda (b evt)
          (send BITMAP-DC set-background COLOR_BACK)
          (set! IND_ACTION #f)
          (send AFF_TRA enable #t)
          (send AFF_CM enable #t)
          (send AFF_EC enable #t)
          (send AFF_FOND set-label "Changer la couleur de fond pour les sections Dessin et Exemples")
          (change-fils VPANEL1 (list GPANEL_INIT_SELECT GPANEL_INIT_DOC))
          (change-fils VPANEL2 (list CANVAS_INIT))))))

;*****************************************************

;                GESTION DES CANVAS

;*****************************************************

;;;; Bitmap-dc associé à un bitmap pour le double-buffer utilisés pour tous les canvas
(define BITMAP (make-object bitmap% 500 500)) 
(define BITMAP-DC (new bitmap-dc% (bitmap BITMAP)))

(send BITMAP-DC set-background (make-object color% "azure"))

;;;; Canvas initial
(define CANVAS_INIT (new canvas% 
                         (parent VPANEL2)
                         (min-width 615)
                         (min-height 460)
                         (paint-callback 
                          (lambda (c dc)
                            (send dc draw-bitmap (make-object bitmap% 
                                                   (build-path 'same "Images" "Icones" "sommaire.jpg")
                                                   'jpeg          
                                                   #f)
                                  0 0 'solid)))
                         (stretchable-width #f)
                         (stretchable-height #f)))

;;;; Canvas pour le dessin

;; Méthodes privées
(define (affiche poly dc type_polygone) ; aussi utilisée pour les exemples
  (send dc clear)
  (when (vector? poly) 
    (let ((Lpoly (vector->list poly)))
      (map (lambda (x) 
             (affiche-polygone x dc COLOR_POLY 'exemples)
             (affiche-polygone x dc COLOR_POLY 'exemples))
           Lpoly)
      (when AFFICHE_EC (map (lambda (x) (affiche-ev x dc COLOR_EC)) Lpoly))
      (when AFFICHE_CM (map (lambda (x) (affiche-centre x dc COLOR_CM)) Lpoly))))
  (when (not (vector? poly))
    (if (equal? 'exemples type_polygone)
        (affiche-polygone poly dc COLOR_POLY 'exemples)
        (affiche-polygone poly dc COLOR_POLY 'ligne_polygonale))
    (when AFFICHE_EC (affiche-ev poly dc COLOR_EC))
    (when AFFICHE_CM (affiche-centre poly dc COLOR_CM))))

(define (action poly) ; aussi utilisée pour les exemples
  (when (vector? poly) 
    (for i from 0 to (- (vector-length poly) 1)
         (for j from (add1 i) to (- (vector-length poly) 1)
              (when
                  (<= (norme (-vect (masse-r (polygone-cm (vector-ref poly i))) (masse-r (polygone-cm (vector-ref poly j))))) 
                      (+ (polygone-rayon (vector-ref poly i)) (polygone-rayon (vector-ref poly j))))
                (let ((inter (intersection (vector-ref poly i) (vector-ref poly j) 0.5)))
                  (when inter (evolution inter))))))
    (map (lambda (x) 
           (next x E))
         (vector->list poly)))
  (when (not (vector? poly))
    (next poly E)))

(define (rajout dc evt)
  (let ((x (max 10 (min 490 (send evt get-x))))
        (y (max 10 (min 490 (send evt get-y)))))
    (ajouter-point x y) ; Considération du point
    (affiche-liste L dc COLOR_POLY))) ; Affichage des points

; Définition d'une nouvelle classe Canvas pour le dessin
(define canvas-draw%
  (class canvas%
    (init-field (evolution (make-vect 0 0)))
    (define/override (on-event evt)
      (if (polygone-nul? POLYGONE)
          ; Le canvas est vierge, le dessin n'a pas été tracé
          (case (send evt get-event-type)
            ((left-down) (rajout (send this get-dc) evt)
                         (set! IND_CLIC #t)
                         (set! IND_ACTION #f)
                         (send BUTTON_DRAW_ACTION set-label "Go")
                         (send this on-paint))
            ((left-up) (set! IND_CLIC #f) ; Création du polygone
                       (creer-poly)
                       (send this on-paint))
            (else (when IND_CLIC ; Si le clic gauche est enfoncé
                    (set! COMPT (modulo (add1 COMPT) 2)) ; Pour ne prendre qu'un point sur deux
                    (when (zero? COMPT)
                      (rajout (send this get-dc) evt)))))
          ; Le dessin a déjà été tracé
          (case (send evt get-event-type)
            ((left-down) (when
                             ;pour pouvoir saisir l'objet par le centre de masse, il faut que la boule enveloppante
                             ;soit à une distance minimale des bords de la fenêtre de 25px
                             ;et que le curseur soit à 15px du centre de masse
                             (and 
                              (<= (norme (-vect (make-vect (send evt get-x) (send evt get-y)) 
                                                (masse-r (polygone-cm POLYGONE)))) 15)
                              (<= (+ (polygone-rayon POLYGONE) (send evt get-x)) 475)
                              (<= (+ (polygone-rayon POLYGONE) (send evt get-y)) 475)
                              (>= (- (send evt get-x) (polygone-rayon POLYGONE)) 25)
                              (>= (- (send evt get-y) (polygone-rayon POLYGONE)) 25))
                           ;si tel est le cas, on change de curseur
                           (send this set-cursor (make-object cursor% 'hand))
                           ;on active le témoin de clic
                           (set! IND_CLIC #t)
                           ;on stoppe l'évolution
                           (set! IND_ACTION #f)
                           (send BUTTON_DRAW_ACTION set-label "Stop")
                           (send this on-paint)))
            ((left-up) 
             ;quand on relache le clic
             (when IND_CLIC
               ;on désactive le témoin de clic
               (set! IND_CLIC #f)
               ;on met la figure en mouvement
               (set! IND_ACTION #t)
               ;on lui communique en vitesse de translation la dernière vitesse transmise à main levée
               ;minorée si besoin (pour éviter des vitesses trop élevées) 
               (set-polygone-vtrans! POLYGONE 
                                     (let* ((evol (*vect 0.1 evolution)) (n (norme evol)))
                                       (if (<= n 2)
                                           evol
                                           (*vect 2 (*vect (/ 1 n) evol)))))
               (set! evolution (make-vect 0 0))
               (send this set-cursor (make-object cursor% 'cross))
               (send this on-paint)))
            (else 
             ;lorsque l'on survole le canvas
             (if 
              ;si le clic est enfoncé
              IND_CLIC
              (if
               ;et si la boule enveloppante est dans la fenêtre
               (and
                (<= (+ (polygone-rayon POLYGONE) (send evt get-x)) 500)
                (<= (+ (polygone-rayon POLYGONE) (send evt get-y)) 500)
                (>= (- (send evt get-x) (polygone-rayon POLYGONE)) 0)
                (>= (- (send evt get-y) (polygone-rayon POLYGONE)) 0))
               ;on actualise le vecteur déplacement evolution
               (begin (set! evolution (-vect (make-vect (send evt get-x) (send evt get-y)) 
                                             (masse-r (polygone-cm POLYGONE))))
                      ;on translate tous les points du polygone
                      (for i from 0 to (- (polygone-size POLYGONE) 1)
                           (set-masse-r! (vector-ref (polygone-poly POLYGONE) i)
                                         (+vect (masse-r (vector-ref (polygone-poly POLYGONE) i)) evolution)))
                      ;ainsi que son centre de masse
                      (set-masse-r! (polygone-cm POLYGONE)
                                    (+vect (masse-r (polygone-cm POLYGONE)) evolution))
                      ;et le curseur reste une main
                      (send this set-cursor (make-object cursor% 'hand))
                      (send this on-paint))
               ;sinon, le curseur redevient une croix
               (send this set-cursor (make-object cursor% 'cross)))
              ;si le clic n'est pas enfoncé
              (if 
               ;on affiche la main si la boule enveloppante
               ;est à une distance minimale des bords de la fenêtre de 25px
               ;et que le curseur est à 15px du centre de masse
               (and
                (<= (norme (-vect (make-vect (send evt get-x) (send evt get-y))
                                  (masse-r (polygone-cm POLYGONE)))) 15)
                (<= (+ (polygone-rayon POLYGONE) (send evt get-x)) 475)
                (<= (+ (polygone-rayon POLYGONE) (send evt get-y)) 475)
                (>= (- (send evt get-x) (polygone-rayon POLYGONE)) 25)
                (>= (- (send evt get-y) (polygone-rayon POLYGONE)) 25))
               (send this set-cursor (make-object cursor% 'hand))
               (send this set-cursor (make-object cursor% 'cross))))))))
    (super-new)))

;;;; Canvas pour le dessin à main levée
(define CANVAS_DRAW (new canvas-draw% 
                         (parent VPANEL2)
                         (min-width 500)
                         (min-height 500)
                         (style '(deleted))                       
                         (paint-callback
                          (lambda (c dc)
                            (do () ((not IND_ACTION) )
                              (action POLYGONE)
                              (affiche POLYGONE BITMAP-DC 'ligne_polygonale)
                              (send dc draw-bitmap BITMAP 0 0 'solid)
                              (next POLYGONE E)
                              (sleep/yield (/ 1 100.)))
                            (affiche POLYGONE BITMAP-DC 'ligne_polygonale)
                            (send dc draw-bitmap BITMAP 0 0 'solid)))
                         (stretchable-width #f)
                         (stretchable-height #f)))

; Affichage d'un curseur en forme de croix au-dessus de la fenêtre du canvas du dessin
(send CANVAS_DRAW set-cursor (make-object cursor% 'cross))

;;;; Canvas pour les exemples
(define CANVAS_EXS (new canvas% 
                        (parent VPANEL2)
                        (min-width 500)
                        (min-height 500)
                        (style '(deleted))
                        (paint-callback
                         (lambda (c dc)
                           (do () ((not IND_ACTION) )
                             (action POLYGONE_EXS)
                             (if (equal? "Parametrisation" (send LBOX_EXS get-string-selection))
                                 (affiche POLYGONE_EXS BITMAP-DC 'ligne-polygonale)
                                 (affiche POLYGONE_EXS BITMAP-DC 'exemples))
                             (send dc draw-bitmap BITMAP 0 0 'solid)
                             (sleep/yield (/ 1 100.)))
                           (if (equal? "Parametrisation" (send LBOX_EXS get-string-selection))
                               (affiche POLYGONE_EXS BITMAP-DC 'ligne-polygonale)
                               (affiche POLYGONE_EXS BITMAP-DC 'exemples))
                           (send dc draw-bitmap BITMAP 0 0 'solid))) 
                        (stretchable-width #f)
                        (stretchable-height #f)))

;;;; Canvas pour la partie objet
(define CANVAS_OBJ (new canvas% 
                        (parent VPANEL2)
                        (min-width 500)
                        (min-height 500)
                        (style '(deleted))
                        (paint-callback
                         (lambda (c dc)
                           (do () ((not IND_ACTION) )
                             (send BITMAP-DC clear)
                             (action-obj BITMAP-DC)
                             (affiche-obj BITMAP-DC)
                             (send dc draw-bitmap BITMAP 0 0 'solid)
                             (sleep/yield (/ 1 24.)))
                           (send BITMAP-DC clear)
                           (affiche-obj BITMAP-DC)
                           (send dc draw-bitmap BITMAP 0 0 'solid)))
                        (stretchable-width #f)
                        (stretchable-height #f)))

; Centrage de la fenêtre seulement horizontalement (du fait de sa hauteur)
(send FRAME1 center 'horizontal)
; Affichage de l'icône en haut à gauche de la fenêtre, dans la barre des tâches (non valable sur Mac OS X) et dans la barre ouverte avec Atl-Tab (seulement sur Windows)
(send FRAME1 set-icon 
      (make-object bitmap% (build-path (current-directory) "Images" "Icones" "phi.jpg") 'jpeg #f))
; Affichage de la fenêtre
(send FRAME1 show #t)





