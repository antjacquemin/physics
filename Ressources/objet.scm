
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "objet.scm"
;      ce fichier gère les exemples de la section
;            orientée objet du projet

;*****************************************************

;*****************************************************

;                 FICHIERS A CHARGER

;*****************************************************

(load (build-path (current-directory) "Ressources" "adt-classes-objet.scm"))

;*****************************************************

;                     CONSTANTES

;*****************************************************

;on désignera l'intensité de la gravité (dirigée vers le bas)
;par le réel GRAVITE
(define GRAVITE 0)

;le monde sera une liste d'objets
(define WORLD '())

;*****************************************************

;               INTERFACE GRAPHIQUE

;*****************************************************

;initialisation du monde
(define (init-objet)
  (set! WORLD '())
  (set! GRAVITE 0))

;centre-de-gravite : liste -> masse%
;renvoie la masse qui est le centre de masse du monde
(define (centre-de-gravite monde)
  (let ((r (apply +vect (map (lambda (x) (*vect (send x m) (send x r))) WORLD))) (M (apply + (map (lambda (x) (send x m)) monde))))
    (new masse% (position (*vect (/ 1 M) r)) (couleur "red") (ray 2))))

;affiche-objet-centre : dc -> void
;ordonne au dc d'afficher le centre de masse
(define (affiche-objet-centre dc)
  (send (centre-de-gravite WORLD) play dc))

;action-objet : dc -> void
;fait evoluer le monde
(define (action-obj dc) 
  ;traitement des chocs pour chaques éléments
  (for-each (lambda (x) (when (not (send x marqueur)) (send x chocs WORLD))) WORLD)
  ;evolution de chacun des elements vers le prochain état
  (for-each (lambda (x) 
              (send x change 500 500 GRAVITE)
              (send x set-marqueur! #f)) WORLD))

;affiche-objet : dc -> void
;affiche le monde
(define (affiche-obj dc)
  ;pour chaque éléments du monde
  (map (lambda (x) (send x play dc)) WORLD)
  ;s'il le faut, afficher le centre de masse
  (when AFFICHE_OBJ_CM (affiche-objet-centre dc)))

