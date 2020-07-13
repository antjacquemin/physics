

;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;                    "slideshow.scm"
;   Executez ce fichier sous le language "module..."
;              pour lancer le slideshow 

;*****************************************************



(module slideshow (lib "slideshow.ss" "slideshow")
  
  ;Chargement des librairies nécessaires au slideshow
  
  (require (prefix gl- (lib "sgl.ss" "sgl"))
           (lib "sendurl.ss" "net")
           (lib "cache-image-snip.ss" "mrlib")
           (lib "code.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss" "mzlib"))
  
  ;Chapitrage du slideshow
  
  (define chapitrage 
    (let ([sub-para (lambda l
                      (apply para (* 3/4 client-w) l))])
      (make-outline
       'preambule "Préambule" 
       (lambda (tag)
         (sub-para "Pour une bonne utilisation du présent slideshow..."))
       
       'introduction "Introduction" 
       #f
       
       'one "Partie I : Physique du point" 
       (lambda (tag)
         (sub-para "Quantité de mouvement, théorème fondamental, lois de Newton"))
       
       'two "Partie II : Quantité de mouvement et chocs" 
       (lambda (tag)
         (sub-para "Applications aux chocs entre des masses circulaires"))
       
       'three "Partie III : Mouvements de rotation"
       (lambda (tag)
         (sub-para "Applications aux chocs entre des polygones"))
       
       'four "Partie IV : Implémentation du projet"
       (lambda (tag)
         (sub-para "Structures, détection de colision, enveloppe convexe, traitement des chocs"))
       
       'conclusion "Conclusion" 
       #f)))
  
  ;souligner : string -> img
  ;renvoie l'image du texte passé en argument souligné
  (define (souligner cetexte)
    (let ((p (t cetexte)))
      (refocus
       (vc-append p (hline (pict-width p) 1))
       p)))
  
  ;lien-url : string x string -> img
  ;renvoie l'image de la premiere chaine de caractere pointant vers l'adresse internet de la seconde
  (define (lien-url mot adresse)
    (clickback (colorize (souligner mot) "Purple")
               (lambda ()
                 (send-url adresse))))
  
  ;lien-url : string x procedure -> img
  ;renvoie l'image de la chaine de caractere soumise à la clickback dont la lambda est la procédure passée en paramètres
  (define (lien-scheme mot f)
    (clickback (colorize (souligner mot) "blue")
               f))
  
  ;exemple : string x string-> void
  ;charge le fichier entre en parametre apres un clic sur un lien-scheme
  (define (exemple mot f)
    (lien-scheme mot (lambda () (load (build-path (current-directory) "slideshow" "exemples" f)))))
  
  ;image : string -> img
  ;renvoie une image gif a taille adaptée au slideshow
  (define (image fichier)
    (scale (bitmap (build-path (current-directory) "slideshow" "images_slideshow" fichier)) 0.64))
  
  ;equation : int -> img
  ;renvoie une image gif d'equation dont la taille est adaptée au slideshow
  (define (equation nb-eq)
    (scale (bitmap (build-path (current-directory) "slideshow" "equations" (string-append "eq" (number->string nb-eq) ".gif"))) 0.64))
  
  ;image-commentee : string x string -> img
  ;renvoie une image gif a taille adaptée au slideshow, et commentée
  (define (image-commentee fichier commentaires)
    (page-para (page-para/c (image fichier))
               (page-para/c (text commentaires main-font 18))))
  
  ;Slide 1 - Presentation
  (slide/center
   (frame (image "physics.gif"))
   (blank) (blank)
   (page-para/c "Projet réalisé par")
   (page-para/c (tt "CORLAIS Florian") ", " (tt "JACQUEMIN Anthony")))
  
  ;Préambule
  (chapitrage 'preambule)  
  
  (slide/title/center
   "Préambule"
   (page-para "Ce document existe en versions Slideshow et pdf. Vous trouverez, lors de son visionnage, ou de sa lecture :")
   'next
   (blank)
   (page-item "Des liens comme"
              (lien-scheme "celui-ci"
                           (lambda ()
                             (message-box "Exemple" 
                                          (string-append 
                                           "Ce type de lien n'est intéractif que dans la version Scheme du slideshow. "
                                           "Il est caractérisé par un texte bleuté, et souligné."))))
              ", bleutés et soulignés, qui permettent, par une lecture via Slideshow, de profiter de l'intéractivité permise par Scheme.")
   'next
   (blank)
   (page-item "Des liens"
              (lien-url "url" "http://fr.wikipedia.org/wiki/Physique")
              ", mauves et soulignés, qui permettent de se référer, dans les deux types de configurations, à des documents disponibles en ligne."))
  
  (slide/title/center
   "Préambule"
   (page-para "Ce document est réalisé dans le cadre de l'option "
              (it "Programmation") (it "fonctionnelle") (it "II")
              " de la deuxième année de licence Mathématiques de l'université de Nice Sophia-Antipolis.")
   (blank)
   (page-para "Pour en savoir plus, vous pouvez visiter le site de M. Roy, enseignant responsable du cours, à l'adresse"
              (lien-url "http://deptinfo.unice.fr/~roy/" "http://deptinfo.unice.fr/~roy/")
              ". Pour toutes remarques, suggestions, ou pour nous signaler d'éventuelles erreurs, vous pouvez nous contacter par mail : ")
   (blank)
   (page-item "Anthony :" (souligner "micheljacquemin@hotmail.fr"))
   (blank)
   (page-item "Florian :" (souligner "amlc36@numericable.fr")))
  
  (slide/title/center
   "Présentation"
   (page-para "Notre projet s'est attaché à rendre physiquement cohérentes des simulations de mécanique," 
              "et nous avons ainsi pu aborder différents problèmes : ")
   'next
   (page-item "La résolution de chocs entre particules élémentaires")
   'next
   (page-subitem "Chocs axés (intervention de la conservation de la quantité de mouvement et de l'énergie cinétique)")
   (page-subitem "Chocs désaxés (usage de la loi de composition des vitesses)")
   'next
   (page-item "La résolution de chocs entre des objets quelconques")
   'next
   (page-subitem "Décomposition du mouvement (mouvements de translation et de rotation)")
   (page-subitem "Traitement des chocs entre deux corps (tenant compte de la réaction normale, et de la friction lors de l'impact)"))
  
  (slide/title/center
   "Présentation"
   (page-para "Le traitement physique du problème passé, il a fallu implémenter les méthodes de résolution des différentes solutions."
              "Pour ce faire, nous avons dû :")
   'next
   (blank)
   (page-item "Choisir des structures de données adéquates")
   (page-item "Résoudre le problème de calcul de l'enveloppe convexe d'un polygone"
              "(en O(n log(n)) par l'algorithme de Graham")
   (page-item "Gérer la détection de colisions entre deux polygones")
   (page-item "Trouver des exemples d'application pour ce slideshow"))
  
  (slide/title/center
   "Présentation"
   (page-para "Par ce document, nous tenterons de justifier au mieux nos choix, et de rendre compte, de la manière la plus exhaustive"
              "possible, les différentes connaissances mises en jeu dans notre projet...")
   (blank)
   (clickback (image "helloworld.gif")
              (lambda ()
                (send-url "http://www.screencast.com/t/kxlW3IjD"))))
  
  ;Introduction
  (chapitrage 'introduction)
  
  (slide/title/center
   "Introduction"
   (page-para/c (it "\"Notre Monde est en mouvement.\""))
   (blank)
   'next
   (page-item "Cette expression courante qui," (it "a priori,") "ne se réfère pas à la physique, dénote néanmoins"
              "un fait remarquable : le" (it "mouvement") "est partout.")
   'next
   (page-item "Pour cette raison, la mécanique, a connu un engouement prononcé, et les outils nécessaires à son étude sont apparus"
              "très tôt. Pour s'en convaincre, il suffit d'isoler les deux notions utiles à la description du mouvement :")
   'next
   (page-subitem "Les distances")
   (page-subitem "Le temps")
   (blank))   
  
  (slide/title
   "La mesure des longueurs"
   (page-item "De ces deux grandeurs fondamentales de physique, la première a été la plus facile à estimer :"
              "une fois l'unité de longueur définie, la mesure directe est possible, et facile à réaliser (du moins, "
              "pour de courtes distances).")
   (blank)
   'next
   'alts
   (list (list (page-subitem "Ainsi, dans l'Egypte antique, la coudée (65 cm) était l'unité en vigueur pour les travaux"
                             "architecturaux et artisanaux, et des barres permettaient d'effectuer les mesures.")
               (blank)
               (image-commentee "coudee.gif" "Barre mesurant la coudée (Egypte)"))
         (list (page-subitem "Les mesures sont facilitées par des références à la vie quotidienne : dans la Grèce antique,"
                             " les habitants parlaient en doigt (2cm), demi pied (32cm), coudée (16cm), brasse (1.86m) ou stade (185m).")
               (blank)
               (image-commentee "panathinaiko.gif" "Stade de Panathinaïko (Grèce)"))
         (list (page-subitem "Aujourd'hui, les avancés technologiques permettent des mesures précises, de l'infiniment grand à l'infiniment petit"
                             "(géolocalisation par satellites, microscope à balayage électronique, laser).")
               (blank)
               (image-commentee "metre.gif" "Prototype du mètre de 1889"))))
  
  
  (slide/title
   "La mesure du temps"
   (page-item "L'Homme n'est pas directement confronté au temps, il n'en ressent que ses effets. Sa quantification"
              "n'est donc pas aussi immédiate que la mesure des longueurs, mais de nombreuses solutions ont été apportées"
              "au cours de l'Histoire.")
   (blank)
   'next
   'alts
   (list (list (page-subitem "En Egypte, à l'instar du sablier, l'horloge à eau, permettait d'évaluer" (it "l'écoulement")
                             "du temps, et de mesurer la durée de certains phénomènes, à l'aide de graduations gravées sur"
                             "le récipient.")
               (blank)
               (image-commentee "clepsydre.gif" "Clepsydre - Karnak (Egypte)"))
         (list (page-subitem "A partir du neuvième siècle, la bougie est utilisée pour décompter le temps (et sera largement employée plus tard,"
                             "au théâtre).")
               (blank)
               (image-commentee "bougie.gif" "Horloge a bougie"))
         (list (page-subitem "De nos jours, la mesure du temps peut se faire à la nanoseconde près, grâce à des moyens toujours plus précis :"
                             "les chronomètres, les horloges à quartz, les horloges atomiques...")
               (blank)
               (image-commentee "nist.gif" "Une horloge atomique du NIST (USA)"))))
  
  (slide/title
   "Introduction"
   (page-item "Ces outils, indispensables à la description de phénomènes physiques, ont permis, très tôt, d'étudier des mouvements : qu'ils soient simples,"
              "comme les oscillations périodiques d'un pendule, ou plus complexes.")
   'next
   (page-item "L'essort de l'informatique au cours du XX° siècle a radicalement changé l'approche des problèmes de mécanique : la résolution de système d'équations"
              "par les machines, et leur rapidité d'exécution, ont rendu possible des modélisations jusqu'à lors inabordables. Pour simple exemple, prenons les dernières"
              "images, quasi-photoréalistes, d'une mer déchainée, extraites d'un jeu vidéo de la dernière génération de console de salon.")
   (blank)
   (image-commentee "eaucallofduty.gif" "Call of Duty 4 (Playstation 3)"))
  
  ;Partie 1 - Physique du point
  (chapitrage 'one)
  
  (slide/title
   "Trajet linéaire"
   (page-item "Dans cette première partie, nous nous intéressons aux particules,"
              "caractérisées par leur masse, concentrée en un point.")
   
   'next
   (page-item "Lorsqu'un point effectue un trajet direct (en ligne droite) d'un point A vers un point B, il est facile de mesurer la distance"
              "qu'il parcourt. En choisissant une origine O sur (AB), nous avons la relation : ")
   (blank)
   (frame (equation 1))
   (blank)
   (page-item "Cette définition rend compte de l'orientation de la configuration du trajet, elle est relative au choix de l'origine, et peut ainsi être négative.")
   'next
   (tt "Quid des trajets quelconques ?"))
  
  (slide/title
   "Trajet courbe"
   (page-item "Comme souvent, pour décrire un phénomène complexe, les physiciens se ramènent à un cas simple, parfaitement connu.")
   (page-item "Lorsqu'une particule effectue un trajet quelconque de A vers B durant un temps T, nous pouvons décomposer son trajet"
              "en N trajets élémentaires, linéaires, et appliquer la définition précédente.")
   'next
   (page-item "Vite, un exemple !")
   'alts
   (list (list 
          (blank) (blank)
          (image-commentee "distance1.gif" "Cas du trajet linéaire"))
         (list (image "distance2.gif"))))
  
  
  (slide/title
   "Description du mouvement ~ x=f(t)"
   (page-item "Nous savons désormais mesurer la distance qu'effectue une particule lors d'un trajet, "
              "tout en connaissant la durée qu'elle met pour l'effectuer.")
   'next
   (page-item "Nous pouvons donc décrire le mouvement par une représentation graphique : dans le cas simple"
              "à une dimension (le mobile se déplaçant sur un axe (Ox)), il suffit de tracer la courbe x=f(t).")
   'alts
   (list (list (image-commentee "xt1.gif" "Cas d'un trajet rectiligne uniforme"))
         (list (image-commentee "xt2.gif" "Cas d'un trajet rectiligne quelconque"))))
  
  (slide/title
   "Description du mouvement ~ x=f(t)"
   (scale (image "xt2.gif") 0.75)
   (page-item "Vous pouvez" (exemple "ici" "xft.scm") "vous familiariser avec ce type de graphique en contrôlant une masse"
              "se déplaçant sur une droite (AB) : des clics sur les bouton + (ou -) augmentent (ou diminuent) sa vitesse."
              "Si la masse percute le point A, elle rebondit sans perdre d'énergie pour se diriger vers B.")
   (page-item "On pourra trouver une vidéo de démonstration à l'adresse : " 
              (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/xft.html" "http://projetphysics.teria.org/Videos_du_slideshow/xft.html")))
  
  (slide/title
   "Vitesse d'un point"
   (page-item "La notion de vitesse nous est familière : il s'agit d'un rapport entre une distance et un temps. En prenant l'exemple d'une voiture partant du point"
              "A au temps tA, pour arriver au temps tB en B, nous avons coutume d'appeler vitesse moyenne :")
   (frame (equation 2))
   'next
   (page-item "Quelques remarques :")
   (page-subitem "Une simple analyse dimensionnelle révèle que la vitesse n'est pas un rapport homogène : [V]=[L]/[T]. Ce fait a été, durant longtemps,"
                 "une difficulté théorique pour décrire les mouvements (le même problème s'est posé en définissant des longueurs algébriques).")
   'next
   (page-subitem "Comment interpréter sur notre précédent graphique ce terme de vitesse ?"))
  
  (slide/title
   "Vitesse lors d'un mouvement rectiligne uniforme"
   (page-item "Reprenons notre graphique x=f(t) dans le cas d'un mouvement rectiligne uniforme : ")
   'alts
   (list (list (image "xt1.gif")
               (blank)
               (page-para/c "Mouvement rectiligne uniforme"))
         (list (image "xt3.gif")
               (blank)
               (page-para/c "Soit C un point sur le trajet AB : d'après le théorème de Thales..."))
         (list (image "xt4.gif")
               (page-para/c (hc-append (t "L'égalité des rapports ") (equation 3) (t " nous permet de déduire :"))))))
  
  (slide/title/center
   "Vitesse lors d'un mouvement rectiligne uniforme"
   (frame (page-para/c "Au cours d'un trajet rectiligne uniforme, la vitesse d'une masse ponctuelle est constante."))
   'next
   (blank) (blank)
   (frame (page-para/c "Une masse, dont la représentation graphique x=f(t) est une droite, est en mouvement rectiligne uniforme, et réciproquement.")))
  
  
  (slide/title
   "Interprétation graphique du terme de vitesse"
   (page-item "A l'instar de la représentation précédente x=f(t), nous pouvons tracer la courbe v=f(t).")
   'next
   (page-item "Nous l'avons vu, la vitesse d'un point sur le trajet AB en mouvement rectiligne uniforme est constante :")
   (image "vt1.gif")
   'next
   (page-item "De notre définition de la vitesse, on en déduit : AB = v*(tB-tA). Ainsi, graphiquement, l'aire colorée est la longueur AB."))
  
  (slide/title/center
   "Vitesse instantanée"
   (blank)
   (page-item "Nous avons caractérisé les mouvements de translation, uniformes, et n'avons abordé que la notion"
              "de vitesse moyenne. Introduisons celle de vitesse instantanée.")
   'next
   (page-item "Beaucoup plus récente (1690), elle est définie comme suit :")
   (frame (equation 4))
   'next
   (page-item "Graphiquement, elle s'interprète comme la pente de la tangente au point (t,x(t)) de la représentation graphique x=f(t).")
   (image "xt5.gif"))
  
  (slide/title
   "Interprétation graphique du terme de vitesse"
   (page-item "Comme précédemment, cherchons à interpréter graphiquement le terme de vitesse à partir de la courbe v=f(t).")
   'next
   (frame (equation 5))
   'next
   (page-item "Ainsi, l'aire (algébrique) colorée ci-dessous correspond à la distance (algébrique) du trajet de A à B parcouru par la masse.")
   (image "vt2.gif"))
  
  (slide/title
   "Loi de composition des vitesses"
   (page-para/c "Les vitesses ont une propriété remarquable :")
   (frame (equation 6))
   'next
   (page-para/c "Démonstration : ")
   (frame (equation 7))
   'next
   (tt "Vite, un exemple !"))
  
  (slide/title/center
   "Loi de composition des vitesses"
   (image "escalator.gif")
   (blank)(blank)
   (page-item "Pour ne pas rater le prochain métro, vous décidez d'emprunter un tapis roulant, ayant une vitesse de 9km/h (vitesse du tapis de la station" 
              (it "Montparnasse")
              "), en marchant d'un pas décidé à 5km/h.")
   (page-item "Votre vitesse est, par rapport au référentiel terrestre, de 14km/h.")
   (page-item "Ce théorème de composition formalise donc notre intuition naturelle concernant l'" (it "addition") "des effets des vitesses, lorsqu'elles"
              "restent raisonnablement faibles (pour ne pas tomber dans le cas relativiste)."))
  
  (slide/title
   "Quantité de mouvement"
   (page-para "Pour poursuivre notre étude, nous devons introduire une nouvelle grandeur vectorielle, la quantité de mouvement :")
   'next
   (page-item "Le vecteur quantité de mouvement d'une masse ponctuelle m, se déplaçant à une vitesse v, est le vecteur :")
   (frame (equation 8))
   'next
   (page-item "Cette définition se généralise à un ensemble S de N masses en sommant les N vecteurs quantité de mouvement associés à"
              "chacun des points du système :")
   (frame (equation 9)))
  
  (slide/title/center
   "Théorème fondamental"
   (page-para/c "Utilisons ce nouveau vecteur pour énoncer le théorème fondamental de la mécanique : le mouvement est conservatif.")
   'next
   (blank)(blank)
   (equation 10))
  
  (slide/title
   "Première loi de Newton"
   (page-item "Newton dans ses" (souligner "Principia") "de 1687, n'évoque pas ouvertement les travaux de Descartes, à l'origine du précédent théorème,"
              "mais il va introduire les notions de force, et de système isolé, indispensables à l'étude de la mécanique.")
   'next
   (page-item (hc-append (t "Un système S est dit isolé si : ") (equation 11)))
   'next
   (page-item "Première loi de Newton : ")
   (equation 12)
   'next
   (page-item "L'application de la définition d'un système isolé conduit au résultat :")
   (frame (equation 13)))
  
  (slide/title/center
   "Deuxième loi de Newton"
   (page-item "Par définition, une force F caractérise une variation de mouvement.")
   'next
   (page-subitem "Il s'agit donc d'une grandeur vectorielle")
   'next
   (page-subitem "Elle est la dérivée temporelle de la quantité de mouvement")
   'next
   (page-item "En supposant la masse m de la particule M constante, on définit la force s'exerçant sur M par : ")
   (frame (equation 14))
   'next
   (page-item "Deuxième loi de Newton : ")
   (equation 15))
  
  (slide/title/center
   "Troisième loi de Newton"
   (page-item "Enoncé de la troisième loi de Newton :")
   (frame (vc-append (t "Si A et B sont deux points en interaction, et forment")
                     (t "un système isolé, alors la force qu'exerce B sur A")
                     (t "est l'opposée de la force exercée sur B par A")))
   'next
   (page-item "La démonstration se base encore sur la définition d'un système isolé : ")
   (frame (equation 16)))
  
  (slide/title/center
   "Chute libre"
   (page-para "Appliquons la seconde loi de Newton à une masse en chute libre pour décrire son mouvement.")
   'next
   (page-item "Un corps est dit en chute libre s'il n'est soumis qu'à son poids," (it "i.e.") "la seule force extérieure s'exerçant sur lui est la force :")
   (frame (equation 17))
   'next
   (page-item "Le poids est une force verticale, dirigée vers le bas, proportionnelle à la masse, résultant de l'action de la Terre sur l'objet. Le coefficient de"
              "proportionnalité g vaut environ 10 N/kg.")
   'next
   (page-item "Soit une masse M en chute libre dans un repère (Oxy) ayant une vitesse initiale v(t0) au temps t0."))
  
  (slide/title/center
   "Chute libre"
   (page-item "Comme M est en chute libre, d'après la seconde loi de Newton : ")
   (frame (equation 18))
   'next
   (page-item "Ainsi, en envisageant des intervalles de temps suffisamment petits pour approximer le mouvement de M, durant ces intervalles, en un mouvement rectiligne uniforme"
              ", sa vitesse varie suivant la relation :")
   (frame (equation 19))
   'next
   (page-item "En exploitant finalement le fait que, pour un mouvement rectiligne uniforme, nous avons la relation : ")
   (frame (equation 20))
   (blank)
   (tt "On aboutit à la construction suivante"))
  
  (slide/title/center
   "Chute libre"
   'alts
   (list (list (image "parabole0.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole1.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole2.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole3.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole4.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole5.gif")
               (t "Mouvement d'une masse M en chute libre"))
         (list (image "parabole6.gif")
               (t "La masse semble suivre une trajectoire parabolique.")
               (tt "Peut-on trouver ce résultat par le calcul ?"))))
  
  (slide/title
   "Chute libre"
   (page-para/c "La seconde loi de Newton nous donne :")
   (frame (equation 18))
   'next
   (page-para/c "On en déduit, par intégration, la vitesse de la masse :")
   (frame (equation 21))
   'next
   (page-para/c "Puis sa position en fonction du temps :")
   (frame (equation 22))
   'next
   (tt "On retrouve bien l'équation d'une parabole."))
  
  (slide/title/center
   "Chute libre"
   (page-item "Une petite" (exemple "visualisation" "parabole.scm") "devrait finir par vous convaincre !"
              "Vous pouvez modifier la masse de la boule en cliquant sur \"Change B\" (B comme bleu), modifier la composante horizontale"
              "de sa vitesse de manière aléatoire via \"Change VB\", ou l'annuler par \"VB=0\". Nous pouvons vérifier deux choses :")
   'next
   (page-subitem "La variation de la composante verticale de la vitesse est indépendante de la masse de l'objet")
   (page-subitem "La masse suit bien une trajectoire parabolique")
   'next
   (page-item "Une vidéo reprenant ces points est visionnable à cette adresse : " 
              (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/parabole.html" "http://projetphysics.teria.org/Videos_du_slideshow/parabole.html")))
  
  ;Partie 2 - Quantité de mouvement et chocs
  (chapitrage 'two)
  
  (slide/title
   "Travail d'une force"
   (page-para "Nous allons, pour traiter le problème des chocs entre particules, préalablement introduire la notion de travail d'une force, et d'énergie.")
   'next
   (page-item "De manière qualitative, le travail est" (it "l'effort") "à fournir pour mettre en mouvement un objet : "
              "il quantifie la dépense énergétique associée à un tel déplacement.")
   'next
   (page-item "Si M est un point matériel et dl la distance infinitésimale parcourue par M au cours d'un intervalle de temps dt, le travail"
              "de la force F est par définition : ")
   (frame (equation 23))
   'next
   (page-item "Calcul du travail du poids :")
   (equation 24))
  
  (slide/title
   "Energie cinétique"
   (page-item "Qualitativement, l'énergie est la ressource nécessaire au travail d'une force.")
   'next
   (page-subitem "Il existe une énergie potentiellement délivrable par un système physique (ressort, altitude, potentiel chimique)")
   (page-subitem "Mais une énergie, liée au mouvement, est plus universelle : il s'agit de l'énergie cinétique")
   'next
   (page-item "L'expression de l'énergie cinétique d'une masse M est :")
   (frame (equation 25))
   'next
   (page-item "Le dernier théorème dont nous aurons besoin dans cette partie est le théorème de l'énergie cinétique :")
   (frame (page-para/c "La variation d'énergie cinétique d'un point matériel M est égale au travail total de toutes les forces s'exerçant sur M"
                       "au cours du déplacement.")))
  
  (slide/title/center
   "Théorème de l'énergie cinétique"
   (page-para/c "Calcul pour un degré de liberté")
   (frame (equation 26))
   'next
   (page-para/c "Calcul pour deux degrés de liberté")
   (frame (equation 27)))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-item "Considérons le problème suivant : ")
   'next
   (page-subitem "Soit S un système isolé constitué de deux masses M1 et M2")
   (page-subitem "Soit v1 la vitesse de M1, M2 étant immobile")
   (page-subitem "On suppose v1 dirigé selon la direction des centres de masse")
   (page-subitem "M1 et M2 entrent en collision au cours d'un choc parfaitement élastique (il y a conservation de l'énergie mécanique du système)")
   (page-subitem "Soient v1' et v2' les vitesses des masses après le choc"))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-item "Il s'agit donc de comprendre" (exemple "ce phénomène." "frontal.scm")
              "Les masses B et V (bleu et vert) peuvent être modifiées ainsi que leur vitesse.")
   'next
   (page-item "Nous pouvons déjà remarquer que si les deux objets ont la même masse, et que l'un des deux est à l'arrêt, alors"
              "celui-ci, après le choc, se déplace à la même vitesse que l'autre masse, qui s'arrête net.")
   'next
   (page-item "Le tout est consultable sur le site internet du projet à l'adresse" 
              (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/axe.html" "http://projetphysics.teria.org/Videos_du_slideshow/axe.html")))
  
  (slide/title/center
   "Choc élastique frontal"
   (image "chocaxe0.gif")
   (blank)
   'next
   (tt "Quelles sont les vitesses des masses")
   (tt "après le choc ?"))
  
  (slide/title
   "Choc élastique frontal"
   (page-item "Bilan énergétique :")
   (equation 28)
   'next
   (page-item "Bilan de la quantité de mouvement :")
   (equation 29)
   'next
   (page-item "Applications des lois de conservation de la quantité de mouvement, et de l'énergie :")
   (frame (equation 30)))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-item "Résolution du système :")
   'next
   (blank)
   'alts
   (list (list (equation 310))
         (list (equation 311))
         (list (equation 312))
         (list (equation 313))
         (list (equation 314)
               (tt "Ouf !!!"))))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-para/c "Avant de tirer une quelconque conclusion, nous pouvons appliquer ce calcul à trois exemples :")
   (blank)
   (exemple "Le pendule de Newton" "pendule.scm")
   (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/pendule.html" "http://projetphysics.teria.org/Videos_du_slideshow/pendule.html")
   'next
   (blank)
   (exemple "La chute d'une balle sur un plan incliné" "plan.scm")
   (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/plan.html" "http://projetphysics.teria.org/Videos_du_slideshow/plan.html")
   'next
   (blank)
   (let ((VISIONNE? #t))
     (lien-scheme "Collisions entre deux boules dans une enceinte circulaire"
                  (lambda ()
                    (if VISIONNE?
                        (begin (set! VISIONNE? #f)
                               (load (build-path (current-directory) "slideshow" "exemples" "circulaire.scm")))
                        (message-box "Attention" 
                                     (string-append 
                                      "Le film ne peut être visionné qu'une seule fois au cours d'une session Slideshow."
                                      "Vous pouvez le visionner sur le lien URL ci-dessous."))))))
   (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/circulaire.html" "http://projetphysics.teria.org/Videos_du_slideshow/circulaire.html")
   (scale (t "(Ce dernier lien scheme est une animation qui prend fin automatiquement)") 0.5))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-item "Finalement, les vitesses après le choc sont données par : ")
   (frame (equation 32))
   'next
   (page-item "Quelques remarques :")
   'next
   (page-subitem "On retrouve l'effet constaté sur la simulation précédente :"
                 "si M1 et M2 ont la même masse, alors M1 après le choc est à l'arrêt, et M2 a la même vitesse que M1 avant le choc")
   'next
   (page-subitem "Si M2 est de masse infinie, par passage à la limite, nous avons v1' = -v1")
   'next
   (page-subitem "Ce modèle nous sert pour modéliser les chocs entre les masses et les parois de la fenêtre d'affichage :"
                 "la composante de la vitesse dirigée vers la paroi change de signe lors d'un tel choc")
   'next
   (tt "Et lorsque M2 est en mouvement ?"))
  
  (slide/title/center
   "Choc élastique frontal"
   (image "chocaxe.gif")
   'next
   (blank)
   (t "Le système devient :")
   (frame (equation 33)))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-para/c "Sa résolution se fait en divisant la première ligne avec la seconde après avoir mis en facteur"
                "m1 et m2 à gauche et à droite des égalités, puis par substitution. Sa solution est :")
   (blank)   (blank)
   'next
   (frame (equation 34))
   (blank)   (blank)
   'next
   (page-para/c "Le cas du choc frontal étant traité, il ne reste plus qu'à généraliser aux chocs désaxés."))
  
  (slide/title/center
   "Choc élastique quelconque"
   (page-item "Ici, c'est la loi de composition des vitesses qui va être mis en jeu.")
   'next
   (page-item "Partons de la configuration initiale suivante :")
   'alts
   (list (list (image "chocdesaxe0.gif"))
         (list (image "chocdesaxe.gif")
               (t "Décomposons les vitesses selon deux axes orthogonaux"))
         (list (image "chocdesaxe.gif")
               (t "Suivant (Ox), nous nous retrouvons dans le précédent cas d'un choc")
               (t "frontal, et suivant (Oy), il n'y a pas de choc..."))
         (list (image "chocdesaxe1.gif")
               (t "Tout a été préalablement traité !"))))
  
  (slide/title/center
   "Choc élastique frontal"
   (page-para/c "Nous pouvons donc reprendre la simulation précédente, dans le cas de deux boules n'entrant pas en collision"
                "axée.")
   'next
   (page-para/c "Si les deux objets ont la même masse, et que l'un est à l'arrêt, nous remarquons que les deux masses évoluent après le choc"
                "suivant des directions orthogonales : en fait, l'intégralité de la composante de vitesse parallèle au mouvement a été transmise"
                "à la masse au repos (cas d'un choc axé entre deux masses identiques dont l'une est à l'arrêt), et seule la composante"
                "orthogonale est restée invariante pour la masse en mouvement.")
   'next
   (exemple "Cliquez ici" "desaxe.scm")
   (t " ou ") 
   (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/desaxe.html" "http://projetphysics.teria.org/Videos_du_slideshow/desaxe.html"))
  
  ;Partie 3 - Mouvements de rotation
  (chapitrage 'three)
  
  (slide/title
   "Système à plusieurs masses"
   (page-para "Maintenant que nous savons comment traiter le problème des collisions entre des corps simples, nous allons tenter de comprendre"
              "comment des objets de formes plus générales réagissent lorsqu'ils entrent en contact.")
   'next
   (page-item "Rappelons la définition de la quantité de mouvement d'un système :")
   'next
   (page-subitem "Lorsque le système est réduit à une masse :")
   (equation 8)
   'next
   (page-subitem "Pour un système à n masses :")
   (equation 35)
   'next
   (page-item "On appelle centre de masse le barycentre des masses du système")
   (frame (equation 36)))
  
  (slide/title
   "Système à plusieurs masses"
   (page-para "L'intérêt d'étudier le centre de masse d'un système tient dans le théorème suivant :")
   (frame (vc-append (t "Le centre de masse d'un système isolé")
                     (t "a une accélération nulle")))
   'next
   (page-item "La définition d'un système isolé conduit au résultat escompté :")
   (frame (equation 37))
   'next
   (tt "Comment appliquer ce résultat ?"))
  
  (slide/title
   "Système de deux masses reliées par un ressort"
   (image "spiral.gif")
   'alts
   (list (list (page-item "On considère le système isolé S constitué de deux masses reliées par un ressort spiral de masse négligeable."))
         (list (page-item "Sans rien supposer des forces s'exerçant sur M1 et M2, on peut affirmer que le centre de masse du système a un mouvement"
                          "rectiligne uniforme. En effet :")
               (equation 38))))
  
  (slide/title
   "Modélisation des objets polygonaux"
   (page-para "Nous allons aborder le dernier point de notre étude : le choc entre deux objets de forme quelconque.")
   'next
   (blank)
   (page-item "De la même manière que nous l'avons fait pour mesurer des courbes, on supposera qu'il est possible d'approcher n'importe quelle forme par une"
              "suite de segments : tous les objets qui suivront pourront donc être modélisés par des polygones.")
   'next
   (blank)
   (page-item "Par exemple :")
   (image "approxpolygonale.gif"))
  
  (slide/title
   "Modélisation des objets polygonaux"
   (page-item "Nous supposerons les objets indéformables, ainsi tout mouvement pourra être décomposé comme :")
   'next
   (page-subitem "Un mouvement de translation : celui du centre de masse")
   (page-subitem "Un mouvement de rotation autour de ce centre de masse")
   'next
   (page-item (hc-append (equation 39) (t " est la vitesse angulaire, en rad/s")))
   'next
   (page-item "La vitesse de tout point délimitant la forme considérée aura donc les deux composantes suivantes :")
   (image "vitessespolygone.gif"))
  
  (slide/title/center
   "Chute libre de polygones"
   (page-para "Reprenons l'exemple de la chute libre d'un corps.")
   'next
   (page-item "Comme le centre de masse suit la trajectoire qu'aurait une masse ponctuelle où serait concentrée toute la masse du corps,"
              "le centre de masse de l'objet en chute libre suit une trajectoire parabolique.")
   'next
   (page-item "De plus, en supposant que l'objet est en rotation, il suffit, pour décrire sa trajectoire, qu'à chaque intervalle de temps élémentaire dt,"
              "l'objet subisse une rotation élémentaire autour du centre de masse.")
   'next
   (page-item "A partir du tracé de la parabole de la partie précédente, nous obtenons le graphique suivant..."))
  
  (slide/title/center
   "Chute libre d'une grenouille"
   (image "chutegrenouille.gif"))
  
  (slide/title
   "Choc entre deux polygones"
   (page-para "On considère le choc suivant entre deux objets A et B :")
   'alts
   (list (list (image "choc1.gif"))
         (list (image "choc2.gif")
               (t "Soit P le point de contact entre A et B"))
         (list (image "choc3.gif")
               (t "On définit un vecteur normal à la surface de contact,")
               (t "normé, pointant vers A"))
         (list (image "choc4.gif")
               (t "P appartient à A, soit vAP sa vitesse")
               (t "(composée de la vitesse de translation, et de rotation)"))
         (list (image "choc5.gif")
               (t "P appartenant également à B, on note vBP la vitesse")
               (t "de P considéré comme point de B"))
         (list (image "choc6.gif")
               (t "Dans le repère du système des deux objets,")
               (t "on note vAB la vitesse relative de P")
               (hc-append (t "On a alors : ") (equation 40)))
         (list (image "choc6.gif")
               (t "En omettant le mouvement de rotation des objets,")
               (t "nous nous retrouvons dans la situation de la partie précédente :")
               (t "seule la composante normale de la vitesse est affectée par le choc.")
               'next
               (tt "Mais..."))))
  
  (slide/title
   "Choc entre deux polygones"
   (page-item "Un choc peut être vu à plusieurs échelles :")
   'next
   (page-subitem "A l'échelle macroscopique : nous observons une impulsion affectant les vitesses")
   'next
   (page-subitem "A l'échelle atomique, des interactions entre les particules constituant les objets surviennent :"
                 "ces interactions, à l'origine d'une dissipation d'énergie, expliquent pourquoi, dans la réalité,"
                 "les chocs ne sont pas parfaitement élastiques (on observe une" (it "perte de mouvement") "au cours du temps)")
   'next
   (page-item "Pour modéliser cette perte d'énergie, introduisons le coefficient d'élasticité e :")
   (frame (equation 41))
   'next
   (page-item "e quantifie la vitesse restituée par le choc suivant la direction normale au choc."))
  
  (slide/title
   "Choc entre deux polygones"
   (page-item "Soit j l'impulsion dûe au choc ressentie par A, MA sa masse :")
   'next
   (equation 42)
   'next
   (page-item "D'après la troisième loi de Newton, -j est l'impulsion de B :")
   'next
   (equation 43)
   'next
   (page-item "Avec le coefficient d'élasticité, nous aboutissons au système :")
   'next
   (frame (equation 44)))
  
  (slide/title
   "Choc entre deux polygones"
   (page-item "La résolution est immédiate, et nous livre l'impulsion :")
   (frame (equation 45))
   'next
   (tt "Et en cas de rotation ?")
   (image "choc7.gif"))
  
  (slide/title
   "Moment d'inertie"
   (page-item "La masse d'un corps tend à s'opposer à la mise en mouvement (par translation) de celui-ci. En effet, par la seconde loi de Newton : ")
   (equation 46)
   'next
   (page-item "Dans le cas d'un système à n masses, il existe également un facteur s'opposant à la mise en rotation du système autour de son centre de masse :"
              "il s'agit du moment d'inertie.")
   'next
   (page-item "Dans notre étude, nous adopterons pour définition du moment d'inertie :")
   (equation 47))
  
  (slide/title
   "Choc entre deux polygones"
   'alts
   (list (list (image "choc8.gif")
               (t "Maintenant, nous avons :")
               (equation 48))
         (list (image "choc8.gif")
               (t "La modification de la vitesse angulaire est donnée")
               (t "par la loi des moments :")
               (equation 49))
         (list (image "choc8.gif")
               (t "En résolvant le système, nous trouvons :")
               (equation 50))
         (list (image "choc8.gif")
               (frame (vc-append (t "Pour conclure, connaissant le point d'impact, la normale au choc,")
                                 (t "et les caractéristiques du système, nous sommes capables")
                                 (t "de prévoir l'évolution du système, avant et après un choc."))))))
  
  ;Partie 4 - Implémentation du projet
  (chapitrage 'four)
  
  (slide/title
   "Implémentation du projet"
   (page-item "Notre projet a été développé, de manière régulière, durant les trois derniers mois, en parallèle des cours de l'option" (it "Programmation")
              (it "fonctionnelle II") 
              "(2 heures hebdomadaires).")
   'next
   (page-subitem "En premier lieu, dès la présentation de la librairie" (code (lib "world.ss" "htdp")) ", nous avons programmé des exemples simples"
                 "de collisions entre deux masses : certains d'entre eux se retrouvent dans ce slideshow.")
   'next
   (page-subitem "Une fois l'introduction à la programmation orientée objet faite, nous nous sommes attaqués au problème plus général de la collision"
                 "au sein d'un système composé de nombreuses masses.")
   'next
   (page-subitem "Finalement, durant les dernières semaines qui nous étaient accordées, nous avons implémenté le problème des chocs"
                 "entre polygones (en privilégiant l'utilisation de structures)."))
  
  (slide/title
   "Implémentation du projet"
   (page-item "L'objectif de cette partie n'est évidemment pas de détailler exhaustivement l'intégralité du code produit durant un trimestre,"
              "mais, en n'en prenant que quelques extraits, de décrire les algorithmes mis en oeuvre dans la résolution de la dernière partie :")
   'next
   (page-subitem "Le problème de l'enveloppe convexe")
   'next
   (page-subitem "Le traitement des chocs entre deux polygones")
   'next
   (page-subitem "La détection des chocs")
   'next
   (blank)(blank)
   (frame (code (when 
                    lecteur-is-ready
                  (play-the-end)))))
  
  (slide/title
   "Structure de polygone"
   (page-item "Dans cette partie, nous nous intéresserons uniquement aux chocs entre polygones. Nous définirons un polygone"
              "comme une structure de la forme :")
   'next
   (blank)
   (frame (scale (code (define-struct polygone 
                         ((code:comment "taille du polygone")
                          size
                          (code:comment "vecteur contenant les size masses formant le polygone")
                          poly   
                          (code:comment "centre de masse du polygone (masse calculée à la création)")
                          cm  
                          (code:comment "vitesse angulaire (type float)")
                          vang      
                          (code:comment "vitesse de translation (type vecteur)")
                          vtrans    
                          (code:comment "moment d'inertie")
                          i        
                          (code:comment "liste des indices de poly constituant l'enveloppe convexe")
                          ev        
                          (code:comment "rayon du plus petit cercle de centre le centre de masse du polygone contenant le polygone")
                          rayon))) 0.45))
   'next
   (blank)
   (page-item "Un vecteur sera une structure à deux champs x et y (on dispose des opérations vectorielles usuelles), et une masse correspondra à la"
              "donnée d'un vecteur position r, d'une masse m, et d'un angle alpha compris entre (Ox), et la droite passant par"
              "la masse et le centre de masse du polygone."))
  
  (slide/title/center
   "Evolution libre d'un polygone"
   (page-para "Si un polygone ne heurte pas une paroi de la fenêtre d'affichage, ou un autre polygone, alors sa position change entre t et t+dt de la manière suivante :")
   'alts
   (list (list (page-item "Translation de l'ensemble des points")
               (blank)
               (frame (scale (code 
                              (code:comment "La fonction a un effet de bords")
                              (define (translation-polygone poly)
                                (code:comment "Quand la vitesse de translation n'est pas nulle")
                                (when (not (and (zero? (vect-x (polygone-vtrans poly))) (zero? (vect-y (polygone-vtrans poly)))))
                                  (code:comment "On translate chacune des masses du polygone")
                                  (for i from 0 to (- (polygone-size poly) 1)
                                       (set-masse-r! (vector-ref (polygone-poly poly) i)
                                                     (+vect (masse-r (vector-ref (polygone-poly poly) i)) (polygone-vtrans poly))))
                                  (code:comment "Et l'on met à jour le centre de masse")
                                  (set-masse-r! (polygone-cm poly)
                                                (+vect (masse-r (polygone-cm poly)) (polygone-vtrans poly)))))) 0.5))
               'next
               (blank)
               (page-item "Complexité : O((polygone-size poly))"))
         (list (page-item "Rotation de l'ensemble des points")
               (blank)
               (frame (scale (code 
                              (code:comment "La fonction a un effet de bords")
                              (define (rotation-polygone poly)
                                (code:comment "Quand la vitesse angulaire est non nulle")
                                (when (not (zero? (polygone-vang poly)))
                                  (for i from 0 to (- (polygone-size poly) 1)
                                       (code:comment "On met à jour la position de chaque masse")
                                       (set-masse-r! (vector-ref (polygone-poly poly) i)
                                                     (code:comment "En faisant la rotation vectorielle de centre CM, d'angle vang")
                                                     (rotation (masse-r (polygone-cm poly))
                                                               (masse-r (vector-ref (polygone-poly poly) i))
                                                               (polygone-vang poly)))
                                       (code:comment "Reste à mettre à jour l'angle alpha formé entre la masse et le centre de masse")
                                       (set-masse-alpha! (vector-ref (polygone-poly poly) i)
                                                         (mod2pi (- (masse-alpha (vector-ref (polygone-poly poly) i))
                                                                    (polygone-vang poly)))))))) 0.5))
               'next
               (blank)
               (page-item "Complexité : O((polygone-size poly))"))
         (list (page-item "Effet de la gravité")
               (blank)
               (frame (scale (code 
                              (code:comment "La fonction a un effet de bords")
                              (define (gravite poly)
                                (code:comment "GRAVITE est une variable globale, de type vecteur")
                                (set-polygone-vtrans! poly (+vect (polygone-vtrans poly) GRAVITE)))) 0.7))
               (blank)
               'next
               (page-item "Complexité : O(1)"))))
  
  (slide/title/center
   "Evolution libre d'un polygone"
   (frame (vc-append (t "Ainsi, lorsque le polygone n'est en contact avec aucun objet,")
                     (t "il suffit, pour le faire passer à l'état suivant,")
                     (t "de lui faire subir séquentiellement une translation,")
                     (t "puis une rotation, puis finalement l'effet de la gravité."))))
  
  (slide/title/center
   "Détection des chocs contre les parois"
   (page-item "Pour savoir si un polygone touche l'une des parois de la fenêtre" 
              "d'affichage, il suffit de savoir si les points extrema (ceux dont"
              "l'une des coordonnées majore celles de tous les autres points) se trouvent ou non dans la fenêtre.")
   'next
   (page-item "Par exemple, pour rechercher l'indice du point d'abscisse minimale, nous pouvons utiliser la fonction :")
   (frame (scale (code 
                  (code:comment "minx : polygone -> int")
                  (define (minx poly)
                    (code:comment "iter : int x int -> int")
                    (define (iter i indice-min)
                      (code:comment "Si i dépasse la taille du polygone")
                      (if (<= (polygone-size poly) i)
                          (code:comment "On renvoit l'indice du point de plus petit abscisse")
                          indice-min
                          (code:comment "Sinon, si l'abscisse du point courant est plus petit que celui de indice-min")
                          (if (< (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))) 
                                 (vect-x (masse-r (vector-ref (polygone-poly poly) indice-
                                                              min))))
                              (code:comment "On itére en modifiant l'indice")
                              (iter (add1 i) i)
                              (iter (add1 i) indice-min))))
                    (iter 1 0))) 0.40))
   'next
   (page-item "Cette méthode, en O((polygone-size poly)), est extrêmement naïve, car elle parcourt l'ensemble des points de tout le polygone..."))
  
  (slide/title/center
   "Détection des chocs contre les parois"
   (page-item "Nous pouvons, pour améliorer cette recherche, suivre deux pistes :")
   'next
   (blank)
   (page-subitem "Ne pas parcourir tout le polygone, mais seulement les points de son enveloppe convexe : en effet, les extrema"
                 "sont bien situés sur cette dernière, et si le polygone n'est pas convexe, l'algorithme précédent sera en O(length (polygone-ev poly)).")
   'next
   (blank)
   (page-subitem "Utiliser le principe de dichotomie pour parcourir plus efficacement le nuage de point (la complexité devient alors logarithmique)."))
  
  (slide/title/center
   "Détection des chocs contre les parois"
   (page-item "Nous nous contenterons, devant les faibles tailles de données (au plus quelques centaines de points), d'un parcours linéaire. En revanche"
              "l'application de la première remarque est immédiate.")
   'alts
   (list (list (frame (scale (code 
                              (define (minx-ev poly)
                                (code:comment "iter : list x int -> int")
                                (define (iterev L indice-min)
                                  (code:comment "Si la liste est vide")
                                  (if (null? L)
                                      (code:comment "On renvoit l'indice du point de plus petit abscisse")
                                      indice-min
                                      (code:comment "Sinon, si l'abscisse du (car L) est plus petit que celui de indice-min")
                                      (if (< (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))) 
                                             (vect-x (masse-r (vector-ref (polygone-poly poly) indice-
                                                                          min))))
                                          (code:comment "On itére en modifiant l'indice")
                                          (iter (cdr L) (car L))
                                          (iter (cdr L) indice-min))))
                                (iter (polygone-ev poly) 0))) 0.50)))
         (list
          (page-item "Comme le montre" (exemple "cet exemple" "ev.scm") "(également visionnable à l'adresse" 
                     (lien-url "http://projetphysics.teria.org/Videos_du_slideshow/ev.html" "http://projetphysics.teria.org/Videos_du_slideshow/ev.html")
                     "), le gain en terme de nombre de points parcourus"
                     "peut être sensible si l'objet considéré n'est pas convexe...")
          'next
          (blank)
          (tt "Reste à calculer l'enveloppe convexe")
          (tt "d'un polygone"))))
  
  (slide/title
   "Enveloppe convexe"
   (page-item "Le problème de recherche de l'enveloppe convexe d'un polygone P revient à trouver un polygone convexe C"
              "(polygone tel que pour tout couple de point à l'intérieur de celui-ci, le segment les liant est inclus dans ce polygone)"
              "dont les sommets appartiennent à P, et tel que P soit à l'intérieur de C.")
   'next
   (image "ev.gif"))
  
  (slide/title/center
   "Présentation de l'algorithme de Graham"
   (page-item "Nous allons résoudre ce problème par l'algorithme de Graham, optimal dans le pire des cas, en O(nlog(n))")
   (blank)
   'alts
   (list (list (image "graham1.gif") (blank) (t "Soit P1 le point d'ordonnée minimale le plus à droite."))
         (list (image "graham2.gif") (blank) (t "On classe les points par ordre d'angles croissants") (t "par rapport à P1"))
         (list (image "graham3.gif") (blank) (t "On initialise une pile EV par les points P1 et P2."))
         (list (image "graham4.gif") (blank) (t "On avance dans la liste :") (t "Tant que la ligne est convexe..."))
         (list (image "graham5.gif") (blank) (t "On empile le point courant dans la EV."))
         (list (image "graham6.gif") (blank) (t "Si la ligne ne l'est plus :") (t "On dépile le dernier élément inséré dans EV..."))
         (list (image "graham7.gif") (blank) (t "Puis on empile le point courant"))
         (list (image "graham8.gif") (blank) (t "On s'arrête quand le dernier point est traité !"))))
  (slide/title
   "Implémentation de l'algorithme de Graham"
   (page-item "On se munit d'une structure abstraite de pile : ")
   'alts
   (list (list
          (frame (scale (code
                         (code:comment "Une pile est une structure à deux champs :")
                         (code:comment "sa taille et la liste qui la compose")
                         (define-struct pile (size l))
                         (code:comment "Une pile est vide si elle est de taille nulle")
                         (define (vide? pile)
                           (zero? (pile-size pile)))
                         (code:comment "Une pile initiale est vide")
                         (define (init-pile)
                           (make-pile 0 '()))) 0.75)))
         
         (list (frame (scale (code  
                              (code:comment "Ajout d'un élément à une pile - effet de bords")
                              (define (push! x pile)
                                (set-pile-size! pile (add1 (pile-size pile)))
                                (set-pile-l! pile (cons x (pile-l pile))))
                              (code:comment "pop! : pile* -> int")
                              (define (pop! pile)
                                (if (vide? pile)
                                    (code:comment "La liste ne doit pas être vide")
                                    (error "Pile vide !")
                                    (begin 
                                      (code:comment "On décrémente la taille de la pile")
                                      (set-pile-size! pile (- (pile-size pile) 1))
                                      (code:comment "On effectue une sauvegarde du premier élément...")
                                      (let ((copie (car (pile-l pile))))
                                        (code:comment "...avant de le supprimer")
                                        (set-pile-l! pile (cdr (pile-l pile)))
                                        (code:comment "Puis on le restitue")
                                        copie))))) 0.75)))
         
         (list (frame (scale (code 
                              (code:comment "elmt1 : pile* -> int")
                              (code:comment "Renvoit le premier élément de la pile")
                              (define (elmt1 pile)
                                (if (vide? pile)
                                    (error "Pile vide !")
                                    (car (pile-l pile))))
                              (code:comment "elmt2 : pile* -> int")
                              (code:comment "Renvoit le second élément de la pile")
                              (define (elmt2 pile)
                                (if (<= (pile-size pile) 1)
                                    (error "Pas assez d'elements dans la pile !")
                                    (cadr (pile-l pile)))) ) 0.75)))))
  
  (slide/title
   "Implémentation de l'algorithme de Graham"
   (page-item "On crée la fonction indice-depart retournant l'indice du point d'ordonnée minimale le plus à droite possible : ")
   'alts
   (list (list
          (frame (scale (code
                         (code:comment "list-min : poly -> list")
                         (code:comment "Renvoit la liste du (ou des) point(s) dont l'ordonnée est minimale")
                         (define (liste-min poly)
                           (code:comment "Itérativement, on balaye tous les points du polygone")
                           (define (iter i liste-indices-min)
                             (code:comment "Si on les a tous traités")
                             (if (<= (polygone-size poly) i)
                                 (code:comment "On renvoit la liste des indices")
                                 liste-indices-min
                                 (code:comment "Sinon, on compare l'ordonnée du point courant avec l'ordonnée minimale trouvée")
                                 (let ((a (vect-y (masse-r (vector-ref (polygone-poly poly) i)))) 
                                       (b (vect-y (masse-r (vector-ref (polygone-poly poly) (car liste-indices-min))))))
                                   (cond 
                                     (code:comment "Si le point courant est plus bas, on remplace la liste ((Oy) dirigé vers le bas)")
                                     ((> a b) (iter (add1 i) (list i)))
                                     (code:comment "S'il est à la même hauteur, on ajoute l'indice à la liste")
                                     ((= a b) (iter (add1 i) (cons i liste-indices-min)))
                                     (code:comment "S'il est plus haut, on ne modifie pas la liste...")
                                     (else (iter (add1 i) liste-indices-min))))))
                           (iter 1 (list 0)))) 0.50))
          'next
          (page-item "Complexité en O((polygone-size poly))."))
         (list
          (frame (scale (code
                         (code:comment "max : poly -> int")
                         (code:comment "A partir de la liste liste-indice-min obtenue précédemment,")
                         (code:comment "max renvoit l'indice du point initial de l'algorithme")
                         (define (max poly)
                           (define (iter L acc)
                             (if (null? L)
                                 acc
                                 (code:comment "On compare désormais les abscisses des points")
                                 (if (< (vect-x (masse-r (vector-ref (polygone-poly poly) acc))) 
                                        (vect-x (masse-r (vector-ref (polygone-poly poly) (car L)))))
                                     (iter (cdr L) (car L))
                                     (iter (cdr L) acc))))
                           (let ((liste-indice-min (liste-min poly)))
                             (iter  (cdr liste-indice-min) (car liste-indice-min))))) 0.50))
          'next
          (page-item "On en déduit la fonction cherchée, de complexité linéaire : ")
          (frame (scale (code
                         (define (indice-depart poly)
                           (max poly))) 0.75)))))
  
  (slide/title
   "Implémentation de l'algorithme de Graham"
   (page-item "On crée la fonction create-liste-angles associant à chaque indice de point l'angle qu'il fait avec pinit, le point initial de l'algorithme, d'indice init.")
   (frame (scale (code
                  (code:comment "create-liste-angles : polygone x int x vecteur -> list")
                  (define (create-liste-angles poly init pinit)
                    (code:comment "On travaille sur une liste L")
                    (let* ((L '()))
                      (code:comment "Il faut veiller à ne pas intégrer le point initial dans L")
                      (for i from 0 to (- init 1)
                           (code:comment "On calcule les coordonnées du vecteur Pinit Pi")
                           (code:comment "En veillant à l'orientation de l'axe (Oy) en Scheme")
                           (let ((vecteur (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly) i))) (vect-x pinit))
                                                     (- (vect-y pinit) (vect-y (masse-r (vector-ref (polygone-poly poly) i)))))))
                             (if (zero? (vect-y vecteur))
                                 (code:comment "Si les points sont à même altitude, l'angle est pi")
                                 (code:comment "car pinit est le plus à droite des points les plus bas")
                                 (set! L (cons (cons i pi) L))
                                 (set! L (cons (cons i (vect-angle vecteur)) L)))))
                      (code:comment "On procède de même pour les points d'indices supérieurs à init")
                      (for i from (add1 init) to (- (polygone-size poly) 1)
                           (let ((vecteur (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly) i))) (vect-x pinit))
                                                     (- (vect-y pinit) (vect-y (masse-r (vector-ref (polygone-poly poly) i)))))))
                             (if (zero? (vect-y vecteur))
                                 (set! L (cons (cons i pi) L))
                                 (set! L (cons (cons i (vect-angle vecteur)) L)))))
                      (code:comment "On retourne L")
                      L))) 0.35))
   'next
   (page-item "La complexité de l'algorithme est clairement en O((polygone-size poly))"))
  
  (slide/title/center
   "Implémentation de l'algorithme de Graham"
   (page-item "On développe un prédicat renvoyant #t si le point d'indice P est à gauche de la droite passant par les points"
              "d'indices A et B")
   (frame (scale (code
                  (define (a-gauche P A B)
                    (let* ((PA (-vect (masse-r (vector-ref (polygone-poly poly) A))
                                      (masse-r (vector-ref (polygone-poly poly) P))))
                           (PB (-vect (masse-r (vector-ref (polygone-poly poly) B))
                                      (masse-r (vector-ref (polygone-poly poly) P))))
                           (d (- (* (vect-x PA) (vect-y PB)) (* (vect-x PB) (vect-y PA)))))
                      (> 0 d)))) 0.60))
   'next
   (page-item "On suppose de plus que l'on dispose d'une fonction" (code (trier-selon-angles poly L pinit)) "de tri (implémenter dans notre projet par le tri fusion, en O(n log(n)))"
              "renvoyant la liste des indices des points du polygone triés par ordre croissant d'angles (avec, dans le cas de deux points ayant le même angle formé avec Pinit, pour seul indice apparaissant"
              "dans la liste l'indice du point le plus éloigné du point initial)."))
  
  (slide/title
   "Implémentation de l'algorithme de Graham"
   (page-para "On déduit finalement l'algorithme de l'enveloppe convexe d'un polygone en O(n log(n)) : ")
   (frame (scale (code
                  (code:comment "enveloppe-convexe : polygone -> int list")
                  (define (enveloppe-convexe poly)
                    (code:comment "Si le polygone est de taille 1, l'enveloppe est réduite à ce seul point")
                    (if (= (polygone-size poly) 1)
                        (list 0)
                        (let* ((init (indice-depart poly)) 
                               (pinit (masse-r (vector-ref (polygone-poly poly) init))) 
                               (code:comment "La pile qui fournira la solution")
                               (EV (init-pile))
                               (lpts (trier-selon-angles poly (create-liste-angles poly init pinit) pinit))
                               (m (length lpts))
                               (code:comment "On stocke la liste des points triés dans une pile")
                               (ppts (make-pile m lpts)))
                          (code:comment "On initialise EV avec l'élément initial")
                          (push! init EV)
                          (code:comment "Ainsi que le point formant avec lui un angle minimal")
                          (push! (pop! ppts) EV)
                          (code:comment "Tant qu'il reste des points à traiter")
                          (while (not (vide? ppts))
                                 (let ((A (elmt1 EV)) (B (elmt2 EV)))
                                   (code:comment "Si le point courant ne perturbe pas la convexité de la ligne polygonale")
                                   (if (a-gauche (elmt1 ppts) A B)
                                       (code:comment "On l'ajoute à EV (et on le supprime de ppts)")
                                       (push! (pop! ppts) EV)
                                       (code:comment "Sinon... on dépile EV")
                                       (pop! EV))))
                          (pile-l EV))))) 0.50)))
  
  (slide/title/center
   "Evolution d'un polygone dans la fenêtre d'affichage"
   (page-item "Maintenant que nous savons parfaitement calculer (lors de leur création) l'enveloppe convexe de nos polygones,"
              "nous sommes en mesure de laisser"
              "évoluer, sous l'effet de la gravité, n'importe quel polygone.")
   'next
   (page-item "Nous procèderons selon l'algorithme rédigé en pseudo-language :")
   (frame (scale (code 
                  (define (evolution polygone)
                    (A,B,C,D <- indices des points extrema de l'enveloppe convexe)
                    (si ((pA est dans la fenêtre)
                         et (pB est dans la fenêtre)
                         et (pC est dans la fenêtre)
                         et (pD est dans la fenêtre))
                        alors
                        (debut (translation polygone)
                               (rotation polygone)
                               (gravite polygone))
                        sinon
                        (choc polygone paroi)))) 0.75)))
  
  (slide/title/center
   "Evolution d'un polygone dans la fenêtre d'affichage"
   (page-item "Nous ne détaillerons pas l'écriture des prédicats \"pX est dans la fenêtre\","
              "en revanche, intéressons nous au développement de la fonction choc.")
   'next
   (page-item "En réalité, nous n'allons pas programmer la fonction" (code (choc polygone paroi)) "mais implémenter directement la fonction"
              (code (choc poly1 poly2 contact normale e)) "qui modifiera les polygones 1 et 2 se percutant au point" (it "contact")
              "(un vecteur passé en argument) au cours d'un choc dont le coefficient d'élasticité est e, et ayant une normale n pointant"
              "vers le poly1.")
   'next
   (page-item "Cette dernière fonction permettra bien de traiter, en plus du choc entre deux polygones,"
              "le choc entre un polygone et une paroi : il suffit de prendre comme poly2 un polygone réduit à une masse infinie"
              "positionnée à l'endroit du point (extrema) le plus proche de la paroi, comme le montre l'illustration suivante."))
  
  (slide/title/center
   "Evolution d'un polygone dans la fenêtre d'affichage"
   (image "chocs.gif"))
  
  (slide/title/center
   "Evolution d'un polygone dans la fenêtre d'affichage"
   (page-para "La fonction choc sera donc définie de la manière suivante, conformément aux formules vues dans les parties précédentes :")
   'next
   (frame (scale (code 
                  (define (choc poly1 poly2 contact normale e)
                    (let* ((pi/2 (/ pi 2.))
                           (code:comment "Vecteur r1 : centre de masse de poly1 vers le point de contact")
                           (r1 (-vect contact (masse-r (polygone-cm poly1))))
                           (code:comment "Vecteur r1 : centre de masse de poly2 vers le point de contact")
                           (r2 (-vect contact (masse-r (polygone-cm poly2))))
                           (code:comment "Définition des vecteurs orthogonaux à ceux calculés juste avant")
                           (r1ortho (rotation (make-vect 0 0) r1 (+ pi/2))) 
                           (r2ortho (rotation (make-vect 0 0) r2 (+ pi/2)))
                           (code:comment "Calcul de j à partir de la dernière formule de la partie précédente")
                           (j (/ (- (* (+ 1 e) 
                                       (.scal normale
                                              (-vect (+vect (polygone-vtrans poly1) (*vect (polygone-vang poly1) r1ortho))
                                                     (+vect (polygone-vtrans poly2) (*vect (polygone-vang poly2) r2ortho)))))) 
                                 (+ (/ (masse-m (polygone-cm poly1))) 
                                    (/ (masse-m (polygone-cm poly2))) 
                                    (/ (sqr (.scal normale r1ortho)) (polygone-i poly1)) 
                                    (/ (sqr (.scal normale r2ortho)) (polygone-i poly2))))))
                      (code:comment "Modification de la vitesse angulaire du poly1")
                      (set-polygone-vang! poly1 (+ (polygone-vang poly1) (/ (* j (.scal normale r1ortho)) (polygone-i poly1))))
                      (code:comment "Modification de la vitesse angulaire du poly2")
                      (set-polygone-vang! poly2 (+ (polygone-vang poly2) (/ (* (- j) (.scal normale r2ortho)) (polygone-i poly2))))
                      (code:comment "Modification de la vitesse de translation du poly1")
                      (set-polygone-vtrans! poly1 (*vect 1 (+vect (polygone-vtrans poly1) (*vect (/ j (masse-m (polygone-cm poly1))) normale))))
                      (code:comment "Modification de la vitesse de translation du poly2")
                      (set-polygone-vtrans! poly2 (+vect (polygone-vtrans poly2) (*vect (/ (- j) (masse-m (polygone-cm poly2))) normale)))    
                      ))) 0.4))
   'next
   (blank)
   (vc-append (tt "Que faire si l'on est en présence")
              (tt "de deux polygones ?")))
  
  (slide/title
   "Détection de collisions entre deux polygones convexes"
   (page-item "Pour faire s'entrechoquer plusieurs polygones entre eux, nous allons d'abord poser quelques restrictions à notre modèle :")
   'next
   (page-subitem "Les polygones seront des convexes")
   (page-subitem "Deux polygones au plus seront dans la fenêtre d'affichage")
   (page-subitem "Les polygones auront des vitesse faibles : en particulier, le champ de gravité ne sera pas important")
   'next
   (page-item "Modulo ces quelques limitations pratiques, nous allons être capable de :")
   'next
   (page-subitem "Déterminer à quel moment deux polygones entrent en collision")
   (page-subitem "Déterminer le point d'impact")
   (page-subitem "Connaître la normale au choc")
   (blank)
   'next
   (vc-append (tt "Grâce à la fonction précédente, nous saurons")
              (tt "faire interagir deux polygones entre eux.")))
  
  (slide/title
   "Présentation de l'algorithme de détection"
   (page-item "Pour savoir si deux polygones sont en collision, nous allons nous demander, pour le polygone1 d'abord,"
              "puis pour le polygone 2, s'il existe un point pour lequel la distance qui le sépare d'un coté de l'autre polygone"
              "est faible ou pas.")
   'next
   (page-item "On définit la distance entre un point et un polygone comme étant la plus courte distance entre ce point et l'un des cotés du polygone :")
   (blank)
   (image "distances.gif"))
  
  (slide/title
   "Présentation de l'algorithme de détection"
   (page-item "L'hypothèse des faibles vitesses est dictée par la technique de reconnaissance de la collision choisie :")
   'next
   (page-item "En fait, nous détecterons si un point appartient ou non à la bande bleue ci-dessous :")
   (image "distances2.gif")
   'next
   (page-item "Le risque avec les vitesses élevées étant d'être dans cette configuration :")
   (image "distances3.gif"))
  
  (slide/title
   "Présentation de l'algorithme de détection"
   (page-item "En fait, on peut définir la distance entre un point et un polygone comme étant la distance de ce point"
              "au segment [AB], de telle sorte que si O est l'isobarycentre des points constituant le polygone,"
              "le point appartient au secteur défini par AOB.")
   'next
   (tt "Vite un schéma !")
   'alts
   (list (list (image "distances4.gif") (t "P n'appartient pas au secteur BOC"))
         (list (image "distances5.gif") (t "Ni au secteur COD"))
         (list (image "distances6.gif") (t "En revanche, il appartient au secteur AOB"))
         (list (image "distances7.gif") (t "La distance du point P à poly")
               (t "est bien la distance d(P,[AB])"))))
  
  (slide/title
   "Présentation de l'algorithme de détection"
   (page-item "Le traitement de la détection de colision de deux convexes C1 et C2 de tailles n1 et n2,"
              "de centres O1 et O2 peut se faire comme suit :")
   (frame (scale (code (define (collision C1 C2)
                         (pour (tous les sommets S de C1)
                               (code:comment "Si S appartient au secteur angulaire (AO2B)")
                               ((A.B) <- (secteur angulaire auquel appartient S))
                               (d <- d(S,(A,B)))
                               (si (d < epsilon)
                                   (traitement de la collision entre C1 et C2)))
                         (pour (tous les sommets S de C2)
                               (code:comment "Si S appartient au secteur angulaire (AO1B)")
                               ((A.B) <- (secteur angulaire auquel appartient S))
                               (d <- d(S,(A,B)))
                               (si (d < epsilon)
                                   (traitement de la collision entre C1 et C2))))) 0.75)))
  
  (slide/title/center
   "L'algorithme de détection"
   (page-item "Définissons d'abord deux fonctions utiles :")
   'next
   (frame (scale (code 
                  (code:comment "mod2pi : float -> float")
                  (code:comment "Renvoit le paramètre modulo 2pi")
                  (define (mod2pi x)
                    (let ((2pi (* 2 pi)))
                      (cond ((< x 0) (mod2pi (+ x 2pi)))
                            ((<= 2pi x) (mod2pi (- x 2pi)))
                            (else x))))) 0.50))
   'next
   (frame (scale (code 
                  (code:comment "alphaext : polygone -> pair")
                  (code:comment "Renvoit les indices des masses possédant le plus petit et le plus grand alpha")
                  (define (alphaext poly)
                    (code:comment "Renvoit l'indice du point de plus petit alpha (parmis les deux points passés en argument)")
                    (define (min i j)
                      (if (< (masse-alpha (vector-ref (polygone-poly poly) i)) 
                             (masse-alpha (vector-ref (polygone-poly poly) j)))
                          i
                          j))
                    (code:comment "Renvoit l'indice du point de plus grand alpha (parmis les deux points passés en argument)")
                    (define (max i j)
                      (if (> (masse-alpha (vector-ref (polygone-poly poly) i)) 
                             (masse-alpha (vector-ref (polygone-poly poly) j)))
                          i
                          j))
                    (do ((k 0 (add1 k)) (mini 0 (min mini k)) (maxi 0 (max maxi k)))
                      ((= k (polygone-size poly)) (cons mini maxi))))) 0.50)))
  
  (slide/title/center
   "L'algorithme de détection"
   (page-item "Vient ensuite la fonction" (scale (code (secteur i poly1 poly2 alphaext2)) 0.85)
              "qui renvoit le secteur" (scale (code (A.B)) 0.85) ", comportant les indices des masses du poly2"
              "tel que le ième point du poly1 appartienne au secteur AO2B, connaissant les indices des masses"
              "du poly2 ayant les alpha extrema.")
   'next
   (frame (image "presentation.gif")))
  
  (slide/title/center
   "L'algorithme de détection"
   (frame (scale (frame (image "presentation.gif")) 0.8))
   'alts
   (list (list (page-item "On définit les constantes suivantes : ")
               (frame (scale (code (alpha (mod2pi (vect-angle (make-vect (- (vect-x (masse-r (vector-ref (polygone-poly poly1) i)))
                                                                            (vect-x (masse-r (polygone-cm poly2))))
                                                                         (- (vect-y (masse-r (polygone-cm poly2)))
                                                                            (vect-y (masse-r (vector-ref (polygone-poly poly1) i)))))))) 
                                   (size2 (polygone-size poly2)) 
                                   (alphamin (car alphaext2))
                                   (alphamax (cdr alphaext2))
                                   (sens (if (= (modulo (add1 alphamax) size2) alphamin) 'positif 'negatif))) 0.5)))
         (list (page-item "Evidemment, nous pourrions définir" (code secteur) "en parcourant tous les points de poly2 à partir d'alphamin, "
                          "et en nous arrêtant à la première masse dont l'angle dépasserait alpha...")
               'next
               (page-item "Dans le pire des cas, il faudrait parcourir les size2 points de poly2."
                          "En faisant le test pour les size1 points de poly1, la complexité de l'algorithme de détection serait en"
                          "O(size1*size2) (puisque les calculs de distances sont O(1)). Pour des polygones comportant beaucoup de points"
                          "(une centaine), cet algorithme serait impraticable..."))
         (list (page-item "En fait, nous pouvons optimiser l'algorithme naïf et lui donner une complexité"
                          "logarithmique. De deux choses l'une :")
               'next
               (page-subitem "Soit le point appartient à la zone 1, et il n'y a rien à faire.")
               (page-subitem "Sinon, il est dans la zone deux, et l'on peut par dichotomie, en réindexant de 0 à size2-1 les points de poly2"
                             "(par la transformation x->x-alphamin[size2] dans le cas du sens positif), définir le secteur auquel il appartient."))))
  
  (slide/title/center
   "L'algorithme de détection"
   (page-item "Définissons la fonction" (code (dicho i j)) "tenant compte de l'observation suivante :")
   'next
   (frame (scale (code
                  (code:comment "On utilisera dicho avec les conditions initiales")
                  (code:comment "(0,size2-1), et l'on aura toujours i<j")
                  (define (dicho i j)
                    (code:comment "Si le sens est positif")
                    (if (equal? sens 'positif)
                        (cond 
                          (code:comment "Si l'on considère deux points consécutifs")
                          ((<= (- j i) 1)
                           (code:comment "Si le point de poly1 est dans ce secteur")
                           (if (and (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamin) size2))))
                                    (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamin) size2)))))
                               (code:comment "On retourne ce secteur")
                               (cons (modulo (+ i alphamin) size2) (modulo (+ j alphamin) size2))
                               (code:comment "Sinon, le point n'appartient au secteur")
                               #f))
                          (code:comment "Si le point appartient au secteur (i,j) d'amplitude >= 2")
                          ((and (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamin) size2))))
                                (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamin) size2)))))
                           (code:comment "On détermine le milieu du secteur")
                           (let ((new (quotient (+ i j) 2)))
                             (code:comment "Le secteur cherché est soit dans le secteur (i,new), soit dans (new,j)")
                             (or (dicho i new) (dicho new j))))
                          (else #f))
                        (code:comment "Si le sens est négatif, la transformation de réindexation est x->x-alphamax[size2]")
                        (cond ((<= (- j i) 1)
                               (if (and (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamax) size2))))
                                        (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamax) size2)))))
                                   (cons (modulo (+ i alphamax) size2) (modulo (+ j alphamax) size2))
                                   #f))
                              ((and (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ j alphamax) size2))))
                                    (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) (modulo (+ i alphamax) size2)))))
                               (let ((new (quotient (+ i j) 2)))
                                 (or (dicho i new) (dicho new j))))
                              (else #f))))) 0.40)))
  
  (slide/title
   "L'algorithme de détection"
   (page-item "On en déduit la fonction secteur :")
   'next
   (frame (scale (code
                  (define (secteur i poly1 poly2 alphaext2)
                    (if 
                     (code:comment "Si le point est dans la zone 1")
                     (or (<= alpha (masse-alpha (vector-ref (polygone-poly poly2) alphamin)))
                         (>= alpha (masse-alpha (vector-ref (polygone-poly poly2) alphamax))))
                     (code:comment "(alphamin . alphamax) est le secteur auquel appartient le point")
                     (cons alphamin alphamax) 
                     (code:comment "Sinon, on le détermine avec dicho")
                     (dicho 0 (- size2 1))))) 0.50))
   'next
   (page-item "La fonction secteur est bien en O(log(size2)) : à chaque itération, la longueur de l'intervalle"
              "passé en paramètre de dicho (j-i) est divisée par deux.")
   'next
   (page-item "Pour traiter la collision entre poly1 et poly2, il faut définir les secteurs auxquels appartiennent les size1 points de poly1"
              "(avec pour chacun d'eux, O(log(size2)) opérations), puis les secteurs des size2 points de poly2. Finalement :")
   'next
   (frame (vc-append (tt "L'algorithme de détection est quasi-linéaire, en")
                     (tt "O(size1 log(size2) + size2 log(size1))"))))
  
  ;Conclusion
  (chapitrage 'conclusion)
  
  
  (slide/title/center
   "Conclusion"
   (page-item "Malgrès ses limites, notre projet permet d'apréhender de manière un peu plus concrète les phénomènes"
              "physiques auxquels nous sommes confrontés au quotidien.")
   'next
   (page-item "Le fait qu'il use d'exemples académiques (directement tirés de cours), ou plus ludiques (le dessin à main levée)"
              "est suceptible d'intéresser le plus grand nombre de personnes. Et la présente documentation, relativement détaillée, permet"
              "à chacun de (re)découvrir les principes de bases de la mécanique, en abordant certains points de manière moins conventionnelle"
              "qu'on pourrait le penser (cf la définition d'une force, ou le postulat de Descartes comme fondement de la mécanique).")
   'next
   (page-item "La dernière partie peut également pousser les lecteurs les plus inspirés à réfléchir aux solutions des limitations"
              "imposées par notre implémentation : envisager la triangularisation des polygones pour traiter le cas des non convexes,"
              "généraliser le problème à N corps, gérer la friction (comme cela est fait dans la partie objet du projet)..."))
  
  (slide/title
   "Conclusion"
   (page-item "Pour ce slideshow, ainsi que le reste du projet, nous avons utilisés : DrScheme, Pages, Appleworks, Slideshow,"
              "Equation editor, Adobe Photoshop, Jing, iWeb, Cyberduck.")
   'next
   (page-item "Ce projet a été réalisé à partir :")
   'next
   (page-subitem "De notes prises lors du cours de" (it "Mécanique I") "dispensé en première année de licence MP"
                 "par M. COULLET")
   'next
   (page-subitem "Du polycopié du cours de complexité de M. AVNAIM")
   'next
   (page-subitem "Et, bien évidemment, du cours de l'option" 
                 (it "Programmation") (it "Fonctionnelle II"))
   'next
   (page-item "Nous tenons à remercier ces enseignants pour l'aide qu'ils nous ont apportée au cours du projet...")
   'next
   (page-item "et espèrons vivement"
              "vous retrouver sur notre site web."))
  
  (slide/center
   (scale (code (the-end)) 4)))