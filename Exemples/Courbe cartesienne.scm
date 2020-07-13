
;*****************************************************

;                PROJET PHYSICS
;                  ANNEE 2008
;      PROGRAMMEURS : F. CORLAIS & A. JACQUEMIN
;               MAJ : 27.04.2008

;*****************************************************


;*****************************************************

;              "courbe cartesienne.scm"
;    ce fichier permet la création d'un polygone 
;       selon une paramétrisation cartésienne

;*****************************************************

;pour vérifier la syntaxe de l'expression saisie par l'utilisateur
;on tokenize la chaine de caractere qu'il saisit, qui ne doit contenir
;que des nombres, des ".", des " ", les fonctions tan, tanh, sin, sinh,
;cos, cosh, sqrt, sqr, expt, exp, log, abs, le paramètre t et la constante pi
;si la chaine comporte une autre séquence, on déclenchera une erreur
(define (tokenizer STR)
  (let ((*i* 0))
    (define (get-char)
      (if (>= *i* (string-length STR))
          #f
          (let ((c (string-ref STR *i*)))
            (set! *i* (+ *i* 1))
            c)))
    (define (unget-char)
      (set! *i* (- *i* 1)))
    (define (digit->int c)  ; c est un chiffre
      (- (char->integer c) (char->integer #\0)))
    (define (char->symbol c)
      (string->symbol (string c)))
    (define (get-token)
      (let ((c (get-char)))
        (if (not c)
            #f
            (cond (;reconnaissance des symboles usuels
                   (member c '(#\. #\+ #\- #\* #\/ #\( #\) #\space)) (char->symbol c))
                  (;reconnaissance des nombres
                   (char-numeric? c) (get-number (digit->int c)))
                  (;reconnaissance des fonctions cos, cosh
                   (and (equal? c #\c) (equal? (get-char) #\o) (equal? (get-char) #\s)) 
                   (let ((c2 (get-char)))
                     (if (equal? c2 #\h)
                         'cosh
                         (if (equal? c2 #\space)
                             'cos
                             (error "Votre chaine n'est pas une expression arithmetique valide")))))
                  (;reconnaissance des fonctions sin, sinh, sqrt, sqr
                   (equal? c #\s)
                   (case (get-char)
                     ((#\i) (if (equal? (get-char) #\n)
                                (case (get-char)
                                  ((#\h) 'sinh)
                                  ((#\space) 'sin)
                                  (else (error "Votre chaine n'est pas une expression arithmetique valide")))
                                (error "Votre chaine n'est pas une expression arithmetique valide")))
                     ((#\q) (if (equal? (get-char) #\r)
                                (case (get-char)
                                  ((#\t) 'sqrt)
                                  ((#\space) 'sqr)
                                  (else (error "Votre chaine n'est pas une expression arithmetique valide")))
                                (error "Votre chaine n'est pas une expression arithmetique valide")))
                     (else (error "Votre chaine n'est pas une expression arithmetique valide"))))
                  (;reconnaissance des fonctions tan, tanh, et du paramètre t
                   (equal? c #\t) 
                   (let ((c3 (get-char)))
                     (if
                      (and (equal? c3 #\a) (equal? (get-char) #\n))
                      (let ((c2 (get-char)))
                        (if (equal? c2 #\h)
                            'tanh
                            (if (equal? c2 #\space)
                                'tan
                                (error "Votre chaine n'est pas une expression arithmetique valide"))))
                      't)))
                  (;reconnaissance de la fonction exp (et de expt car t est est valide dans l'expression saisie)
                   (and (equal? c #\e) (equal? (get-char) #\x) (equal? (get-char) #\p)) 'exp)
                  (;de la fonction log
                   (and (equal? c #\l) (equal? (get-char) #\o) (equal? (get-char) #\g)) 'log)
                  (;de la constante pi
                   (and (equal? c #\p) (equal? (get-char) #\i)) 'pi)
                  (;de la fonction abs
                   (and (equal? c #\a) (equal? (get-char) #\b) (equal? (get-char) #\s)) 'abs)
                  (;sinon déclenchement d'une erreur
                   else (error "Votre chaine n'est pas une expression arithmetique valide"))))))
    (define (get-number n)
      (let ((d (get-char)))
        (cond ((not d) n)
              ((char-numeric? d) (get-number (+ (* n 10) (digit->int d))))
              (else (unget-char) n))))
    get-token))

; string représentant la paramétrisation x(t) donnée par l'utilisateur 
(define x_saisi
  CART_X)

(define get_x (tokenizer x_saisi))

;s'il y a des mots interdits : pour tester l'erreur -> (list? x_non_degenere?)
(define x_non_degenere?
  (with-handlers ((exn? (lambda (e) 
                          (send BUTTON_EXS_ACTION enable #f)
                          (send SLIDER_EXS_ANGLE enable #f)
                          (send BITMAP-DC clear)
                          (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                          (send MSG_EXS_CART set-label "Mauvaise saisie dans x(t)"))))
    (do ((x (get_x) (get_x)) (L '() (cons x L)))
      ((not x) (reverse L)))))

;s'il y a une mauvaise gestion des parentheses : pour tester l'erreur -> (procedure? x)
(define x
  (when (list? x_non_degenere?)
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Erreur dans la construction de x(t)"))))
      (eval (append '(lambda (t)) (read-from-string-all x_saisi))))))

; string représentant la paramétrisation y(t) donnée par l'utilisateur 
(define y_saisi
  CART_Y)

(define get_y (tokenizer y_saisi))

(define y_non_degenere?
  (with-handlers ((exn? (lambda (e) 
                          (send BUTTON_EXS_ACTION enable #f)
                          (send SLIDER_EXS_ANGLE enable #f)
                          (send BITMAP-DC clear)
                          (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                          (send MSG_EXS_CART set-label "Mauvaise saisie dans y(t)"))))
    (do ((y (get_y) (get_y)) (L '() (cons y L)))
      ((not y) (reverse L)))))

(define y
  (when (list? y_non_degenere?)
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Erreur dans la construction de y(t)"))))
      (eval (append '(lambda (t)) (read-from-string-all y_saisi))))))

; string représentant la borne inférieure donnée par l'utilisateur 
;(en fait, l'ordre des bornes n'a aucune incidence sur le déroulement du programme)
;on vérifie encore la véracité de la saisie de l'utilisateur
(define a_saisi
  CART_INF)

(define get_a (tokenizer a_saisi))

(define a_non_degenere?
  (with-handlers ((exn? (lambda (e) 
                          (send BUTTON_EXS_ACTION enable #f)
                          (send SLIDER_EXS_ANGLE enable #f)
                          (send BITMAP-DC clear)
                          (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                          (send MSG_EXS_CART set-label "Mauvaise saisie dans borne inférieure"))))
    (do ((a (get_a) (get_a)) (L '() (cons a L)))
      ((not a) (reverse L)))))

(define a
  (when (list? a_non_degenere?)
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Erreur dans l'évaluation de la borne inférieure"))))
      (eval (car (read-from-string-all a_saisi))))))

; string représentant la borne supérieure donnée par l'utilisateur 
;(en fait, l'ordre des bornes n'a aucune incidence sur le déroulement du programme)
;on vérifie encore la véracité de la saisie de l'utilisateur
(define b_saisi
  CART_SUP)

(define get_b (tokenizer b_saisi))

(define b_non_degenere?
  (with-handlers ((exn? (lambda (e) 
                          (send BUTTON_EXS_ACTION enable #f)
                          (send SLIDER_EXS_ANGLE enable #f)
                          (send BITMAP-DC clear)
                          (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                          (send MSG_EXS_CART set-label "Mauvaise saisie dans borne supérieure"))))
    (do ((b (get_b) (get_b)) (L '() (cons b L)))
      ((not b) (reverse L)))))

(define b
  (when (list? b_non_degenere?)
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Erreur dans l'évaluation de la borne supérieure"))))
      (eval (car (read-from-string-all b_saisi))))))

;On construit la liste des masses de la paramètrisation donnée, que l'on range dans le car de Lcart
;Si la liste des points n'est réduite qu'à un point du plan, on déclenche une erreur (le rayon d'un tel polygone étant nul)
;Ce fait est vérifié par le booleen rangé dans le cdr de Lpol
(define Lcart
  (when (and (number? a) (number? b) (procedure? x) (procedure? y))
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Mauvaise saisie"))))
      (let ((h (/ (- b a) 99)))
        (define (iter L BOOL i)
          (if 
           ;la définition des courbes est de 100 points (charge à l'utilisateur d'adapter son intervalle de définition)
           (= i 100)
           (cons L BOOL)
           (let* (;soit k la valeur de la variable de paramètrisation
                  (k (+ a (* h i)))
                  ;on crée la masse correspondante à cette variable dans la paramétrisation saisie
                  (m (make-masse 1 (make-vect (x k) (- (y k))) 0)))
             (iter 
              ;on ajoute à la liste la mase crée
              (cons m L)
              ;si la position de celle-ci est différente du car de la liste
              ;alors le boolleen devient #t
              (or BOOL
                  (if (null? L)
                      #f
                      (or (not (= (vect-x (masse-r m)) (vect-x (masse-r (car L)))))
                          (not (= (vect-y (masse-r m)) (vect-y (masse-r (car L))))))))
              (add1 i)))))
        (iter '() #f 0)))))

;création du polygone paramétré si le rayon n'est pas nul
(define POLY_PARAMETRE 
  (when (and (number? a) (number? b) (procedure? x) (procedure? y))
    (with-handlers ((exn? (lambda (e) 
                            (send BUTTON_EXS_ACTION enable #f)
                            (send SLIDER_EXS_ANGLE enable #f)
                            (send BITMAP-DC clear)
                            (send (send CANVAS_EXS get-dc) draw-bitmap BITMAP 0 0 'solid)
                            (send MSG_EXS_CART set-label "Mauvaise saisie"))))
      (if (cdr Lcart)
          (begin
            (make-poly 
             (car Lcart)
             0. 
             (make-vect 0 0)))
          (error "Un seul point")))))

;si le polygone a été créé, on réalise une homothétie rendant son rayon égal à 200 px
(when (not (void? POLY_PARAMETRE))
  (let ((d (/ 200 (polygone-rayon POLY_PARAMETRE))))
    (set!
     POLY_PARAMETRE
     (make-poly
      (map (lambda (x)
             (make-masse
              (masse-m x)
              (*vect d (masse-r x))
              (masse-alpha x))) 
           (vector->list (polygone-poly POLY_PARAMETRE)))
      0.01 
      (make-vect 0 0)))))

;si le polygone n'est pas vide, on centre le polygone paramétré ainsi modifié
(when (not (void? POLY_PARAMETRE))
  (let ((v (-vect (make-vect 250 250) (masse-r (polygone-cm POLY_PARAMETRE)))))
    (set-polygone-vtrans! POLY_PARAMETRE v)
    (translation-polygone POLY_PARAMETRE)
    (set-polygone-vtrans! POLY_PARAMETRE (make-vect 1 2))))

; Si POLY_PARAMETRE existe, alors on crée POLYGONE_EXS à partir de celui-ci que l'on tourne de ANGLE
(when (not (void? POLY_PARAMETRE))
  (begin (set! POLYGONE_EXS POLY_PARAMETRE)
         (for i from 0 to (- (polygone-size POLYGONE_EXS) 1)
              (set-masse-r! (vector-ref (polygone-poly POLYGONE_EXS) i)
                            (rotation (masse-r (polygone-cm POLYGONE_EXS)) (masse-r (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))
              (set-masse-alpha! (vector-ref (polygone-poly POLYGONE_EXS) i) (mod2pi (- (masse-alpha (vector-ref (polygone-poly POLYGONE_EXS) i)) ANGLE))))))