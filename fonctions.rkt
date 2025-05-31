#lang racket
(require srfi/1 srfi/13)
(require (only-in mzlib/string read-from-string-all expr->string))


(define (produire-fichier f obj)
  (call-with-output-file f
    (lambda (p-out)
      (write obj p-out))
    #:exists 'replace))

(define (lire-fichier f)
  (call-with-input-file f
    (lambda (in)
      (read in))))


(define-syntax while
  (syntax-rules ()
    ((_ pred? stmt ...)
     (do () ((not pred?))
       stmt ...))))


;; 
(define (liste->ensemble x)
  (cond((null? x)x)
       ((member (car x)(cdr x))(liste->ensemble (cdr x)))
       ((cons(car x)(liste->ensemble (cdr x))))))
 
(define (nb-occur x it)
  (cond((null? x)0)
       ((equal?(car x )it)
        (+ 1(nb-occur (cdr x )it)))
       ((nb-occur (cdr x )it))))



(define (nb-occur* X TXT)
  (let* ((index 0)  (lg (length X))
                    (Tp '())
                    (it ""))
    (letrec ((loop (lambda ()
                     (if(= lg index)
                        (sort  Tp #:key  cadr >)
                        (begin
                          (set! it (list-ref X index))
                          (set! Tp (cons (list it (nb-occur TXT it))Tp))
                          (set! index (+ index 1))
                          (loop))))))
      (loop))))



(define (butlast l)
  (reverse (cdr (reverse  l))))
(define (foo x)
  (cond((null? x) x)
       ((null? (cdr x))x)
       ((cons(car x)(foo(cddr x)))))) 
;; 
(define  (Str->m l );decoupage standard

  (let* ((index 0)
         (tampon '())(Tp '())
         (Longueur (string-length l))
         (item ""))
    (letrec ((loop (lambda ()
                     (if(= Longueur index)

                        (delete "" (reverse (cons (list->string (reverse tampon)) Tp)))
                        (begin
                          (set! item (string-ref l index));(print item)(newline)
                          (cond
                            ((member item '( #\space #\newline #\tab)) 
                             (begin
                               (set! tampon (reverse  tampon))
                               (set! Tp (cons (list->string tampon) Tp))(set! tampon '())
                               (set! index (+ index 1))
                               (loop)))
                                

                            ((member item '(#\—  #\. #\, #\?  #\! #\: #\; #\( #\) #\« #\» #\“ #\„))
                             (begin
                               (set! tampon (reverse tampon))
                               (set! Tp (cons  (list->string tampon )Tp))
                               (set! tampon '())
                               (set! Tp (cons ( string item)Tp))
                               (set! index (+ index 1))
                               (loop)))

 
                            ((char=? item  #\’)

                                      (begin 
                        
                               (set! tampon  (cons item tampon)) 
                               (set! index (+ index 1))
                               (loop)))


                            ((begin
                               (set! tampon (cons item tampon))
                               (set! index (+ index 1))
                               (loop)))
                            )
                          )))))
      (loop ))))


(define  (it->seq l)
  (let* ((index 0)
         (tampon '())(Tp '())
         (Longueur (length l))
         (item ""))
    (letrec ((loop (lambda ()
                     (if(= Longueur index)
                        (reverse (cons (reverse (cons item tampon)) Tp))
                        (begin
                          (set! item (list-ref l index))
                          (cond


                            ((member item '(".""?""!"":"";"))
                             (begin
                               (set! tampon (reverse(cons item tampon)))
                               (set! Tp (cons  tampon Tp))
                               (set! tampon '())
                               (set! index (+ index 1))
                               (loop)))

                            ((begin
                               (set! tampon (cons item tampon))
                               (set! index (+ index 1))
                               (loop)))
                            ))))))
      (loop))))

(define VY* '( #\a #\e  #\ê  #\i #\o #\ñ #\ù #\u))



(define (flag? x)
  (if (member x'( #\a #\e  #\ê  #\i #\o #\ñ #\ù #\u ))'v
      'k))

(define (voy-kons x)
  (let* (
         (x (string->list x))
         (lg (length x))
         (tp-flag (list (flag?(car x))) )
         (flag-p(car tp-flag ))
         (flag-c "")
         (tp (list (car x)))
         (TP '())
         (it "")
         (index 1))
    (letrec ((loop (lambda ()
                     (if(= index lg)(reverse (cons(list->string (reverse tp))TP))
                        (begin
                          (set! flag-p (car   tp-flag))
                          (set! it (list-ref x index))
                          (set! flag-c (flag? it))
                          (cond((equal? flag-p flag-c)
                                (begin    

                                  (set! tp (cons it tp))
                                  (set! tp-flag (cons flag-c tp-flag))
                                  (set! index (+ index 1))
                                  (loop)))
                               ((begin
                                  
                                  (set! TP (cons (list->string (reverse tp))TP))
                                     
                                  (set! tp-flag (cons flag-c tp-flag))
                                  (set! tp '())
                                  (set! tp (cons it tp))
                                  (set! index (+ index 1))
                                  (loop)))))))))
      (loop))))

;; 
;; 

(define (valeur-cle cle aliste)
  (let((doublet (assq cle aliste)))
    (if doublet
        (cdr doublet)
        #f)))

;
(define (supprime-cle cle aliste)
  (cond((null? aliste)'())
       ((eq? cle (caar aliste))(supprime-cle cle (cdr aliste)))
       (else(cons(car aliste)(supprime-cle cle (cdr aliste)))))) 

(define (sublis aliste s)
  (cond((pair? s)(cons(sublis aliste (car s))(sublis aliste (cdr s))))
       ((null? s)'())
       (else(let((paire(assoc s aliste)))
              (if paire
                  (cdr paire)
                  s)))))
;
;;(sublis '((un . one)(deux . two)(trois . three))'(un + deux = trois))
;
;
;;pour augmenter une a-liste
;
(define (acons s1 s2 aliste)
  (cons(cons s1 s2) aliste))


;
(define (aliste-change aliste cle valeur)
  (let ((aliste aliste))
    (cond((null? (assq cle aliste))(set! aliste(acons cle valeur)))
         ((begin(set! aliste(supprime-cle cle aliste))(set! aliste (acons cle valeur aliste)))))
    aliste))



(define (rplaca X a)
  (cons a (cdr X)))

(define (ajouter-ou-creer cle valeur dico)
  (define trouvée? #f)
  (define dico-mis-à-jour
    (map (lambda (entree)
           (if (equal? (first entree) cle)
               (begin
                 (set! trouvée? #t)
                 (list cle (cons valeur (second entree))))
               entree))
        dico))
  (if trouvée?
      dico-mis-à-jour
      (begin
      (cons (list cle (list valeur)) dico-mis-à-jour)
     
      )))



(define (profondeur L)
  (cond((null? L)0)       
        ((not(list? L))0)
     (else(+ 1 (profondeur (car L))))))


(define (majuscule? mot)  
      (if (char-upper-case? (string-ref mot 0))#t #f))

(define (dans-dico x liste)
  (if(member x liste )#t #f ))

 (provide (all-defined-out))




