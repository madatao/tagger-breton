#lang racket

(require srfi/1 srfi/13)
(require (only-in mzlib/string read-from-string-all expr->string))
(require "fonctions.rkt")




(define k-d "Jemand mußte Josef K verleumdet haben, denn ohne daß er etwas Böses\ngetan hätte, wurde er eines Morgens verhaftet. Die Köchin der Frau\nGrubach, seiner Zimmervermieterin, die ihm jeden Tag gegen acht Uhr\nfrüh das Frühstück brachte, kam diesmal nicht. Das war noch niemals\ngeschehen. K wartete noch ein Weilchen, sah von seinem Kopfkissen aus\ndie alte Frau, die ihm gegenüber wohnte und die ihn mit einer an ihr\nganz ungewöhnlichen Neugierde beobachtete, dann aber, gleichzeitig\nbefremdet und hungrig, läutete er. Sofort klopfte es und ein Mann, den\ner in dieser Wohnung noch niemals gesehen hatte, trat ein. Er war\nschlank und doch fest gebaut, er trug ein anliegendes schwarzes Kleid,\ndas ähnlich den Reiseanzügen mit verschiedenen Falten, Taschen,\nSchnallen, Knöpfen und einem Gürtel versehen war und infolgedessen,\nohne daß man sich darüber klar wurde, wozu es dienen sollte, besonders\npraktisch erschien. Wer sind Sie?  fragte K und saß gleich halb\naufrecht im Bett.")

(define T-k1 "Moarvat e oa bet falstamallet Josef K gant unan bennak. Rak hep m'en dije graet an disterañ gaou da zen e voe lakaet, ur beurevezh, harz warnañ. Keginerez an itron Grubach, e lojerez, hag a zegas dezhañ bep mintin war-dro eizh eur e lein-vihan, ne zeuas ket ar wech-mañ. Biskoazh c'hoazh ne oa bet c'hoarvezet an dra-se. K a c'hortozas ur pennadig; diwar e c'houbenner e wele penaos e veze ar vaouez kozh a oa o chom a-dal dezhañ o sellout outañ gant ur ranellerezh digustum eviti; neuze avat, sebezet ha naonek war un dro, e sonas. Diouzhtu e skoas unan bennak ouzh an nor hag un den ha n'en doa gwelet biskoazh c'hoazh en ti-se a zeuas tre. Mistr ha frammet-mat e oa; gwisket e oa gant ul lifre du hag enk, heñvel a-walc'h ouzh dilhadou-beaj, warnañ plegoù liesdoare, godelloù, boukloù, nozelennoù hag ur gouriz; gant-se e seblante ar gwiskamant-se, hep ma vije bet anat da betra e servije an holl vitrakou warnañ, bezañ pleustrek-kenañ.
Ha piv oc'h? a c'houlennas K. en doa hanter savet e gein diwar e wele.")




(define Ponctuation'("—" "," "?" "!" ":" ";" "(" ")" "«" "»" "."))
;; 
(define (ponct? x)
  (if(member x Ponctuation)#t #f))

(define Mut-A'("a" "ne" "na" "pa" "daou" "div" "gwall" "hanter" "re" "holl" "seul" "war" "diwar" "dindan" "dre" "da" "tra" "endra" "ur" "ar" "un" "em" "da""pe" ) ); "e"
(define Mut-S'("me" "he""am" "em" "tri""teir""pevar""peder""nav""ar""ur"))
(define Mut-R '("ho""az""ez""d’an""d’ar"))

(define Mut-M '("o" "e" "ma" ))

(define mutateur* (flatten (list Mut-A Mut-S Mut-R Mut-M )))

(define (mutateur? x)
  (if(member x mutateur*)#t #f))

(define suffixes-nom'("ioù""où""ou" "ed" "ien" "iz" "ezed" "ienez" "i" "ennou" "oùoù" "oùed"))

(define suffixes-adj
  '("ek" "us" "el" "ig" "oc’h" "lañ" "ell" "ienn" "er" "adur"))
;ez  "añ"

(define suffixes-verbe
  (append
   '("an" "a" "as""omp" "it" "ont")    ; présent
   '("en" "es" "e" "emp" "ec'h" "ent")  ; imparfait
   '( "in""i" "o" "imp" "it" "int");futur
   '( "et") ;participe passé
   '( "iñ""añ""iañ") ;participe passé
   ))

(define suffixes-v-a '("ez""añ"))
(define suffixes-v-n  '("i"))
(define suffixes-a-n '("i"))

(define SUFF*(append suffixes-nom(append suffixes-adj suffixes-verbe)))


(define(dek-suf str Sf)
  (cond((null? Sf )(list str))
       ((string-suffix?(car Sf) str )
        (list  (string-take str (- (string-length str)(string-length(car Sf))))  
               (car Sf)))
       ((dek-suf str (cdr Sf)))))

(define  (dek-mot m );decoupage standard

  (let* ((index 0)
         (tampon '())(Tp '())
         (Longueur (string-length m))
         (item "")
         (l (string->list m)))
    
    (letrec ((loop (lambda ()
                     (if(= Longueur index)

                        (delete '() (reverse (cons (list->string (reverse tampon)) Tp)))
                        (begin
                          (set! item (list-ref l index));(;;print item)(newline)
                          (cond
                            ((member item '( #\-)) 
                             (begin
                               (set! tampon (reverse  tampon))
                               (set! Tp (cons (list->string tampon) Tp))(set! tampon '())
                               (set! index (+ index 1))
                               (loop)))
                         


                            ((begin
                               (set! tampon (cons item tampon))
                               (set! index (+ index 1))
                               (loop)))
                            )
                          )))))
      (loop ))))




(define not-identified '())




(define dokum-compound
  (lambda (CP)
    (cond
      [(null? CP) '()]
      [(member (car CP) index-lex)
       (cons (list (car CP) (vector->list (hash-ref Dico1 (car CP))))
             (dokum-compound (cdr CP)))]
      [(member (car CP) index-add)
       (cons (list (car CP) (cadr (member (car CP) Add)))
             (dokum-compound (cdr CP)))]
      [else
       (cons (list (car CP) '(nil))
             (dokum-compound (cdr CP)))])))


;; (define (majuscule? mot)  
;;   (if (char-upper-case? (string-ref mot 0))#t #f))
                        

(current-directory "/Users/izuko/Desktop/breton/dictionary/")

(define Dico1 (lire-fichier "Dict.rktd"))

(define (cles-de-hashtable ht)
  (hash-keys ht))

(define index-lex  (cles-de-hashtable Dico1))

(define forme-contract
  
  '( "m'en"   ( (me + en) 1S )
              ; "n'en"   ( (me + en) 1S );"PRON:pers + PRON:obj"  "je... en")
              "d'en"   ((di + en )  2S )                 ; "PRON:pers + PRON:obj" "tu... en")
              "h'en"   ( ( he  + en )  3S)              ;  "PRON:pers + PRON:obj" "il/elle... en")
              "hon'en"  ( (  hon  + en) 1P);  "PRON:pers + PRON:obj" "nous... en")
              "ho'en" (( ho +  en ) 2P ) ; "PRON:pers + PRON:obj" (  "vous... en")
              "o'en"  ( (o  +  en )  3P ) ; "PRON:pers + PRON:obj"  "ils/elles... en")
              "m'az"  (( me  + az )  1S ) ; "PRON:pers + PRON:obj" (2S)  "que je te...")
              "d'her"  (( di +  her) 2S ) ; "PRON:pers + PRON:obj" (3Sf)   "que tu la...")
              "n'he"    ((ne +  he )  3S )))

(define Np** '("Josef"  "K" ))

(define Add '("d'ar" ("Adv < prep. + Art-def.")
                     "d'an" ("Adv < prep. + Art-def.")
                     "kein" ("N" "où")
                     "ul" ("Art-i")
                     "kenañ" ("adv")
                     "n'en"   ( "PRON:pers + PRON:obj"(neg + en) 3S ) 

                     "m'en"   ( "PRON:pers + PRON:obj"(me + en) 1S )      
                     "d'en"   ("PRON:pers + PRON:obj"(di + en )  2S )       
                     "h'en"   ("PRON:pers + PRON:obj" ( he  + en )  3S)        
                     "hon'en"  ("PRON:pers + PRON:obj" (  hon  + en) 1P) 
                     "ho'en" ("PRON:pers + PRON:obj"( ho +  en ) 2P )  
                     "o'en"  ("PRON:pers + PRON:obj" (o  +  en )  3P )  
                     "m'az"  ("PRON:pers + PRON:obj"( me  + az )  1S )  
                     "d'her"  ("PRON:pers + PRON:obj"( di +  her) 2S ) 
                     "n'he"    ( "PRON:pers+neg"(ne +  he )  3S )))



(define Prep-cj
  '(
    ;; da ("à")
    "din"    ("da" "1S")
             "dit"    ("da" "2S")
             "dezhañ" ("da" "3S.M")
             "dezhi"  ("da" "3S.F")
             "deomp"  ("da" "1P")
             "deoc'h" ("da" "2P")
             "dezho"  ("da" "3P")

             ;; gant ("avec")
             "ganin"   ("gant" "1S")
             "ganit"   ("gant" "2S")
             "gantañ"  ("gant" "3S.M")
             "ganti"   ("gant" "3S.F")
             "ganeomp" ("gant" "1P")
             "ganeoc'h"("gant" "2P")
             "ganto"   ("gant" "3P")

             ;; evit ("pour")
             "evidon"  ("evit" "1S")
             "evidout" ("evit" "2S")
             "evitañ"  ("evit" "3S.M")
             "eviti"   ("evit" "3S.F")
             "evidomp" ("evit" "1P")
             "evidoc'h"("evit" "2P")
             "evito"   ("evit" "3P")

             ;; ouzh ("envers")
             "ouzhin"  ("ouzh" "1S")
             "ouzhit"  ("ouzh" "2S")
             "outañ"   ("ouzh" "3S.M")
             "outi"    ("ouzh" "3S.F")
             "ouzhomp" ("ouzh" "1P")
             "ouzhoc'h"("ouzh" "2P")
             "outo"    ("ouzh" "3P")

             ;; war ("sur")
             "warnon"  ("war" "1S")
             "warnout" ("war" "2S")
             "warnañ"  ("war" "3S.M")
             "warni"   ("war" "3S.F")
             "warnomp" ("war" "1P")
             "warnoc'h"("war" "2P")
             "warno"   ("war" "3P")

             ;; dre ("à travers")
             "drezon"  ("dre" "1S")
             "drezout" ("dre" "2S")
             "drezañ"  ("dre" "3S.M")
             "drezi"   ("dre" "3S.F")
             "drezomp" ("dre" "1P")
             "drezoc'h"("dre" "2P")
             "drezo"   ("dre" "3P")

    

             ;; diouzh ("de, depuis")
             "diouzhin"  ("diouzh" "1S")
             "diouzhit"  ("diouzh" "2S")
             "dioutañ"   ("diouzh" "3S.M")
             "diouti"    ("diouzh" "3S.F")
             "diouzhomp" ("diouzh" "1P")
             "diouzhoc'h"("diouzh" "2P")
             "diouto"    ("diouzh" "3P")))
(set!  Add (append Add Prep-cj))
(define index-add (foo Add))
 
(define (demutation lettre mutation-type)
  (case mutation-type
    ((mut-lenn) ; mutation adoucissante
     (cond ((equal? lettre "m") "v")
           ((equal? lettre "d") "z")
           ((equal? lettre "k")"g")
           ((equal? lettre "p") "b")
           ((equal? lettre "t") "d")
           (else lettre)))
    ((mut-dur)
     (cond ((equal? lettre "b") "p")
           ((equal? lettre "d") "t")
           ((equal? lettre "g") "k")
           (else lettre)))
    ((mut-spir)
     (cond ((equal? lettre "z") "d")
           ((equal? lettre "v") "b")
           (else lettre)))
    (else lettre)))


(define (demutat-A mot) 
  (if (<(string-length mot )3)mot 

 
      (let*((init1 (string-take mot 1))
            (init3 (string-take mot 3))
            (m1 (string-drop mot 1))
            (m2 (string-drop mot 2))
            (m3 (string-drop mot 3))
            )
        (cond ((equal? init1 "g") (string-append "k" m1 )) 
              ((equal? init1 "d")  (string-append "t" m1 ))
              ((equal? init1 "b") (string-append "p" m1 ))
              ((equal? init3 "c'h") (list(string-append "g" m3 )))
              ((equal? init1 "w" )  (string-append "gw" m1 ))
              ((equal? init1 "z")   (string-append "d" m1 ))
              ((equal? init1 "v")  (string-append "b" m1 ))
              (else mot)
              ))))


(define (demutat-S mot)
  (let*((init1 (string-take mot 1))
        (init3 (string-take mot 3))
        (m1 (string-drop mot 1))
        (m2 (string-drop mot 2))
        (m3 (string-drop mot 3)))
    (cond ((equal? init3 "c'h") (string-append "k" m3 )) 
          ((equal? init1 "z")  (string-append "d" m1 ))
          ((equal? init1 "f") (string-append "p" m1 ))      
          (else mot)
          )))

(define (demutat-D mot)
  (let*((init1 (string-take mot 1))
        (init2 (string-take mot 2))
        (m1 (string-drop mot 1))
        (m2 (string-drop mot 2)))
    (cond ((equal? init1 "k") (string-append "g" m1 )) 
          ((equal? init2 "kw")  (string-append "gw" m2 ))
          ((equal? init1 "t") (string-append "d" m1 ))
          ((equal? init1 "p") (string-append "b" m1 )) 
          (else mot) 
          )))



(define (demutat-M mot)
  (let*((init1 (string-take mot 1))
        (init2 (string-take mot 2))
        (init3 (string-take mot 3))
        (m1 (string-drop mot 1))
        (m2 (string-drop mot 2))
        (m3 (string-drop mot 3))
        (ls '()));liste des possibilités
    (cond ((equal? init3 "c'h") (string-append "g" m3 )) 
          ((equal? init1 "w")  (string-append "gw" m2 ))
          ((equal? init1 "v") (string-append "b" m1 ))
          ((equal? init1 "p") (list(string-append "b" m1 )(string-append "m" m1 )))
          ((equal? init1 "t") (string-append "d" m1 ))
          (else mot) )))


;; (define (verbe-existe? mot)
;;   ;; essaie la démulation + suffixes verbaux
;;   (let* ((d (demutation  mot)) 
;;          (racines (dek-suf d suffixes-verbe)))
;;     (and (>= (length racines) 1)
;;          (member (car racines) index-lex))))

(define (mot-existe? mot)
  ;; essaie la démulation + suffixes verbaux
  (let* ((d (demutation  mot))
         (racines (dek-suf d SUFF*)))
    (and (>= (length racines) 1)
         (member (car racines) index-lex))))

(define  (tag-k1 l )
  (let* ((i 0)
         (Tp '("zyz"))
         (Lg (length l))(it-p "")(it-s "")
         (l (append l '("")))
         (it-c "")(it-c1(string-downcase it-c)))
          

    (letrec ((loop (lambda ()
                     (if(= Lg   i) 
                        (delete '()  (cdr (reverse  Tp)))
                        (begin
                                                                   
                          (set! it-c  (list-ref l i)) (set! it-c1 (string-downcase it-c))(set! it-p (car Tp))
                          (set! it-s (list-ref l (+ 1 i)))
                          (cond
                            ((member it-c Np**)
                             (begin 
                               (set! Tp (cons (list it-c '("NPR")'A1)Tp))  
                               (set! i (+ i 1))
                               (loop)))         
                            [(and (> i 1) (not (ponct? it-c)) (majuscule? it-c)) 
                             (begin
                               (set!  Np** (cons it-c Np** ))
                               (set! Tp (cons (list it-c '("NPR")'A2) Tp))
                               (set! i (+ i 1))
                               (loop))]
                            [(ponct? it-c)
                             (begin
                               (set! Tp (cons (list it-c '("pct")'A3) Tp))
                               (set! i (+ i 1))
                               (loop))]

                         
                            ( (member it-c1 '("an" "el" "ar"))
                              (begin
                                (set! Tp (cons (list it-c '("Art-def." 'A4)) Tp))
                                (set! Tp (cons (list it-s '("N-def" 'A4)) Tp))
                                (set! i (+ i 2))
                                (loop)))

                            ((and  (not(equal? it-p "zyz"))
                                   (equal? (car it-p)"betek") 
                                   (equal? it-c "ma"))
                             (begin
                               (set! Tp(cdr Tp))

                               (set! Tp (cons (list (car it-p) '( "prep" 'A5)) Tp))
                               (set! Tp (cons (list   it-c '( "CONJ" 'A5)) Tp))
                               (set! i (+ i 1))
                               (loop)))

                            ( (member it-c1 '("dud"))
                              (begin
                                (set! Tp (cons (list it-c '("N" '("tud" "< M-R" 'A6))) Tp))
                                (set! i (+ i 1))
                                (loop)))
                        
                               

 
                            ((and (equal?(string-downcase it-c) "e" )
                                  (>(string-length it-s )3)
                                  (member (demutat-A (car ( dek-suf it-s SUFF*))) index-lex))
                             (begin
                               (set! Tp (cons (list it-c '("Poss" 'A7)) Tp))
                               (set! Tp (cons (list  it-s 
                                                     (list "<= m-A" (demutat-A (string-downcase it-c))(vector->list (hash-ref Dico1   (demutat-A (car ( dek-suf it-s SUFF*)))  'A7 )))) Tp) )
                               (set! i (+ i 2)) 
                               (loop)))
                            

                            ((and (equal?(string-downcase it-c) "e" );ok
                                  (>(string-length it-s )3);ok
                                  (member (demutat-A (car ( dek-suf it-s suffixes-nom))) index-add))
                             (begin
                                    (set! Tp (cons (list it-c '("Poss" 'A7)) Tp))
                                    (set! Tp (cons (list  it-s 
                                                          (list "<= m-A"   (demutat-A (car ( dek-suf it-s suffixes-nom)))(cadr(member (demutat-A (car ( dek-suf it-s suffixes-nom)))Add))  'A7 )) Tp) )
                                    (set! i (+ i 2)) 
                                    (loop)))
                     



                            
                                     
                            ((and (member it-c '("ne"))
                                  (= (length (dek-suf it-s  suffixes-verbe))2))
                             (begin
                               (set! Tp (cons (list it-c '("Prt-V-neg" 'A9)) Tp))
                               (set! Tp (cons (list it-s '("V-cj-e" 'A9)) Tp))  
                               (set! i (+ i 2))
                               (loop)))
                            ((member it-c '("oa"))
                             (begin
                               (set! Tp (cons (list it-c  (vector->list (hash-ref Dico1 it-c))'A10)Tp))
                                 
                               (set! i (+ i 1))
                               (loop)))
                           

                         

                   
               
                          
                            ((begin
                               (set! Tp (cons(list  it-c  '("UNKN"))Tp))
                               ;(set! cpt (+ cpt 1))
                               (set! i (+ i 1))
                               (loop)))))))))
 
                           
                     
      (loop ))))

      
                    


(define  (tag-k2 l )
  (let* ((i 0)
         (Tp '(("zyz"("EE")) ))
         (Lg (length l))(it-p "")(it-s "")
         (l (append l '("zyz"("EE"))'("zyz"("EE"))))
         (it-c "")(it-c1 ""))
    (letrec ((loop (lambda ()
                     (if(= Lg   i)
                        (cdr (reverse   Tp))
                        (begin                                                                   
                          (set! it-c  (list-ref l i)) (set! it-p (car Tp)) (set! it-s (list-ref l (+ 1 i))) 
                          ; (newline)  (print "it-p")  (print it-p)(print "it-c")  (print it-c)  (print "it-s")  (print it-s)
                          (cond
                            
                            ((not(member  "UNKN" (flatten it-c)))
                             (begin 
                               (set! Tp (cons  it-c  Tp))                            
                               (set! i (+ i 1))
                               (loop)))

                           
                             
                            
                            ((and  (equal? (car it-p) "war")
                                   (equal? (car it-c) "un")
                                   (equal? (car it-s)  "dro")) 
                             (begin  
                               (set! Tp(cdr Tp))                           
                               (set! Tp (cons (list "war un dro"  '("Adv-Man" 'B1)) Tp))
                               (set! i (+ i 2))
                               (loop)))
                            ((and
                              (equal? "N"(string-take (caadr it-p)1))
                              (not (equal? "NPR"(caadr it-p)) )
                              (member  "UNKN" (flatten it-c)))   
                             (begin  
                               (set! Tp (cons (list (car it-c) '("Adj" 'B2)) Tp))  
                               (set! i (+ i 1))
                               (loop)))

                            ((and(member (car it-p )Mut-A)  
                                 (> (string-length (car it-c)) 3)  
                                 (member  (demutat-A (string-downcase (car it-c))) index-lex)) 
                             (begin  
                               (set! Tp (cons (list (car it-c) (list "<= m-A" (demutat-A (string-downcase (car it-c)))(vector->list (hash-ref Dico1   (demutat-A (string-downcase (car it-c)))))'B3 )) Tp))  
                               (set! i (+ i 1))
                               (loop)))

                            ((and(member (car it-p )Mut-S) 
                                 (member  (demutat-S (string-downcase (car it-c))) index-lex))    
                             (begin 
                               (set! Tp (cons (list (car it-c) (list "<= m-S" (demutat-S (string-downcase (car it-c)))(vector->list (hash-ref Dico1   (demutat-S (string-downcase (car it-c)))))'B4 )) Tp))  
                               (set! i (+ i 1))
                               (loop)))

                            ((and(member (car it-p )Mut-R) 
                                 (member  (demutat-D (string-downcase (car it-c))) index-lex)) 
                             (begin
                               (set! Tp (cons (list (car it-c) (list "<= m-D" (demutat-D (string-downcase (car it-c)))(vector->list (hash-ref Dico1   (demutat-D (string-downcase (car it-c)))))'B5 )) Tp))  
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((and(member (car it-p )Mut-M)
                                 (member  (demutat-M (string-downcase (car it-c))) index-lex))    
                             (begin 
                               (set! Tp (cons (list (car it-c) (list "<= m-M" (demutat-M (string-downcase (car it-c)))(vector->list (hash-ref Dico1   (demutat-M (string-downcase (car it-c)))))'B6 )) Tp))  
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((and(equal? (car it-p)"oa")
                                 (equal?(string-downcase (car it-c)) "o"))  
                             (begin 
                               (set! Tp (cons (list (car it-c) '("Prt-v-o") 'B7 ) Tp))
                               (set! Tp (cons (list (car it-s) '("V-inf") 'B7 ) Tp)) 
                               (set! i (+ i 2))
                               (loop)))

                              
                            ((equal?(string-downcase (car it-p)) "o")  
                             (begin)
                             (set! Tp (cons (list (car it-c) '("V-inf") 'B8 ) Tp)) 
                             (set! i (+ i 1))
                             (loop))

                            ((and(or(not(mutateur? (car it-p)))(= i 0))
                                 (member  (string-downcase (car it-c)) index-lex))  
                             (begin 
                               (set! Tp (cons (list (car it-c) (vector->list (hash-ref Dico1  (string-downcase (car it-c))))'B9)Tp))
                               (set! i (+ i 1))
                               (loop)))

                            ((and(equal? "V"(string-take (caadr it-p)1))
                                 (= 2 (length (dek-suf (car it-c)'("et")))))     
                             (begin 
                               (set! Tp (cons (list (car it-c) '("V-pp" 'B10)) Tp))   
                               (set! i (+ i 1))
                               (loop)))
            
                            ((and(or(not(mutateur? (car it-p)))(= i 0))
                                 (member  (string-downcase (car it-c)) index-add))  
                             (begin 
                               (set! Tp (cons (list (car it-c) (cadr (member  (string-downcase (car it-c)) Add))'B11 ) Tp))  
                               (set! i (+ i 1))
                               (loop)))

                            ((and(equal? (car it-p ) "a" ) 
                                 (> (string-length (car it-c)) 3) 
                                 (= (length (dek-suf (car it-c ) suffixes-verbe))2))
                             (begin 
                                    (set! Tp (cdr Tp))
                                    (set! Tp (cons (list (car it-p) '("Prt-v-a")'B12)Tp)) 
                                    (set! Tp (cons (list (car it-c) (list "<= m-A" (demutat-A (string-downcase (car it-c))) "V-cj-a" 'B12 )) Tp))  
                                    (set! i (+ i 1))
                                    (loop)))
                        
                            ((member (string-downcase (car it-c))index-add)
                             (begin  
                               (set! Tp (cons (list (car it-c) (vector->list (hash-ref Prep-cj  (string-downcase (car it-c))))'B13 ) Tp))  
                               (set! i (+ i 1))
                               (loop)))

                            ((string-contains? (car it-c) "-")
                             (begin
                               (define doc (dokum-compound (dek-mot (car it-c))))
                               (set! Tp (cons (list (car it-c) (list "COMP" doc) 'B14) Tp))
                               (set! i (+ i 1)) 
                               (loop)))

                              
                            ((= 2 (length (dek-suf (car it-c)suffixes-adj)))   
                             (begin 
                               (set! Tp (cons (list (car it-c) '("Adj" 'B15)) Tp))   
                               (set! i (+ i 1))
                               (loop)))
                                   
          
   
                            ((= 2 (length (dek-suf (car it-c) suffixes-nom)))  
                             (begin
                               (set! Tp (cons (list (car it-c) '("N-pl." 'B16)) Tp))  
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((= 2 (length (dek-suf(car it-c) '("et"))))  
                             (begin 
                               (set! Tp (cons (list (car it-c) '("part-p."'B17 )) Tp)) 
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((not(null? (lset-intersection equal?  (string->list (car it-c))'(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
                             (begin 
                               (set! Tp (cons (list (car it-c) '("Nbr." 'B18)) Tp)) 
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((member (string-downcase (car it-c))index-add)
                             (begin 
                               (set! Tp (cons (list (car it-c) (cadr (member (string-downcase (car it-c))Add)'B19)) Tp)) 
                               (set! i (+ i 1))
                               (loop)))

                            ((and(equal? (string-downcase (car it-c))"vo")
                                 (equal?(string-downcase (car it-p))"a"))
                             (begin 
                               (set! Tp (cdr Tp))
                               (set! Tp (cons (list (car it-p) '("Part-Va" 'B20)) Tp))
                               (set! Tp (cons (list (car it-c) '("VB-bezañ"  'B20)) Tp)) 
                               (set! i (+ i 1))
                               (loop)))

                          
                            ((member "CONJ" (cadr it-p))
                             (begin 
                               (set! Tp (cons (list (car it-c)  '("V-cj"  'B21)) Tp))
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((and  (equal? (car it-p) "da")
                                   (equal? (car it-c)  "zen")) 
                             (begin
                               (set! Tp(cdr Tp))
                               (set! Tp (cons (list (car it-p) '("Prep" 'B22)) Tp))  
                               (set! Tp (cons (list (car it-c) '("N" '("den" "< M-S")'B22)) Tp))
                               (set! i (+ i 1))
                               (loop)))
                            
                            ((member (string-downcase (car it-c)) index-lex)
                             (begin
                               (set! Tp (cons (list (car it-c) (vector->list (hash-ref Dico1 (string-downcase (car it-c))))'B23 ) Tp)) 
                               (set! i (+ i 1))
                               (loop)))

                            ((member (string-downcase (car it-c)) index-add)
                             (begin
                               (set! Tp (cons (list (car it-c) (cadr (member (string-downcase (car it-c))Add))103 ) Tp)) 
                               (set! i (+ i 1))
                               (loop)))

                            ((and(member "Card-f" (flatten (cadr it-p)))
                                 (member "UNKN" (flatten (cadr it-c))))
                             (begin
                               (set! Tp(cdr Tp))
                               (set! Tp (cons (list (car it-p) '("Card-f" 'B24 )) Tp))  
                               (set! Tp (cons (list (car it-c) '("Npl" 'B24)) Tp))
                               (set! i (+ i 1))
                               (loop)))

                            ((begin 
                               (set! Tp (cons  it-c  Tp))                            
                               (set! i (+ i 1))
                               (loop)))))))))
      (loop))))

 
(define  (normalize l )
  (let* ((i 0)
         (Tp '())
         
         (Lg (length l)) (bal "")
         (it-c ""))
    (letrec ((loop (lambda ()
                     (if(= Lg   i)
                        (reverse   Tp)
                        (begin                                                                   
                          (set! it-c  (list-ref l i))(set! bal (cadr it-c))
                          ;(print it-c)     (print bal)(newline)
   
                          (cond

                            ((and ( equal? (car it-c)"ket") (equal?  bal '("INTJ:disagree")))
                             (begin (set! Tp (cons (list (car it-c)'( "NEG"))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                                  
                            ((not(null? (lset-intersection equal?  bal' ("CONJ:caus"))))
                             (begin (set! Tp (cons (list (car it-c)'(  S-CONJ))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                                ((not(null? (lset-intersection equal?  bal' ("CONJ:coord"))))
                             (begin (set! Tp (cons (list (car it-c)'(  C-CONJ))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))

                            ((not(null? (lset-intersection equal? (flatten bal)' ("ADV:modal" "adv.""ADV:manner""Adv-Man"))))
                             (begin (set! Tp (cons (list (car it-c)'(  ADV))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                           

                            ((not(null? (lset-intersection equal?  bal' ("Prt-v""Prt-V-neg"))))
                             (begin (set! Tp (cons (list (car it-c)'(  PART))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                        

                            ((not(null? (lset-intersection equal?  bal '("adj.""Adj"))))
                             (begin (set! Tp (cons (list (car it-c)'(  ADJ))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))

                            ((not(null? (lset-intersection equal? (flatten bal ) '("VB:bezañ" "VB:kaout" "boe"))))
                             (begin (set! Tp (cons (list (car it-c)'(  AUX))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                            
                             
                                 
                            ((not(null? (lset-intersection equal? (flatten bal)' ("m.""f."("f.")("m.")"N-pl." "N-def"))))
                             (begin (set! Tp (cons (list (car it-c)'(  NOUN))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))

                            ((not(null? (lset-intersection equal?  bal' ("Art-def.""Art-i"))))
                             (begin (set! Tp (cons (list (car it-c)'(  DET))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
               
                            
                          
                             ((not(null? (lset-intersection equal? (flatten  bal)' ("pro-i.""PRP:int" ))))
                             (begin (set! Tp (cons (list (car it-c)'(  PRO))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))

                            ((member  "PRON:pers + PRON:obj" bal )
                             (begin (set! Tp (cons (list (car it-c)'(  PRO+PRO))Tp))                            
                                    (set! i (+ i 1))
                                    (loop)))
                          
                          

                             ((not(null? (lset-intersection equal? (flatten  bal)' ("v." "V-cj-e" "V-cj-a" "ppast" "part-p." "v-bez"))))
                              (begin (set! Tp (cons (list (car it-c)'(  VERB))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
                             ((not(null? (lset-intersection equal? (flatten  bal)' ("INTJ:disagree" ))))
                              (begin (set! Tp (cons (list (car it-c)'(  INTJ))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))

                             ((equal? bal'("V-pp" 'B10))
                              (begin (set! Tp (cons (list (car it-c)'(  VERB))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
  
                             ((equal? bal ' ("NPR"))
                              (begin (set! Tp (cons (list (car it-c)' ( PROPN))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
                       
                             ((not(null? (lset-intersection equal? (flatten  bal)' ("prép.""PREP:simple" "Prep"  ))))
                              (begin (set! Tp (cons (list (car it-c)'(  ADP))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
                             ((not(null? (lset-intersection equal?  bal' ("da" ))))
                              (begin (set! Tp (cons (list (car it-c)'(  ADP))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
  ((and (equal? "COMP" (car (cadr it-c)))
                                          (equal? "adv"(last (flatten(cadr it-c))))) 
                                (begin (set! Tp (cons (list (car it-c)'(ADV))Tp))                            
                                       (set! i (+ i 1))
                                    (loop))) 


    
                             ((equal? bal '("pct"))
                              (begin (set! Tp (cons (list (car it-c)'(  PUNCT))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))

                             ((equal? (car it-c )"eizh")
                              (begin (set! Tp (cons (list (car it-c)'(  NUM))Tp))                            
                                     (set! i (+ i 1))
                                     (loop)))
                          
                           
                             ((begin (set! Tp (cons it-c Tp))                            
                                     (set! i (+ i 1))
                                     (loop))))))))) 

                                
             (loop))))

  (define  (tag-correctif l )
    (let* ((i 0)
           (Tp '(("zyz"("EE")) ))
           (Tp-succes '())
           (Lg (length l))(it-p "")(it-s '("zyz"("FF")))(bal-it-p '())(bal-it-c '()) (bal-it-s '())

           (it-c "")(it-c1 ""))

     ; (print "list de depart")(newline) (print l)(newline)
      (letrec ((loop (lambda ()
                        (if(= Lg (+ 1 i))
                         (cdr (reverse (cons it-s Tp)))
                          (begin                                                                   
                            (set! it-c  (list-ref l i)) (set! it-p (car Tp)) (set! it-s (list-ref l (+ 1 i)))
                            (set! bal-it-p (cadr it-p)) (set! bal-it-c (cadr it-c)) (set! bal-it-s (cdr it-s))
                          
                            (cond

                              ((and(equal? (cadr it-p  ) '(PART))(equal? (cadr it-c)  '("UNKN"))(equal? (car it-s)  "," ))
                               (begin
                                 (set! Tp (cddr Tp))
                                 (set! Tp (cons  (list (car it-p) '(DET ))  Tp))
                                 (set! Tp (cons  (list (car it-c) '(NOUN ))  Tp))
                                 (set! Tp (cons  (list (car it-s) '(PUNCT ))  Tp))
                                 (set! i (+ i 2))
                                 (loop)))

                              ;'(("hag" ("adv." "conj.") B9) ("a" (ADP)) ("zegas" (VERB)))
                              ((and(equal? (cadr it-p  ) '("adv." "conj."))(equal? (car it-c) "a")
                                   (equal? (cadr it-s) '(VERB)))
                               (begin
                                 (set! Tp (cdr Tp))
                                 (set! Tp (cons  (list (car it-p) '(SCONJ  ))  Tp))
                                 (set! Tp (cons  (list (car it-c) '(PART  ))  Tp))
                                 (set! Tp (cons  (list (car it-s) '(VERB  ))  Tp))
                                 (set! i (+ i 2))
                                 (loop)))
                              ((and(equal? (cadr it-p  ) '("adv." "conj."))(equal? (cadr it-c) '(ADJ));  ("hag" ("adv." "conj.") B9)
                                   (equal? (cadr it-s)'(ADJ)))
                               (begin
                                 (set! Tp (cdr Tp))
                                 (set! Tp (cons  (list (car it-p) '(ADJ  ))  Tp))
                                 (set! Tp (cons  (list (car it-c) '(CCONJ))  Tp))
                                 (set! Tp (cons  (list (car it-s) '(ADJ  ))  Tp))
                                 (set! i (+ i 2))
                                 (loop))) ;WORK
     ((and (equal? (car it-p) "e" )
                                  (>(string-length (car it-c ))3)
                                  (equal? (string-take-right (car it-c) 2)"as"))                               
                             (begin
                                 (set! Tp (cdr Tp))
                                 (set! Tp (cons (list (car it-p )'(PART)) Tp))
                                      (set! Tp (cons (list (car it-c )'(VERB)) Tp))
                             
                               (set! i (+ i 1)) 
                               (loop)))

                                 ((and(equal? (car it-p  ) "e" )
                                      (and (> (string-length (car it-c))2)(equal?(string-take-right (car it-c) 1) "e" )))                               
                                  (begin
                                    (set! Tp (cdr Tp))
                                    (set! Tp (cons  (list (car it-p) '(PRO))  Tp))
                                    (set! Tp (cons  (list (car it-c) '(VERB ))  Tp))
                                    (set! i (+ i 1))
                                 (loop)))
   
    ((and(equal? (car it-p  ) "hep" )(equal? (car it-c) "ma"))                                 
                                  (begin
                                    (set! Tp (cons  (list (car it-c) '(SCONJ  ))  Tp))
                                    ;(set! Tp (cons  (list (car it-s) '(VERB A1))  Tp))
                                    (set! i (+ i 1))
                                 (loop)))


                              ((and(equal? (cadr it-p  ) '( "conj.""adv."))(equal? (cadr it-c) '(NOUN)))
                               (begin
                                 (set! Tp (cdr Tp))
                                 (set! Tp (cons  (list (car it-p) '(ADV  ))  Tp))
                                 (set! Tp (cons  (list (car it-c) '(NOUN  ))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                              ((and(equal? (cadr it-p  )'(ADP))
                                   (equal? (cadr it-c)  '(ADJ))
                                   (equal? (cadr it-s)  '(NOUN)))
                               (begin
                                 ;(set! Tp (cddr Tp))
                                 (set! Tp (cons  (list (car it-c) '(NUM  ))  Tp))
                                 (set! Tp (cons  (list (car it-s) '(NOUN ))  Tp))
                                 (set! i (+ i 2))
                                 (loop)))
  
                              ((and(equal? (car it-p  )"e")

                                   (equal? (cadr it-c)  '(NOUN)))
                               (begin
                                 (set! Tp (cdr Tp))
                                 (set! Tp (cons  (list (car it-p) '(DET  ))  Tp))
                                 (set! Tp (cons  (list (car it-c) '(NOUN D))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                              ((equal? (cadr it-c  ) '(NOUN D))
                                  (begin (set! Tp (cons  (list (car it-c) '(NOUN))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                 ((equal? (cadr it-c  ) '(NOUN D))
                                  (begin (set! Tp (cons  (list (car it-c) '(NOUN))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                  ((equal? it-c '("a" ("Prt-v-a") B12))
                                      (begin (set! Tp (cons  (list (car it-c) '(PART))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                    ((member "ADV:int" (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(ADV))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                      ((member "Prt-v-a" (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(PART))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                          ((member "Prt-v-o" (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(PART))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                               ((member "V-inf" (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(VERB))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                                      ((member 'B11 (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(ADP))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                                                        ((member 'B11 (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(ADP))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))

                                                     
                                                                      ((member '("Poss" 'A7) it-c )
                                        (begin (set! Tp (cons  (list (car it-c) '(DET))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                                                                                        ((member "où" (flatten  it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(N))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                                                                                   ((member 'D (flatten it-c ))
                                        (begin (set! Tp (cons  (list (car it-c) '(NOUN))  Tp))
                                 (set! i (+ i 1))
                                 (loop)))
                                     

                              ((begin
                                 (set! Tp (cons   it-c   Tp))              
                                 (set! i (+ i 1))
                                 (loop)))))))))
        (loop))))



  (define stage1 (map tag-k2  (map tag-k1 (it->seq (Str->m T-k1 )))))

  (define stage2 (map tag-correctif  (map normalize stage1)))

 (car stage2)
