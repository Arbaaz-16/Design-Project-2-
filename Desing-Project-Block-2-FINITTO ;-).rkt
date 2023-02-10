#lang racket
(require racket/gui)
(require graph)
(require racket/gui/base)
;------------------------------------------------------------------------- Graphic User Interface ---------------------------------------------------------;

( define myframe ( new frame%
                       [ label "Underground App" ]
                       [ width 250] [ height 400] ) )
                           
( send myframe show #t)

(define pan (new vertical-panel%
                 [parent myframe]))
  
(define start (new text-field%
                      [label "Select Starting Station" ] [ parent myframe]
                      [init-value "Your Starting Point"]))

(define end(new text-field%
                     [ label "Select Destination Station" ] [parent myframe]
                     [init-value "Your Destionation Please"]))

(define ShowRoute (new button%
                       [parent myframe]
                       [label "Confirm Route"]
                       [vert-margin 10]	 
                       [horiz-margin 50]	 
                       [min-width 30]	 
                       [min-height 30]
                       [callback (lambda (o e)
                                   (send myframe show #t)
                                   (define a-var (send start get-value ))
                                   (define b-var (send end get-value ))
                                   (define reachable-2 ( reachable a-var b-var g ) )
                                   (println(string-join reachable-2 "----") ) (println reachable-2)
                                   (send GUI-1 set-label "Your Journey Adventure:" )
                                   (send GUI-1 set-label (string-join reachable-2 "\n----")))]))

( define panel ( new horizontal-panel%
                     [parent myframe]))
(define GUI-1 (new message% [label "Your Route:"] [min-width 200] [min-height 400][parent panel]))

(define GUI-2 (new message% [label "Starting Point\n->Stops\n->Final Destination"] [parent panel] [min-width 200][min-height 300]))

;------------------------------------ TRY TO FIX THE GUI HOW IT LOOKS LIKE -------------------------------------------------------------------------;
(define myframe-center (new vertical-panel% [alignment '(center center)] [parent myframe]  ) )

(define horizontalpanel-data (new horizontal-panel% [parent myframe-center][alignment '(center top)] [min-width 200][min-height 50] ) )

(define verticalpanel-data (new vertical-panel% [parent myframe][alignment '(center top)][min-width 200][min-height 200]  ) )


;-------------------------------------------------------------------FUNCTION--------------------------------------------------------------------------;
(define g '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "d") ("d" "c") ("c" "b") ("b" "a" )))

(define graphnodes '( "a" "b" "c" "d" "e"))

(define edge (λ (x graph) (map (λ (y) (first(rest y))) (filter(λ (y) (equal? (car y) x)) graph ))))

(define path (λ (x y graph nodeset list-2)
               (define list-1 (append-lists x list-2))
               (cond
                 ((equal? (last list-1) y) list-1)
                 ((not (set-member? nodeset x)) #f)
                 ((not (set-member? nodeset y)) #f)
                 ((equal? x y) #t)                 
                 (#t (ormap (λ (z) (path z y graph (set-remove nodeset x) list-1)) (edge x graph))))))

(define append-lists (λ (x ll)
                       (append ll (list x))))
                                             
(define reachable (λ (x y graph )
                    (define flat-graph (list->set (flatten graph)))
                    (define list2 (list ))
                    (define returnthis (path x y g flat-graph list2))
                    returnthis
                    (cond
                      ((boolean? returnthis) (list "Make sure you have your correct stations"))
                      (#t returnthis))))
                    
                    

