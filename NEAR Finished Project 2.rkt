#lang racket
(require racket/gui)
(require graph)
(require racket/gui/base)

( define myframe ( new frame%
                       [ label "Underground App" ]
                       [ width 250] [ height 400] ) )


(define globalmessage (new message% [label "Downbelow: Please enter your start and end destination \n when done select 'Confirm' to view your journey.\n"] [parent myframe]
                           ))



( send myframe show #t)

(define pan (new vertical-panel%
                 [parent myframe]))

(define check-box (new check-box%
                       [label "A-E-line"] [parent pan]
                       [value #t]))


;-------------------------------------------------------------------------------------------------NEEDS FIXNG!!!!!!-------------------------------------------------------------------------------------------  
(define choice-1 (new choice%
                      [label "Select Destination Station" ] [ parent myframe]
                      [ choices ( list "a" "b" "c" "d" "e") ] ))

(define choice-2(new choice%
                     [ label "Select Starting Station" ] [parent myframe]
                     [choices ( list "a"  "b" "c" "d" "e") ] ))
(define ShowRoute (new button%
                       [parent myframe]
                       [label "Confirm Route"]
                       [vert-margin 10]	 
                       [horiz-margin 50]	 
                       [min-width 30]	 
                       [min-height 30]
                       [callback (lambda (o e)
                                   (send myframe2 show #t)

                                   (define a-var (send choice-1 get-value))
                                   (define b-var (send choice-2 get-value))




                                   
                                   (send text-output set-value (reachable a-var b-var g)))]))

 
;-------------------------------------------------------------------------------------------------NEEDS FIXNG!!!!!!-------------------------------------------------------------------------------------------  

( define myframe2 (new frame%
                       [ label "Route"]
                       [ width 200] [height 300]))

( define text-output ( new text-field% 
                           [parent myframe2 ]
                           [ label "Train Route" ]
                           [ min-width 150]
                           [min-height 150]
                           [vert-margin 10]
                           [horiz-margin 10 ]
                           [stretchable-width #t ]
                           [stretchable-height #t]))

(define myframe-center (new vertical-panel% [alignment '(center center)] [parent myframe]  ) )

( define panel ( new horizontal-panel%
                     [parent myframe]))

(define horizontalpanel-data (new horizontal-panel% [parent myframe-center][alignment '(center top)] [min-width 200][min-height 50] ) )

(define verticalpanel-data (new vertical-panel% [parent myframe][alignment '(center top)][min-width 200][min-height 200]  ) )





(define GUI-1 (new message% [label "The Travel Planner Data Of Stations:"] [min-width 200] [min-height 400][parent panel]))

(define GUI-2 (new message% [label "Start location\n->Stops\n->Destination location"] [parent panel] [min-width 200][min-height 300]))






























;----------------------------------------------------------------------------------------------------------------------------;
(define g '(("a" "b") ("b" "c") ("c" "d") ("d" "e") ("e" "d") ("d" "c") ("c" "b") ("b" "a" )))

(define graphnodes '( "a" "b" "c" "d" "e"))

(define edge (λ (x graph) (map (λ (y) (first(rest y))) (filter(λ (y) (equal? (car y) x)) graph ))))

(define path (λ (x y graph nodeset templist2) ; path recursively checks vertices connected through edges looking for a path
               (define templist1 (append-lists x templist2))
               (cond
                 ((equal? (last templist1) y) templist1)
                 ((not (set-member? nodeset x)) #f)
                 ((not (set-member? nodeset y)) #f)
                 ((equal? x y) #t)                 
                 (#t (ormap (λ (z) (path z y graph (set-remove nodeset x) templist1)) (edge x graph))))))

(define append-lists (λ (x ll)
                       (append ll (list x))
                       
                       ))



(define reachable (λ (x y graph )
                    (define flat-graph (list->set (flatten graph)))
                    (define templist2 (list ))
                    (define returnthis (path x y g flat-graph templist2))
                    returnthis
                    (cond
                      ((boolean? returnthis) (list "Please check your station names are correctly spelt, then press the 'Confirm' button"))
                      (#t returnthis)
                      )
                    ))

