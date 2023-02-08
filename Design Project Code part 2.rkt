#lang racket
(require graph)
(require racket/trace)
(require racket/gui/base)

( define vertices '( A B C D E ) )
( define edges ' ( ( A B ) ( B A )
                 ( B C ) ( C B ) ;line 1 

                 ( C D ) ( D C ) ;line 2 

                 ( C E ) ( E C ) ) ) ; line 3

(define edge (lambda ( x graph )
   ( map ( lambda ( y ) ( car ( cdr y ) ) ) ) ( filter ( lambda ( y )
                 ( = ( car y ) x ) ) graph ) ) )

( define path ( lambda ( x y graph vertex-set )
               ; (define vertex-set ( edges x graph))  ;;;;
                 (println x)
                 (println graph)
                 ( cond
                    ((equal? x y) "valid") 
                    ;((equal? (last vertex-set) y) vertex-set) ;;;;
                   ; [(and = x y ) #t  ]
                    [( not ( set-member? vertex-set x ) ) #f ]
                    [( not ( set-member? vertex-set y ) ) #f] 
                    ( #t ( ormap ( lambda ( z ) ( path z y graph ( set-remove vertex-set x ) ) )( edge x graph ) ) )
                    ) ) )

( define reachable ( lambda ( x y a-graph)
                      ( path x y a-graph ( list->set ( flatten a-graph ) ) ) ) )

( define graph '(  (A B) (B A) (B C) (C B) (C D) (D C) (C E) (E C)  ))

( define line1 ( weighted-graph/directed '( ( 1  A B ) ( 1  B A ) ( 1.5  B C ) ( 1.5 C B ) ( 2 C D ) ( 2 D C )  ( 2.5 C E ) ( 2.5 E C )  )) ) 
;( define line2 ( weighted-graph/directed '( ( 2 C D ) ( 2 D C ) ) )) 
;( define line3 ( weighted-graph/directed '( ( 2.5 C E ) ( 2.5 E C ) ) ) )

 ( graph? line1 )

; ( graph? line2 )

;( graph? line3 )

;has-edge

( has-edge? line1 'A 'B )  ;;Line1
( has-edge? line1 'B 'A )
( has-edge? line1 'B 'C )
( has-edge? line1 'C 'B )
( has-edge? line1 'C 'D )  
( has-edge? line1 'D 'C )
( has-edge? line1 'C 'E )  
( has-edge? line1 'E 'C 

;( has-edge? line2 'C 'D )  ;;Line2
;( has-edge? line2 'D 'C )

;( has-edge? line3 'C 'E )  ;;Line3
;( has-edge? line3 'E 'C )

( edge-weight line1 'A 'B ) ;;Line1
( edge-weight line1 'B 'A )
( edge-weight line1 'B 'C )
( edge-weight line1 'C 'B )
( edge-weight line1 'C 'D ) 
( edge-weight line1 'D 'C )
( edge-weight line1 'C 'E ) 
( edge-weight line1 'E 'C )



;( edge-weight line2 'C 'D ) ;;Line2
;( edge-weight line2 'D 'C )

;( edge-weight line3 'C 'E ) ;;Line3
;( edge-weight line3 'E 'C )









;; GUI

( define myframe ( new frame%
                       [ label "Underground App" ]
                       [ width 400] [ height 600] ) )

 (define globalmessage (new message% [label "Downbelow: Please enter your start and end destination \n when done select 'Confirm' to view your journey.\n"] [parent myframe]
                           ))




( define text-field ( new text-field%
                          [label "Start"] [ parent myframe] (init-value "Please Enter The Station Name for Start...")))
( define text-field1 ( new text-field%
                          [label "Destination" ] [ parent myframe] (init-value "Please Enter The Station Name for Destination...")))

(define myframe-center (new vertical-panel% [alignment '(center center)] [parent myframe]  ) )

( define panel ( new horizontal-panel%
                 [parent myframe]))


(define horizontalpanel-data (new horizontal-panel% [parent myframe-center][alignment '(center top)] [min-width 200][min-height 50] ) )

(define verticalpanel-data (new vertical-panel% [parent myframe][alignment '(center top)][min-width 200][min-height 200]  ) )


( new button% [parent horizontalpanel-data ]
      [ label "Clear"]
      [callback ( lambda ( o e )
                   (send text-field set-value "")(send text-field1 set-value ""  ))])

( new button% [parent horizontalpanel-data]
      [ label "Switch" ]
      [callback ( lambda ( o e  )
                   ( send text-field set-value "")(send text-field1 set-value "" ))])

( new button% [parent horizontalpanel-data ]
      [ label "Confirm"]
      [callback ( lambda ( o e  )
                   (send text-field set-value "") (send text-field1 set-value "" ))])





(define GIU-1 (new message% [label "The Travel Planner Data Of Stations:"] [min-width 200] [min-height 600][parent panel]))

(define GUI-2 (new message% [label "Start location\n->Stops\n->Destination location"] [parent panel] [min-width 200][min-height 500]))

(send myframe show #t)
