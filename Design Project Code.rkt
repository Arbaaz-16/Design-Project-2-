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
( define path ( lambda ( x y a-graph vertex-set )
                 ( cond
                    [(and = x y ) #t  ]
                    [( not ( set-member? vertex-set x ) ) #f ]
                    [( not ( set-member? vertex-set y ) ) #f]
                    [ #t ( ormap ( lambda ( z ) ( path z y a-graph ( set-remove vertex-set x ) ) )
                                 ( edges x a-graph ) ) ]
                    ) ) ) 
( define reachable ( lambda ( x y a-graph)
                      ( path x y a-graph ( list->set ( flatten a-graph ) ) ) ) )

( define graph '(  (A B) (B A) (B C) (C B) (C D) (D C) (C E) (E C)  ))

( define line1 ( weighted-graph/directed '( ( 1  A B ) ( 1  B A ) ( 1.5  B C ) ( 1.5 C B ) ) )) 
( define line2 ( weighted-graph/directed '( ( 2 C D ) ( 2 D C ) ) )) 
( define line3 ( weighted-graph/directed '( ( 2.5 C E ) ( 2.5 E C ) ) ) )

 ( graph? line1 )

 ( graph? line2 )

( graph? line3 )

;has-edge

( has-edge? line1 'A 'B )  ;;Line1
( has-edge? line1 'B 'A )
( has-edge? line1 'B 'C )
( has-edge? line1 'C 'B )

( has-edge? line2 'C 'D )  ;;Line2
( has-edge? line2 'D 'C )

( has-edge? line3 'C 'E )  ;;Line3
( has-edge? line3 'E 'C )

( edge-weight line1 'A 'B ) ;;Line1
( edge-weight line1 'B 'A )
( edge-weight line1 'B 'C )
( edge-weight line1 'C 'B )

( edge-weight line2 'C 'D ) ;;Line2
( edge-weight line2 'D 'C )

( edge-weight line3 'C 'E ) ;;Line3
( edge-weight line3 'E 'C )