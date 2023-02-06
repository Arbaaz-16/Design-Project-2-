#lang racket
(require graph)

(define  a 1)
(define  b 2)
(define  c 3)
(define  d 4)
(define  e 5)



(define g (weighted-graph/directed '((2.5 a b) (5 b a) (10 b c) (15 c b) (5 c d) (5 d c) (5 c e) (5 e  c))))


( define edge ( lambda ( x weighted-graph/directed )
                 ( map ( lambda ( y )( first ( rest y ))) ( filter ( lambda ( y )
                                                                      (= ( first y ) x )) weighted-graph/directed ))))


( define path ( lambda ( x y a-graph vertex-set )
                 ( cond
                    ((= x y ) #t )
                    (( not ( set-member? vertex-set x )) #f )
                    (( not ( set-member? vertex-set y )) #f )
                    (#t ( ormap ( lambda ( z )( path z y a-graph
                                                     ( set-remove vertex-set x )))
                                ( edge x a-graph )))
                    )))


( define reachable ( lambda ( x y a-graph )                     
                      ( path x y a-graph ( list->set ( flatten a-graph )))))

