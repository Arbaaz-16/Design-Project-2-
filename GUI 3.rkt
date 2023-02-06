#lang racket/gui
( define myframe ( new frame%
                       [ label "Underground App" ]
                       [ width 300] [ height 450] ) )
( send myframe show #t)
( define text-field ( new text-field%
                          [label "Start"] [ parent myframe][init-value ""]))
( define text-field1 ( new text-field%
                          [label "Destination" ] [ parent myframe]))
( define panel ( new horizontal-panel%
                 [parent myframe]))
( new button% [parent panel ]
      [ label "Clear"]
      [callback ( lambda ( o e )
                   (send text-field set-value "")(send text-field1 set-value ""))])
( new button% [parent panel]
      [ label "Switch" ]
      [callback ( lambda ( o e  )
                   ( send text-field set-value "")(send text-field1 set-value "" ))])
( new button% [parent panel ]
      [ label "Confirm"]
      [callback ( lambda ( o e  )
                   (send text-field set-value "") (send text-field1 set-value "" ))])