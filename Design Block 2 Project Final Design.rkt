#lang racket
(require racket/gui)
(require graph)
(require racket/gui/base)
;------------------------------------------------------------------------- Graphic User Interface ---------------------------------------------------------;
;this The frame measures 250 pixels wide by 200 pixels height and features a caption that reads "Underground App."

( define myframe ( new frame%
                       [ label "Underground App" ]
                       [width 200][height 200]
                       [stretchable-width #t]
                       [stretchable-height #t]))
                           
(define pan (new vertical-panel%
                 [parent myframe]))

;this is the panel belongs to the larger "myframe" user interface element because "myframe" is the panel's parent.
;The developer can construct sophisticated user interfaces with several elements and sections by using the vertical panel to organise elements vertically, one above the other.
;The development of a vertical panel allows the developer to control the arrangement and look of linked user interface elements as a unified unit.
;For instance, they might put text fields, buttons, labels, or other user interface components on the vertical panel.
  
(define start (new text-field%
                      [label "Your Location" ] [ parent myframe]
                      [init-value "Type your Starting Point"]
                      [min-width 15]
                      [min-height 15]
                      [vert-margin 10]
                      [horiz-margin 10]))
;The text field's default entry is " Type Your Starting Point."
;This is the text that will appear in the text field prior to the user entering any information.
;The initial value is often used as a placeholder to ask the user for the necessary data.

(define end(new text-field%
                     [ label "Your Destination" ] [parent myframe]
                     [init-value "Type your Destionation "]
                     [min-width 15]
                     [min-height 15]
                     [vert-margin 10]
                     [horiz-margin 10]))

;This The text field's default entry is " Type Your Destination."
;This is the text that will appear in the text field prior to the user entering any information.
;Usually, the initial value serves as a placeholder to ask the user for the necessary data.

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
                                   (define reachable-2 ( reachable a-var b-var Northernline  ) )
                                   (println(string-join reachable-2 "----") ) (println reachable-2)
                                   (send GUI-1 set-label "Your Journey Adventure:" )
                                   (send GUI-1 set-label (string-join reachable-2 "\n----")))]))

;This defines the "ShowRoute" button, a new button. The button has a callback function that is activated when it is clicked,
;along with other attributes like its parent, label, margins, and size.
;myframe is the button's parent. Confirm Route is the button's label. The margins on the button are 10 pixels vertically and 50 pixels horizontally.
;The button must be at least 30 pixels wide and 30 pixels tall.
;When the button is clicked, the callback function executes a number of operations. The "myframe" is first prepared to be displayed.
;The values of "start" and "end" are then obtained and correspondingly stored in the variables "a-var" and "b-var." The "g" graph's reachability of "a-var" and "b-var" is identified and recorded.

( new button% [parent myframe]
      [ label "Clear"]
      [callback ( lambda ( o e )
                   (send start set-value "")(send end set-value ""))])
;this is a function callback clear button which it will clear the text-field from both inputs start and destination,
;when a user click on it. 


( define panel ( new horizontal-panel%
                     [parent myframe]))
(define GUI-1 (new message% [label "Your Route:"][min-width 200]
                            [min-height 400][parent panel]))
(define GUI-2 (new message% [label "Starting Point\n->Stops\n->Final Destination"]
                            [parent panel] [min-width 200][min-height 300]))

(define myframe-center (new vertical-panel% [alignment '(center center)] [parent myframe]))

(define horizontalpanel-data (new horizontal-panel% [parent myframe-center]
                                                    [alignment '(center top)]
                                                    [min-width 200][min-height 50]))

(define verticalpanel-data (new vertical-panel% [parent myframe]
                           [alignment '(center top)]
                           [min-width 200][min-height 200]))


;-------------------------------------------------------------------FUNCTION--------------------------------------------------------------------------;

(define Northernline  '(("Chalk Farm" "Camden Town") ("Camden Town" "Euston") ("Euston" "King’s Cross St. Pancras") ("King’s Cross St. Pancras" "Warren Street")
                                         ("Warren Street" "King’s Cross St. Pancras") ("King’s Cross St. Pancras" "Euston") ("Euston" "Camden Town")
                                         ("Camden Town" "Chalk Farm" )))
;this is  is a list of tuples, where each tuple consists of two strings that represent two adjacent locations in a graph.
;The locations in the graph are:

;PRESENTING THE WAYS 

;"Chalk Farm"------"Camden Town"
;"Camden Town"------"Euston"
;"Euston"--------"King's Cross St. Pancras"
;"King's Cross St. Pancras"-------"Warren Street"
;"Warren Street"-----------"King's Cross St. Pancras"
;"King's Cross St. Pancras"------ "Euston"
;"Euston"-----------"Camden Town"
;"Camden Town"------"Chalk Farm"

(define graphnodes '( "Chalk Farm" "Camden Town" "Euston" "King’s Cross St. Pancras" "Warren Street"))

;This graphnodes is a list of strings that represents the nodes or vertices of a graph, specifically:
;"Chalk Farm"
;"Camden Town"
;"Euston"
;"King's Cross St. Pancras"
;"Warren Street" 

(define edge (λ (x graph) (map (λ (y) (first(rest y))) (filter(λ (y) (equal? (car y) x)) graph ))))

;This is a function edge takes two arguments, x and graph, filters the graph to keep only the sublists whose first element is equal to x,
;And returns a new list consisting of the second elements of these sublists.
;This new list represents the edges or connections in the graph that are connected to the node represented by x.

(define path (λ (x y graph nodeset list-2)
               (define list-1 (append-lists x list-2))
               (cond
                 ((equal? (last list-1) y) list-1)
                 ((not (set-member? nodeset x)) #f)
                 ((not (set-member? nodeset y)) #f)
                 ((equal? x y) #t)                 
                 (#t (ormap (λ (z) (path z y graph (set-remove nodeset x) list-1)) (edge x graph))))))
;the function path takes five arguments, x, y, graph, nodeset, and list-2, and returns a list representing a path from x to y in the graph.
;if one exists, or #f if one doesn't exist.
;The function uses recursion and a set data structure to keep track of the visited nodes and ensure that no node is visited more than once.


(define append-lists (λ (x ll)
                       (append ll (list x))))
; This function add-lists returns the value of (append ll (list x)) and accepts the two variable x and ll.

;A singleton list containing the single entry x is produced by the function (list x).
;The concatenation of the two input lists is returned by the append function, which accepts two lists as inputs.
;As a result, the definition (define append-lists (x ll) (append ll (list x))) generates a new function called append-lists that accepts the parameters x and ll and returns a new list that is the result of concatenating ll with a singleton list that contains x.
                                             
(define reachable (λ (x y graph )
                    (define flat-graph (list->set (flatten graph)))
                    (define list2 (list ))
                    (define returnthis (path x y Northernline  flat-graph list2))
                    returnthis
                    (cond
                   ((boolean? returnthis) (list "Make sure you have your correct stations"))
                      (#t returnthis))))
;The function reachable takes in three arguments: x, y, and graph, flattens the graph and converts the result to a set, defines an empty list list2, and calls the function path with the provided arguments.
;The result of this call is then stored in the variable returnthis.
;If the value of returnthis is a boolean, the function returns the string "MAKE SURE YOU HAVE THE CORRECT STATION", otherwise it Will not work.

( send myframe show #t)