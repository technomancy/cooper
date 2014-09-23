;; sample stack file
#s(stack
    "mystack"
    #hash(("first"
           .
           #s(card
              "first"
              ((set-brush "green" solid) (draw-rectangle 5 10 90 20) (draw-line 60 60 100 100))
              (#s(button (5 10 95 30) "second"))))
          ("second"
           .
           #s(card
              "second"
              ((set-brush "blue" solid) (draw-ellipse 10 10 50 50))
              (#s(button (10 10 60 60) "first")))))
    800
    600)