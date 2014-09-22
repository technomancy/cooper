;; sample stack file
[("first" [(set-brush "green" solid)
           (draw-rectangle 5 10 90 20)
           (draw-line 60 60 100 100)]
  [([5 10 95 30] "second")])
 ("second" [(set-brush "blue" solid)
            (draw-ellipse 10 10 50 50)]
  [([10 10 60 60] "first")])]
