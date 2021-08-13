(setq svg-scratch (svg-create 100 100))
(svg-rectangle svg-scratch 25 25 50 50 :id "a")
(svg-line svg-scratch 25 25 75 75 :id "b" :stroke-color "black")

;; (svg-remove svg-scratch "a")

(insert-image (svg-image (append svg-scratch (nthcdr 2 svg-labels))))
