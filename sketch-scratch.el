(setq svg-scratch (svg-create 100 100))
;; ((svg-path svg-scratch '((moveto ((0 . 0)))
;;                  (elliptical-arc ((75 75 100 100)))))
;; (svg-rectangle svg-scratch 25 25 50 50 :id "a")
;; (svg-line svg-scratch 25 25 75 75 :id "b" :stroke-color "black")

;; (svg-remove svg-scratch "a")

;; (insert-image (svg-image (append svg-scratch (nthcdr 2 svg-labels))))


;; (setq svg (svg-create 400 400))
;; (svg-rectangle svg 0 0 100 100 :stroke "red")

;; (setq sketch (dom-node 'g '((transform . "translate(100 100)"))))
;; (svg-rectangle sketch 0 0 100 100 :stroke "yellow")

;; (dom-append-child svg sketch)

(defun sketch-coords-sum (coords-1 coords-2)
  (cons (+ (car coords-1) (car coords-2))
        (+ (cdr coords-1) (cdr coords-2))))

(defun sketch-coords-diff (coords-1 coords-2)
  (cons (- (car coords-1) (car coords-2))
        (- (cdr coords-1) (cdr coords-2))))

(defun sketch-coords-scale (coords factor)
  (cons (* factor (car coords))
        (* factor (cdr coords))))

(defun sketch-coords-cross (coords-1 coords-2)
  (- (* (car coords-1) (cdr coords-2))
     (* (cdr coords-1) (car coords-2))))

(defun sketch-line-start (coords)
  (cons (nth 0 coords) (nth 1 coords)))

(defun sketch-line-end (coords)
  (cons (nth 2 coords) (nth 3 coords)))

(defun sketch-lines-intersection (coords-1 coords-2)
  (let* ((p (sketch-line-start coords-1))
         (q (sketch-line-start coords-2))
         (r (sketch-coords-diff (sketch-line-end coords-1) p))
         (s (sketch-coords-diff (sketch-line-end coords-2) q))
         (u (/ (float (sketch-coords-cross (sketch-coords-diff q p) r))
               (sketch-coords-cross r s))))
    (sketch-coords-sum q (sketch-coords-scale s u))))

(defun sketch-lines-angle-arc (line1-coords line2-coords arc-radius)
  "Draw angle arc in positive angle direction."
  (let* ((intersection (sketch-lines-intersection line1-coords line2-coords))
         (slope1-angle (atan (/ (- (nth 3 line1-coords) (nth 1 line1-coords))
                          (- (nth 2 line1-coords) (nth 0 line1-coords)))))
         (slope2-angle (atan (/ (- (nth 3 line2-coords) (nth 1 line2-coords))
                          (- (nth 2 line2-coords) (nth 0 line2-coords)))))
         (start-angle (min slope1-angle slope2-angle))
         (stop-angle (max slope1-angle slope2-angle))
         (start (cons (+ (car intersection) (* arc-radius (cos start-angle)))
                      (+ (cdr intersection) (* arc-radius (sin start-angle)))))
         (stop (cons (+ (car intersection) (* arc-radius (cos stop-angle)))
                      (+ (cdr intersection) (* arc-radius (sin stop-angle))))))
    (svg-path svg-scratch `((moveto (,start))
                            (elliptical-arc
                             ((,arc-radius ,arc-radius ,(car stop) ,(cdr stop) :sweep t))))
              :fill "transparent" :stroke "black")))

(defun sketch-add lines-angle-arc arc-radius)
(sketch-lines-angle-arc '(0 0 100 100) '(100 0 0 100) 10)


(insert-image (svg-image svg-scratch))
