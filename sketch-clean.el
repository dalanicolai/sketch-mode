(require 'shr-color)

;;; Rendering
(defvar sketch-svg nil)
(defvar sketch-size '(1200 . 900))
(defvar sketch-grid nil)
(defvar sketch-show-grid t)
(defvar sketch-background "white")
(defvar sketch-grid-param 50)
(defvar sketch-minor-grid-param nil)
(defvar sketch-minor-grid-freq 4)
(defvar sketch-grid-colors '("gray" . "gray"))
(defvar sketch-default-stroke "black")
(defvar sketch-snap-to-grid t)

(defvar sketch-canvas nil)
(defvar sketch-root nil)

(defvar sketch-active-layer 0)
(defvar sketch-layers-list nil)
(defvar show-layers nil)

(defvar sketch-show-labels nil)

(defun svg-marker (svg id width height &optional color reverse)
  "Define a marker with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (svg--def
   svg
   (apply
    #'dom-node
    'marker
    `((id . ,id)
      (viewBox . "0 0 10 10")
      (refX . 5)
      (refY . 5)
      ,(pcase id
         ("arrow" `(markerWidth . ,width))
         ("dot" `(markerWidth . ,width)))
      ,(pcase id
         ("arrow" `(markerHeight . ,height))
         ("dot" `(markerHeight . ,height)))
      ,(pcase id
         ;; ("arrow" '(orient . auto-start-reverse))))
         ("arrow" (if reverse
                      '(orient . auto)
                    '(orient . auto-start-reverse)))))
    (pcase id
      ("arrow" (list (dom-node 'path `((d . "M 0 0 L 10 5 L 0 10 z")
                                       (fill . ,(or color "black"))))))
      ("dot" (list (dom-node 'circle `((cx . 5)
                                       (cy . 5)
                                       (r . 5)
                                       (fill . ,(or color "black"))))))))))

(defun svg-group (&rest args)
  (apply #'dom-node
         'g
         `(,(svg--arguments nil args))))

(defun sketch-group (id &rest args)
  (apply #'svg-group
         :id id
         :transform "translate(0,0)"
         args))

(defun sketch-create (w h &optional scale pan-x pan-y &rest args)
  (let ((scale (or scale 1)))
    (apply #'svg-create w h :viewBox (format "%s %s %s %s"
                                             (or pan-x 0)
                                             (or pan-y 0)
                                             (/ (float w) scale)
                                             (/ (float h) scale))
           args)))

(defun sketch-image (svg &rest props)
  "Return an image object from SVG.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   (with-temp-buffer
     (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n")
     (svg-print svg)
     (buffer-string))
   'svg t props))

(defun sketch-insert-image (svg string &rest props)
  "Insert SVG as an image at point.
If the SVG is later changed, the image will also be updated."
  (let ((image (apply #'sketch-image svg props))
        (marker (point-marker)))
    (insert-image image string)
    (dom-set-attribute svg :image marker)))

(defun sketch-labels ()
  (interactive)
  (let ((nodes (pcase sketch-show-labels
                 ("layer" (dom-children (nth sketch-active-layer sketch-layers-list)))
                 ("all" (apply #'append (mapcar (lambda (l)
                                                  (dom-children (nth l sketch-layers-list)))
                                                show-layers)))))
        (svg-labels (sketch-group "labels")))
    (mapc (lambda (node)
            (pcase (dom-tag node)
              ('rect (svg-text svg-labels
                               (dom-attr node 'id)
                               :x (+ (dom-attr node 'x) 2)
                               :y (+ (dom-attr node 'y)
                                     (- (dom-attr node 'height) 2))
                               :font-size sketch-label-size
                               :stroke "red"
                               :fill "red"))
              ('line (svg-text svg-labels
                               (dom-attr node 'id)
                               :x (dom-attr node 'x1)
                               :y (dom-attr node 'y1)
                               :font-size sketch-label-size
                               :stroke "red"
                               :fill "red"))
              ((or 'circle 'ellipse) (svg-text svg-labels
                                               (dom-attr node 'id)
                                               :x (dom-attr node 'cx)
                                               :y (dom-attr node 'cy)
                                               :font-size sketch-label-size
                                               :stroke "red"
                                               :fill "red"))
              ((or 'polyline 'polygon) (let ((coords (split-string
                                                      (car (split-string (dom-attr node 'points) ","))
                                                      nil
                                                      t)))
                                         (svg-text svg-labels
                                                   (dom-attr node 'id)
                                                   :x (string-to-number (car coords))
                                                   :y (string-to-number (cadr coords))
                                                   :font-size sketch-label-size
                                                   :stroke "red"
                                                   :fill "red")))
              ('text (svg-text svg-labels
                               (dom-attr node 'id)
                               :x (dom-attr node 'x)
                               :y (+ (dom-attr node 'y)
                                     sketch-label-size)
                               :font-size sketch-label-size
                               :stroke "red"
                               :fill "red"))
              ('g (let ((s (dom-attr node
                                     'transform)))
                    (string-match "translate\(\\([0-9]*\\)[, ]*\\([0-9]*\\)" s)
                    (let ((x (match-string 1 s))
                          (y (match-string 2 s)))
                      (svg-text svg-labels
                                (dom-attr node 'id)
                                :x x
                                :y y
                                :font-size sketch-label-size
                                :stroke "red"
                                :fill "red"))))))
          nodes)
    svg-labels))


(defun sketch-labels-list ()
  (apply #'append (mapcar (lambda (l)
                            (mapcar (lambda (node)
                                      (dom-attr node 'id))
                                    (dom-children (nth l sketch-layers-list))))
                          show-layers)))
(defun sketch-create-label (type)
  (interactive)
  (let* ((prefix (concat (when (/= sketch-active-layer 0)
                           (number-to-string sketch-active-layer))
                         (pcase type
                           ('line "l")
                           ('rectangle "r")
                           ('circle "c")
                           ('ellipse "e")
                           ('polyline "p")
                           ('polygon "g")
                           ('freehand "f")
                           ('text "t")
                           ('group "g"))))
         (idx 0)
         (label (concat prefix (number-to-string idx)))
         (labels (sketch-labels-list)))
    (while (member label labels)
      (setq idx (1+ idx))
      (setq label (concat prefix (number-to-string idx))))
    label))

(defun sketch--create-canvas (width height)
  (setq sketch-canvas (sketch-create width height nil nil nil :stroke sketch-default-stroke))
  (apply #'svg-rectangle sketch-canvas 0 0 "100%" "100%"
         :id "bg"
           (when (or sketch-show-grid sketch-background)
             (list :fill
                   (if sketch-show-grid
                       "url(#grid)"
                     sketch-background)
                   )))) ; sketch-background)

(defun sketch-create-grid (&optional grid-param minor-grid-freq)
  (setq sketch-grid-param (or grid-param sketch-grid-param))
  (setq sketch-minor-grid-param (/ (float grid-param) (or minor-grid-freq 4)))
    (setq sketch-grid (cons
                       ;; major-grid
                       (dom-node 'pattern
                                 `((id . "grid")
                                   (width . ,grid-param)
                                   (height . ,grid-param)
                                   (patternUnits . "userSpaceOnUse"))
                                 (dom-node 'rect `((width . ,grid-param) (height . ,grid-param)
                                                   (x . 0) (y . 0)
                                                   (stroke-width . 0.8) (stroke . ,(car sketch-grid-colors))
                                                   (fill . "url(#minorGrid)"))))
                       ;; minor grid
                       (dom-node 'pattern
                                 `((id . "minorGrid")
                                   (width . ,sketch-minor-grid-param)
                                   (height . ,sketch-minor-grid-param)
                                   (patternUnits . "userSpaceOnUse"))
                                 (dom-node 'rect `((width . ,sketch-minor-grid-param) (height . ,sketch-minor-grid-param)
                                                   (x . 0) (y . 0)
                                                   (stroke-width . 0.4) (stroke . ,(cdr sketch-grid-colors))
                                                   ,(when sketch-background
                                                     `(fill . ,sketch-background))))))))

(defun sketch-draw-insert-image (width height)
      (sketch-insert-image sketch-svg
                         (prin1-to-string sketch-root)
                         :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                                 ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                                 sketch
                                 (pointer
                                  arrow)))))
                                  ;; help-echo (lambda (_ _ pos)
                                  ;;             (let (
                                  ;;                   ;; (message-log-max nil)
                                  ;;                   (coords (cdr (mouse-pixel-position))))
                                  ;;               (setq sketch-cursor-position
                                  ;;                     (format "(%s, %s)"
                                  ;;                             (- (car coords) sketch-im-x-offset)
                                  ;;                             (+ (cdr coords) sketch-im-y-offset))))
                                  ;;                           (force-mode-line-update)))))))

(defun sketch-update-insert-image (width height)
  (sketch-insert-image sketch-svg
                       nil
                       ;; :pointer 'arrow
                       :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                               ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                               sketch
                               (pointer arrow))))
  (backward-char))

(defun sketch-object-preview-update (object-type node start-coords end-coords)
  (pcase object-type
    ('line
     (setf (dom-attr node 'x2) (car end-coords))
     (setf (dom-attr node 'y2) (cdr end-coords)))
    ('rectangle
     (setf (dom-attr node 'x) (car (sketch--rectangle-coords start-coords end-coords)))
     (setf (dom-attr node 'y) (cadr (sketch--rectangle-coords start-coords end-coords)))
     (setf (dom-attr node 'width) (caddr (sketch--rectangle-coords start-coords end-coords)))
     (setf (dom-attr node 'height) (cadddr (sketch--rectangle-coords start-coords end-coords))))
    ('circle
     (setf (dom-attr node 'r) (sketch--circle-radius start-coords end-coords)))
    ('ellipse
     (setf (dom-attr node 'cx) (car (sketch--ellipse-coords start-coords end-coords)))
     (setf (dom-attr node 'cy) (cadr (sketch--ellipse-coords start-coords end-coords)))
     (setf (dom-attr node 'rx) (caddr (sketch--ellipse-coords start-coords end-coords)))
     (setf (dom-attr node 'ry) (cadddr (sketch--ellipse-coords start-coords end-coords))))
    ('translate
     (let ((dx (- (car end-coords) (car start-coords)))
           (dy (- (cdr end-coords) (cdr start-coords))))
       (sketch--svg-move dx dy node)))))

(defun sketch-redraw (&optional lisp lisp-buffer update)
  ;; (unless sketch-mode
  ;;   (user-error "Not in sketch-mode buffer"))
  ;; (save-current-buffer
  (when lisp-buffer
    (sketch-update-lisp-window lisp lisp-buffer))
    ;; (let ((lisp-window (or (get-buffer-window "*sketch-root*")
    ;;                        (get-buffer-window lisp-buffer))))
    ;;   (unless (string= (buffer-name (window-buffer lisp-window)) "*sketch*")
    ;;     (if-let (buf (get-buffer"*sketch-root*"))
    ;;         (sketch-update-lisp-window sketch-root buf)
    ;;       (sketch-update-lisp-window lisp lisp-buffer))))
    (setq sketch-root (append (cl-subseq sketch-root 0 2) (list (nth (car show-layers) sketch-layers-list))))
    (dolist (layer (cdr show-layers))
      (setq sketch-root (append sketch-root (list (nth layer sketch-layers-list)))))
    (setq sketch-svg (append sketch-canvas
                             (when sketch-show-labels  (list (sketch-labels)))
                             (list sketch-root)))
    (when sketch-show-grid
      (svg--def sketch-svg (cdr sketch-grid))
      (svg--def sketch-svg (car sketch-grid)))
    (with-current-buffer "*sketch*"
      (let ((inhibit-read-only t))
        (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
        (let ((w (car sketch-size))
              (h (cdr sketch-size)))
          (if update
              (sketch-update-insert-image w h)
            (sketch-draw-insert-image w h))
          (goto-char (point-min))))))

(defun sketch--init (width height &optional grid-param minor-grid-freq)
  (setq sketch-grid-param (or grid-param sketch-grid-param))
  (setq sketch-minor-grid-freq (or minor-grid-freq sketch-minor-grid-freq))
  (let* (svg)
    ;; (when sketch-background
    ;; (unless (memq "none" (list sketch-start-marker sketch-mid-marker sketch-end-marker))
    ;;   (svg-marker sketch-canvas "arrow" 8 8 "black" t))
    (sketch--create-canvas width height)
    (setq sketch-svg (copy-list sketch-canvas))
    (when sketch-show-grid
      (sketch-create-grid grid-param)
      (svg--def sketch-svg (cdr sketch-grid))
      (svg--def sketch-svg (car sketch-grid)))
    (setq sketch-root (sketch-group "root"))
    (setq sketch-layers-list (list (sketch-group "layer-0")))
    (setq show-layers '(0))
    (sketch-draw-insert-image width height)
    (goto-char (point-min)) ; cursor over image looks better
    (sketch-toggle-toolbar)
    (add-hook 'kill-buffer-hook 'sketch-kill-toolbar nil t)
    (special-mode)
    (sketch-mode)))
    ;; (evil-emacs-state)))

(defun sketch (arg)
  "Initialize or switch to (new) SVG image.
With prefix ARG, create sketch using default (customizable)
values"
  (interactive "P")
  (let ((call-buffer (current-buffer))
        (buffer (get-buffer "*sketch*"))
        (width (if arg (car sketch-size) (read-number "Enter width: ") ))
        (height (if arg (cdr sketch-size) (read-number "Enter height: "))))
        (switch-to-buffer (get-buffer-create "*sketch*"))
        ;; (add-to-list 'mode-line-format '(:eval sketch-cursor-position) t)
        (setq sketch-grid-param (if arg 50 (read-number "Enter grid parameter (enter 0 for no grid): ")))
        (sketch--init width height sketch-grid-param)
        (setq sketch-call-buffer call-buffer))) ;; variable is buffer local))

(define-key image-map "o" nil)

(define-minor-mode sketch-mode
  "Create svg images using the mouse.
In sketch-mode buffer press \\[sketch-transient] to activate the
transient."
  :lighter "sketch-mode"
  :keymap
  `(
    ([sketch down-mouse-1] . sketch-interactively)
    ([sketch mouse-3] . sketch-text-interactively)
    ;; ([sketch S-down-mouse-1] . sketch-select)
    ("o" . sketch-set-object)
    ("c" . sketch-set-colors)
    ("w" . sketch-set-width)
    ("d" . sketch-set-dasharray)
    ("g" . sketch-toggle-grid)
    ("s" . sketch-toggle-snap)
    ("S" . image-save)
    ("?" . sketch-help)
    (,(kbd "C-c C-c") . sketch-quick-insert-image))
    ;; (,(kbd "C-c C-s") . sketch-transient))
  (if (boundp 'undo-tree-mode)
      (undo-tree-mode)
    (buffer-enable-undo))
  (setq-local global-hl-line-mode nil)
  (blink-cursor-mode 0))

;;; 
(defvar sketch-object 'line)
(defvar sketch-stroke-color "Black")
(defvar sketch-fill-color "none")
(defvar sketch-stroke-width 1)
(defvar sketch-stroke-dasharray nil)

(defvar sketch-font nil)


(defun sketch-norm (vec)
  "Return norm of a vector (list of numbers).
VEC should be a cons or a list containing only number elements."
  (let ((sum-of-squares (apply #'+
                               (mapcar (lambda (x) (expt x 2))
                                       vec))))
    (expt sum-of-squares 0.5)))

(defun sketch--circle-radius (start-coords end-coords)
  (sketch-norm
   (list (- (car end-coords) (car start-coords))
         (- (cdr end-coords) (cdr start-coords)))))

(defun sketch--rectangle-coords (start-coords end-coords)
  (let ((base-coords (cons (apply #'min (list (car start-coords) (car end-coords)))
                           (apply #'min (list (cdr start-coords) (cdr end-coords))))))
    (list (car base-coords)
          (cdr base-coords)
          (abs (- (car end-coords) (car start-coords)))
          (abs (- (cdr end-coords) (cdr start-coords))))))

(defun sketch--ellipse-coords (start-coords end-coords)
  (list (/ (+ (car start-coords) (car end-coords)) 2)
        (/ (+ (cdr start-coords) (cdr end-coords)) 2)
        (abs (/ (- (car end-coords) (car start-coords)) 2))
        (abs (/ (- (cdr end-coords) (cdr start-coords)) 2))))

(defun sketch--snap-to-grid (coord grid-param)
  (cons (* (round (/ (float (car coord)) grid-param)) grid-param)
        (* (round (/ (float (cdr coord)) grid-param)) grid-param)))

(defun sketch-interactively (event)
  "Draw objects interactively via a mouse drag EVENT. "
  (interactive "@e")
  (let* ((start (event-start event))
         (start-coords (if sketch-snap-to-grid
                           (sketch--snap-to-grid (posn-object-x-y start) sketch-minor-grid-param)
                         (posn-object-x-y start)))
         (points (list (cons (car start-coords) (cdr start-coords)))) ;; list of point needed for polyline/gon
         (object-props (list :stroke-width
                             sketch-stroke-width
                             :stroke
                             sketch-stroke-color
                             :fill
                             sketch-fill-color
                             :stroke-dasharray
                             sketch-stroke-dasharray
                             ;; :marker-end (if args (pcase (transient-arg-value "--marker=" args)
                             ;;                        ("arrow" "url(#arrow)")
                             ;;                        ("dot" "url(#dot)")
                             ;;                        (_ "none"))
                             ;;               (if sketch-include-end-marker
                             ;;                   "url(#arrow)"
                             ;;                 "none"))
                             ))
         (start-command-and-coords (pcase sketch-object
                                     ('line (list 'svg-line
                                                   (car start-coords) (cdr start-coords)
                                                   (car start-coords) (cdr start-coords)))
                                     ('rectangle `(svg-rectangle
                                                    ,@(sketch--rectangle-coords start-coords start-coords)))
                                     ('circle (list 'svg-circle
                                                     (car start-coords) (cdr start-coords)
                                                     (sketch--circle-radius start-coords start-coords)))
                                     ('ellipse `(svg-ellipse ,@(sketch--ellipse-coords start-coords start-coords)))
                                     (var (list (pcase var
                                                  ((or 'polyline 'freehand) 'svg-polyline)
                                                  ('polygon 'svg-polygon))
                                                points))))
         (label (unless (memq sketch-object '(move translate))
                  (sketch-create-label sketch-object))))
    (unless (memq sketch-object '(move translate))
      (apply (car start-command-and-coords)
             (nth sketch-active-layer sketch-layers-list)
             `(,@(cdr start-command-and-coords) ,@object-props :id ,label)))
    (let ((node (car (dom-by-id (nth sketch-active-layer sketch-layers-list)
                                (or label
                                    ;; (transient-arg-value "--object="
                                    ;;                      (oref transient-current-prefix value))
                                    )))))
      (cond ((member sketch-object '(line rectangle circle ellipse move translate))
             (track-mouse
               (while (not (eq (car event) 'drag-mouse-1))
                 (setq event (read-event))
                 (let* ((end (event-start event))
                        (end-coords (if sketch-snap-to-grid
                                        (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                      (posn-object-x-y end))))
                   (sketch-object-preview-update sketch-object node start-coords end-coords)
                   (sketch-redraw nil nil t)
                   (setq sketch-cursor-position (format "(%s, %s)"
                                                        (car end-coords)
                                                        (cdr end-coords)))
                   ;; (force-mode-line-update)
                   )))
             ;; (sketch-possibly-update-image sketch-svg)))
             (let* ((end (event-end event))
                    (end-coords (if sketch-snap-to-grid
                                    (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                  (posn-object-x-y end))))
               (sketch-object-preview-update sketch-object node start-coords end-coords)))
            ((member sketch-object '(polyline polygon))
             (track-mouse
               (while (not (eq (car event) 'double-mouse-1))
                 (setq event (read-event))
                 (let* ((end (event-start event))
                        (end-coords (if sketch-snap-to-grid
                                        (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                      (posn-object-x-y end))))
                   (setf (dom-attr node 'points) (mapconcat (lambda (pair)
                                                              (format "%s %s" (car pair) (cdr pair)))
                                                            (reverse
                                                             (if (eq (car event) 'down-mouse-1)
                                                                 (push end-coords points)
                                                               (cons end-coords points)))
                                                            ", "))
                   (sketch-redraw nil nil t)
                   (setq sketch-cursor-position (format "(%s, %s)"
                                                        (car end-coords)
                                                        (cdr end-coords)))
                   (force-mode-line-update)))
               ;; (sketch-possibly-update-image sketch-svg)))
               (let* ((end (event-end event))
                      (end-coords (if sketch-snap-to-grid
                                      (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                    (posn-object-x-y end))))
                 (setf (dom-attr node 'points) (mapconcat (lambda (pair)
                                                            (format "%s %s" (car pair) (cdr pair)))
                                                          (reverse
                                                           (if (eq (car event) 'down-mouse-1)
                                                               (push end-coords points)
                                                             (cons end-coords points)))
                                                          ", ")))))
            ((string= sketch-object 'freehand)
             (track-mouse
               (while (not (eq (car event) 'drag-mouse-1))
                 (setq event (read-event))
                 (let* ((end (if (eq (car event) 'drag-mouse-1)
                                 (event-end event)
                               (event-start event)))
                        (end-coords (if sketch-snap-to-grid
                                        (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                      (posn-object-x-y end))))
                   (setf (dom-attr node 'points) (mapconcat (lambda (pair)
                                                              (format "%s %s" (car pair) (cdr pair)))
                                                            (reverse (cl-pushnew end-coords points))
                                                            ", "))
                   (sketch-redraw nil nil t)
                   (setq sketch-cursor-position (format "(%s, %s)"
                                                        (car end-coords)
                                                        (cdr end-coords)))
                   (force-mode-line-update))))))
      (when-let (buf (get-buffer "*sketch-root*"))
        (sketch-update-lisp-window sketch-root buf))
      (sketch-redraw))))

(defvar sketch-font-size 15)
(defvar sketch-font-weight "normal")

(defun sketch-text-interactively (event)
  (interactive "@e")
  (let* ((start (event-start event))
         (coords (if sketch-snap-to-grid
                     (posn-object-x-y start)
                   (sketch--snap-to-grid (posn-object-x-y start) sketch-grid-param)))
         (text (read-string "Enter text: "))
         (object-props (append (list :font-size sketch-font-size
                                     :font-weight sketch-font-weight)
                               (when sketch-font
                                 (list :font-family sketch-font))
                               (when sketch-stroke-color
                                 (list :stroke sketch-stroke-color))
                               (when sketch-fill-color
                                 (list :fill sketch-fill-color)))))
    ;; :fill
    ;; (transient-arg-value "--fill-color=" sketch-args)
    ;; :marker-end (if sketch-args (pcase (transient-arg-value "--marker=" sketch-args)
    ;;                        ("arrow" "url(#arrow)")
    ;;                        ("dot" "url(#dot)")
    ;;                        (_ "none"))
    ;;               (if sketch-include-end-marker
    ;;                   "url(#arrow)"
    ;;                 "none"))))
    (apply #'svg-text (nth sketch-active-layer sketch-layers-list) text :x (car coords) :y (cdr coords) :id (sketch-create-label 'text) object-props))
  (sketch-redraw))


;; Web/SVG colors
(defun sketch-colors-sort (colors-rgb-alist)
  (let ((list-colors-sort 'hsv))
    ;; color sort function in courtesy of facemenu.el
    ;; (colors-sorted (mapcar (lambda (c) (cons c (color-name-to-rgb c))) (defined-colors)))
    ;; Schwartzian transform with `(color key1 key2 key3 ...)'.
    (mapcar
     'car
     (sort (delq nil (mapcar
                      (lambda (c)
                        (let ((key (list-colors-sort-key
                                    (car c))))
                          (when key
                            (cons c (if (consp key)
                                        key
                                      (list key))))))
                      colors-rgb-alist)) ;; HERE IS THE LIST
           (lambda (a b)
             (let* ((a-keys (cdr a))
                    (b-keys (cdr b))
                    (a-key (car a-keys))
                    (b-key (car b-keys)))
               ;; Skip common keys at the beginning of key lists.
               (while (and a-key b-key (equal a-key b-key))
                 (setq a-keys (cdr a-keys) a-key (car a-keys)
                       b-keys (cdr b-keys) b-key (car b-keys)))
               (cond
                ((and (numberp a-key) (numberp b-key))
                 (< a-key b-key))
                ((and (stringp a-key) (stringp b-key))
                 (string< a-key b-key)))))))))

;; Adapted from `read-color'
(defun read-color-web (&optional prompt convert-to-RGB)
  "Read a color name or RGB triplet.
Completion is available for color names, but not for RGB triplets.

RGB triplets have the form \"#RRGGBB\".  Each of the R, G, and B
components can have one to four digits, but all three components
must have the same number of digits.  Each digit is a hex value
between 0 and F; either upper case or lower case for A through F
are acceptable.

In addition to standard color names and RGB hex values, the
following are available as color candidates.  In each case, the
corresponding color is used.

 * `foreground at point'   - foreground under the cursor
 * `background at point'   - background under the cursor

Optional arg PROMPT is the prompt; if nil, use a default prompt.

Interactively, or with optional arg CONVERT-TO-RGB-P non-nil,
convert an input color name to an RGB hex string.  Return the RGB
hex string.

Interactively, displays a list of colored completions.  If optional
argument FOREGROUND is non-nil, shows them as foregrounds, otherwise
as backgrounds."
  (interactive "i\np")    ; Always convert to RGB interactively.
  (let* ((completion-ignore-case t)
         (colors (mapcar
                  (lambda (color-name)
                    (let ((color (copy-sequence color-name)))
                      (propertize color 'face
                                  (list :foreground (readable-foreground-color color-name)
                                        :background color))))
                  (mapcar #'car (sketch-colors-sort shr-color-html-colors-alist))))
         (color (completing-read
                 (or prompt "Color (name or #RGB triplet): ")
                 ;; Completing function for reading colors, accepting
                 ;; both color names and RGB triplets.
                 (lambda (string pred flag)
                   (cond
                    ((null flag)        ; Try completion.
                     (or (try-completion string colors pred)
                         (if (color-defined-p string)
                             string)))
                    ((eq flag t)        ; List all completions.
                     (or (all-completions string colors pred)
                         (if (color-defined-p string)
                             (list string))))
                    ((eq flag 'lambda)  ; Test completion.
                     (or (member string colors)
                         (color-defined-p string)))))
                 nil t)))

    ;; Process named colors.
    (when (member color colors)
      (cond ((string-equal color "foreground at point")
             (setq color (foreground-color-at-point)))
            ((string-equal color "background at point")
             (setq color (background-color-at-point))))
      (when (and convert-to-RGB
                 (not (string-equal color "")))
        (let ((components (x-color-values color)))
          (unless (string-match-p "^#\\(?:[[:xdigit:]][[:xdigit:]][[:xdigit:]]\\)+$" color)
            (setq color (format "#%04X%04X%04X"
                                (logand 65535 (nth 0 components))
                                (logand 65535 (nth 1 components))
                                (logand 65535 (nth 2 components))))))))
    color))

(defvar sketch-colors-basic '("White" "Silver" "Gray" "Black"
                            "Red" "Maroon" "Yellow" "Olive"
                            "Lime" "Green" "Aqua" "Teal"
                            "Blue" "Navy" "Fuchsia" "Purple"))


;;; Toolbar
(defun sketch-toolbar-refresh ()
    (with-current-buffer (get-buffer "*sketch-toolbar*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Press ? for help\n\n" 'face 'bold))
        (sketch-toolbar-colors)
        (sketch-toolbar-widths)
        (sketch-toolbar-objects)
        (insert "\n")
        (sketch-toolbar-toggles)
        (insert "\n\n")
        (sketch-toolbar-font)
        (goto-char (point-min)))))

(defun sketch-toggle-toolbar ()
  (interactive)
  (let ((win (get-buffer-window "*sketch-toolbar*")))
    (if win
        (delete-window win)
      (let ((buffer (get-buffer-create "*sketch-toolbar*")))
        (set-window-dedicated-p
         (display-buffer-in-side-window (get-buffer-create "*sketch-toolbar*")
                                        '((side . right) (window-width . 20)))
         t)
        (with-current-buffer buffer
          (setq cursor-type nil)
          (special-mode))
        (sketch-toolbar-refresh)))))

(defun sketch-toolbar-colors ()
  ;; STROKE COLOR
  (insert (propertize "STROKE COLOR\n"))
  (insert-text-button "   "
                       'action
                            (lambda (button) (interactive)
                              (setq sketch-stroke-color (plist-get (button-get button 'face) :background)))
                      'face (list :background
                                  (alist-get sketch-stroke-color
                                             shr-color-html-colors-alist
                                             nil nil 'string=)))
  (insert " ")
  (insert (if (string= sketch-stroke-color "none")
              "none"
            sketch-stroke-color))
  (insert "\n")
  (insert-text-button "none"
                      'action (lambda (button) (interactive)
                                (setq sketch-stroke-color "none")
                                (sketch-toolbar-refresh)))
  (insert "\n\n")
  (let ((counter 0))
    (dolist (color sketch-colors-basic)
      (insert-text-button "   "
                          'action
                          (lambda (button) (interactive)
                                    (setq sketch-stroke-color
                                          (car (rassoc (plist-get (button-get button 'face) :background)
                                                       shr-color-html-colors-alist)))
                                    (sketch-toolbar-refresh)
                                    ;; (transient-quit-all)
                                    ;; (call-interactively #'sketch-transient)
                                    )
                          'face (list
                                 :background (alist-get color shr-color-html-colors-alist nil nil 'string=)))
      (setq counter (1+ counter))
      (if (not (= counter 4))
          (insert " ")
        (insert "\n\n")
        (setq counter 0))))

  ;; FILL COLOR
  (insert (propertize "FILL COLOR\n"))
  (insert-text-button "   "
                       'action
                            (lambda (button) (interactive)
                              (print sketch-fill-color))
                      'face (list :background (alist-get sketch-fill-color
                                                         shr-color-html-colors-alist
                                                         nil nil 'string=)))
  (insert " ")
  (insert (if (string= sketch-fill-color "none")
              "none"
            sketch-fill-color))
  (insert "\n")
  (insert-text-button "none"
                      'action (lambda (_) (interactive)
                                (setq sketch-fill-color "none")
                                (sketch-toolbar-refresh)))
  (insert "\n\n")
  (let ((counter 0))
    (dolist (color sketch-colors-basic)
      (insert-text-button "   "
                          'action
                          (lambda (button) (interactive)
                            (setq sketch-fill-color
                                  (car (rassoc (plist-get (button-get button 'face) :background)
                                               shr-color-html-colors-alist)))
                            (sketch-toolbar-refresh)
                            ;; (transient-quit-all)
                            ;; (call-interactively #'sketch-transient)
                            )
                          'face (list
                                 :background (alist-get color shr-color-html-colors-alist nil nil 'string=)))
      (setq counter (1+ counter))
      (if (not (= counter 4))
          (insert " ")
        (insert "\n\n")
        (setq counter 0)))))

(defun sketch-toolbar-widths ()
  (insert "STROKE WIDTH: ")
  (insert (number-to-string sketch-stroke-width))
  (insert "\n")
  (let* ((widths 9)
         (button-width (+ (* 4 (default-font-width)) 3))
         (button-height (default-font-height))
         (stroke-height (/ button-height 2)))
    (let ((counter 0))
      (dotimes (w widths)
        (insert-text-button (format "%s" (1+ w))
                            'action
                            (lambda (button) (interactive)
                              (setq sketch-stroke-width (string-to-number (button-label button)))
                              (sketch-toolbar-refresh)
                              ;; (transient-quit-all)
                              ;; (call-interactively #'sketch-transient)
                              )
                            'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                  (svg-rectangle svg 0 0 button-width button-height
                                                                 :fill "white")
                                                  (svg-line svg 5 stroke-height (- button-width 5) stroke-height
                                                            :stroke "black" :stroke-width (1+ w))
                                                  svg)))
        (setq counter (1+ counter))
        (if (not (= counter 3))
            (insert " ")
          (insert "\n\n")
          (setq counter 0))))))

(defun sketch-toolbar-objects ()
  (insert "OBJECT\n")
  (let ((objects '(line polyline circle ellipse rectangle polygon)))
    (while objects
      (let ((o (car objects)))
        (apply #'insert-text-button
               (symbol-name o)
               'action (lambda (button) (interactive)
                         (setq sketch-object (intern (button-label button))))
               (when (eq o sketch-object)
                 (list 'face 'custom-button-unraised)))
        (dotimes (_ (- 10 (length (symbol-name o))))
          (insert " ")))
      (let ((o (cadr objects)))
        (apply #'insert-text-button
               (symbol-name o)
               'action (lambda (button) (interactive)
                         (setq sketch-object (intern (button-label button))))
               (when (eq o sketch-object)
                 (list 'face 'custom-button-unraised))))
      ;; (list 'face (if (eq o sketch-object)
      ;;                 'widget-button-pressed
      ;;               'widget-button)))
      (insert "\n")
      (setq objects (cddr objects)))
    (apply #'insert-text-button
           "freehand"
           'action (lambda (button) (interactive)
                     (setq sketch-object (intern (button-label button))))
           (when (eq 'freehand sketch-object)
             (list 'face 'custom-button-unraised)))
    (insert "\n")))

(defun sketch-toolbar-toggles ()
  (insert "TOGGLES\n")
  (apply #'insert-text-button (format "Grid: %s" (if sketch-show-grid "show" "hide"))
                      'action
                      (lambda (button) (interactive)
                        (sketch-toggle-grid)
                        (sketch-toolbar-refresh))
                      (when sketch-show-grid
                        (list 'face 'custom-button-unraised)))
                      ;; (list 'face (if sketch-grid
                      ;;                 'widget-button-pressed
                      ;;               'widget-button)))
  (insert "\n")
  (apply #'insert-text-button (format "Snap: %s" (if sketch-snap-to-grid "on" "off"))
                      'action
                      (lambda (button) (interactive)
                        (sketch-toggle-snap)
                        (sketch-toolbar-refresh))
                      (when sketch-snap-to-grid
                        (list 'face 'custom-button-unraised))))
                      ;; (list 'face (if sketch-snap-to-grid
                      ;;                 'widget-button-pressed
                      ;;               'widget-button))))


(defun sketch-kill-toolbar ()
  (let ((toolbar (get-buffer "*sketch-toolbar*")))
    (when toolbar
      (kill-buffer toolbar))))

;;; Configuration
(defun sketch-set-object ()
  (interactive)
  (setq sketch-object
        (intern (read-answer "Select object: "
                             '(("freehand"  ?f "draw freehand with mouse drag")
                               ("line"      ?l "draw line with mouse drag")
                               ("rectangle" ?r "draw rectangle with mouse drag")
                               ("circle"    ?c "draw circle with mouse drag")
                               ("ellipse"   ?e "draw-ellipse with mouse drag")
                               ("polyline"  ?p "draw polyline by clicking. Double click to insert end.")
                               ("polygon"   ?g "draw polygon by clicking. Double click to insert end."))))))

(defun sketch-set-colors (&optional fill)
  "Set stroke or FILL color.
When FILL is t (i.e. with prefix), set fill color. Otherwise set
stroke color."
  (interactive "P")
  (set (if fill
           'sketch-fill-color
         'sketch-stroke-color)
       (substring-no-properties (read-color-web "Select color: ")))
  (sketch-toolbar-refresh))

(defun sketch-set-width ()
  (interactive)
  (setq sketch-stroke-width (string-to-number
                             (completing-read "Enter width (floats allowed): "
                                              (number-sequence 1 10)))))

(defun sketch-set-dasharray ()
  (interactive)
  (setq sketch-stroke-dasharray (completing-read "Enter dasharry (custom values allowed): "
                                                 '("8" "8,4"))))

(defun sketch-set-font ()
  (interactive)
  (pop-to-buffer "*sketch-fonts*")
  (let ((button-width (* 4 5 (default-font-width)))
        (button-height (* 2 (default-font-height)))
        (counter 0))
    (dolist (x (sort (cl-remove-duplicates (font-family-list)) #'string-lessp))
      (insert-text-button x
                          'action
                          (lambda (button) (interactive)
                            (setq sketch-font (button-label button))
                            (kill-buffer)
                            (sketch-toolbar-refresh)
                            ;; (transient-quit-all)
                            ;; (call-interactively #'sketch-transient)
                            )
                          'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                (svg-rectangle svg 0 0 button-width button-height
                                                               :fill "white")
                                                (if sketch-font
                                                    (svg-text svg "ABC abc"
                                                           :font-size button-height
                                                           :font-family x
                                                           :stroke "black"
                                                           :fill "black"
                                                           :x 4
                                                           :y (- button-height 4)))
                                                svg)))
      (insert " ")
      (insert x)
      (setq counter (1+ counter))
      (if (/= counter 2)
            (insert (make-string
                     (- 30 (length x)) (string-to-char " ")))
        (insert "\n\n")
        (setq counter 0)))
    (goto-char (point-min))))

(defun sketch-toolbar-font ()
  (interactive)
  (insert "FONT\n")
  (insert "family: ")
  (if sketch-font
      (let ((button-width (* 2 5 (default-font-width)))
            (button-height (default-font-height))
            (counter 0))
        (insert-text-button sketch-font
                            'action
                            (lambda (_) (interactive)
                              (sketch-set-font)
                              ;; (transient-quit-all)
                              ;; (call-interactively #'sketch-transient)
                              )
                            'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                  (svg-rectangle svg 0 0 button-width button-height
                                                                 :fill "white")
                                                  (svg-text svg "ABC abc"
                                                            :font-size button-height
                                                            :font-family sketch-font
                                                            :stroke "black"
                                                            :fill "black"
                                                            :x 3
                                                            :y (- button-height 3))
                                                  svg))))
        (insert-text-button "none"
                            'action
                            (lambda (_) (interactive)
                              (sketch-set-font))))
  (insert"\n")
  (insert "Size:   ")
  (insert-text-button (number-to-string sketch-font-size)
                      'action
                      (lambda (_) (interactive)
                        (setq sketch-font-size (completing-read "Select font: " (number-sequence 8 40 2)))
                        ;; (transient-quit-all)
                        ;; (call-interactively #'sketch-transient)
                        )))


(defun sketch-toggle-grid ()
  (interactive)
  (setq sketch-show-grid (if sketch-show-grid nil t))
  (if (not sketch-show-grid)
      (dom-set-attribute (car (dom-by-id sketch-canvas "bg")) 'fill sketch-background)
    (unless sketch-grid
      (sketch-create-grid))
    (dom-set-attribute (car (dom-by-id sketch-canvas "bg")) 'fill "url(#grid)"))
  ;;        (svg--def sketch-svg (cdr sketch-grid))
  ;;        (svg--def sketch-svg (car sketch-grid)))
  ;;       (t
  ;;        (dom-remove-node sketch-svg (car (dom-by-id sketch-svg "^grid$")))
  (sketch-redraw)
  (sketch-toolbar-refresh))

(defun sketch-toggle-snap ()
  (interactive)
  (setq sketch-snap-to-grid (if sketch-snap-to-grid nil t))
  (sketch-toolbar-refresh)
  (message "Snap-to-grid %s" (if sketch-snap-to-grid "on" "off")))

(defun sketch-toggle-labels ()
  (interactive)
  (setq sketch-show-labels (if sketch-show-labels nil t))
  (sketch-toolbar-refresh))

(defvar-local sketch-call-buffer nil)

(add-hook 'org-ctrl-c-ctrl-c-final-hook 'sketch-org-toggle-image)

(defun sketch-org-toggle-image ()
  (let* ((context (org-element-lineage
                   (org-element-context)
                   ;; Limit to supported contexts.
                   '(babel-call clock dynamic-block footnote-definition
                                footnote-reference inline-babel-call inline-src-block
                                inlinetask item keyword node-property paragraph
                                plain-list planning property-drawer radio-target
                                src-block statistics-cookie table table-cell table-row
                                timestamp)
                   t))
         (type (org-element-type context)))
    (when (eq type 'paragraph)
      (let ((parent (org-element-property :parent context)))
        (when (eq (org-element-type parent) 'special-block)
          (let* ((props (cadr parent))
                 (beg (plist-get props :contents-begin))
                 (end (plist-get props :contents-end)))
            (if (get-char-property (point) 'display)
                (remove-text-properties beg end '(display nil))
              (let* ((xml (buffer-substring-no-properties beg end))
                     (image (create-image xml 'svg t)))
                (put-text-property beg (1- end) 'display image)
                (goto-char beg)))))))))

(defun sketch-quick-insert-image (&optional insert-at-end-of-file)
  "Insert image at point as overlay wrapped in org image block.
The image overlay is created over the inserted xml
definition and is wrapped inside an image block (not yet
supported by org-mode). When INSERT-AT-END-OF-FILE is non-nil
then insert the image at the end"
  (interactive "P")
  (let ((insert-buffer sketch-call-buffer)
        (image-def sketch-svg))
    (kill-buffer "*sketch*")
    (switch-to-buffer insert-buffer)
    (when insert-at-end-of-file
      (goto-char (point-max))
      (unless (= (current-column) 0)
        (newline)))
    (insert "#+BEGIN_IMAGE\n")
    (let* ((image (svg-image image-def))
           (data (image-property image :data)))
      (insert-image image (with-temp-buffer
                            (insert data)
                            (let ((bounds (bounds-of-thing-at-point 'line)))
                              (sgml-pretty-print (car bounds) (cdr bounds)))
                            (buffer-string)))
      (insert "\n#+END_IMAGE"))))

(defun sketch-help ()
  (interactive)
  (describe-keymap 'sketch-mode-map))
