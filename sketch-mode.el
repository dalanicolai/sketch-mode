;;; sketch-mode.el --- Quickly create svg sketches using keyboard and mouse -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: D.L. Nicolai <dalanicolai@gmail.com>
;; Created: 17 Jul 2021
;; Version: 1.0.2

;; Keywords: multimedia
;; URL: https://github.com/dalanicolai/sketch-mode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;; DONE add functionality to toggle grid

;; DONE implement (simple) undo mechanism


;; TODO maybe transform relevant transient argument (strings) to variables ;; (af`add-object-modify-feature' branch)
;; TODO add functionality to start drawing from org-mode source block and update
;; source block after each draw/edit (showing the image as the block'ss output)

;; TODO maybe add keybindings (save/bind transient setting to specific 'mouse keys')

;; TODO add clipping fuctionality (see `svg-clip-path')

;; TODO create function to insert svg snippets (so you could design objects in
;; advanced software and use them quickly here in your sketches)

;; TODO create function to save snippets

;; TODO implement modularity. i.e. create 'layers' via svg groups <g> (related
;; to snippet functionality)

;; TODO create zoom functionality

;; NOTE this is a most straightforward sketch-mode. A more advanced/general version
;; could implement a drawing DSL based on nodes (a la tikz/asymptote etc.)


;;;; Code
(require 'svg)
(require 'shr-color)
(require 'sgml-mode)
(require 'org-element)

(eval-when-compile
  (require 'evil-vars nil t))
(require 'undo-tree nil t)


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

(defvar sketch-action 'line)
(defvar sketch-stroke-color "Black")
(defvar sketch-fill-color "none")
(defvar sketch-stroke-width 1)
(defvar sketch-stroke-dasharray nil)

(defvar sketch-font nil)
(defvar sketch-font-size 20)
(defvar sketch-font-weight "normal")


(defvar sketch-selection nil)

(defvar sketch-active-layer 0)
(defvar sketch-layers-list nil)
(defvar show-layers nil)

(defvar sketch-show-labels nil)
(defvar sketch-label-size 15)

(defvar sketch-call-buffer nil)

(defvar sketch-lisp-buffer-name nil)
(defvar sketch-side-window-max-width (lambda () (- 1 (/ (float (car
                                                                (image-size
                                                                 (get-text-property (point-min) 'display) t)))
                                                        (frame-pixel-width)))))
(defvar sketch-im-x-offset nil)
(defvar sketch-cursor-position "")
(defvar sketch-show-coords nil)
(defvar sketch-coordless-mode-line-format nil)

(declare-function evil-redo "evil-commands" ())
(declare-function evil-undo "evil-commands" ())
(declare-function undo-tree-redo "undo-tree" ())
(declare-function undo-tree-undo "undo-tree" ())

;;; Rendering

;;; Some snippets for svg.el
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
    (apply #'svg-create w h
           :viewBox (format "%s %s %s %s"
                            (or pan-x 0)
                            (or pan-y 0)
                            (/ (float w) scale)
                            (/ (float h) scale))
           args)))

(defun sketch-image (svg &rest props)
  "Return an image object-label from SVG.
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

(defun sketch-add-layer ()
  (interactive)
  (let ((new-layer (length sketch-layers-list)))
    (setq sketch-layers-list (append
                              sketch-layers-list
                              (list (sketch-group
                                     (format "layer-%s" new-layer)))))
    (setq sketch-active-layer new-layer)
    (setq show-layers (append show-layers (list new-layer)))
    (message "Existing layers (indices): %s"
             (mapconcat #'number-to-string
                        (number-sequence 0 (1- (length sketch-layers-list)))
                        ", "))))

(defun sketch-labels ()
  "Create svg-group with svg text nodes for all elements in layer.
If value of variable ‘sketch-show-labels' is ‘layer', create ..."
  (interactive)
  (let ((nodes (pcase sketch-show-labels
                 ("layer" (dom-children (nth sketch-active-layer sketch-layers-list)))
                 ("all" (apply #'append (mapcar (lambda (l)
                                                  (dom-children (nth l sketch-layers-list)))
                                                show-layers)))))
        (svg-labels (sketch-group "labels")))

    (defun sketch-label-text-node (node x y &rest props)
      (apply #'svg-text
             svg-labels
             (dom-attr node 'id)
             (append (list :x x
                           :y y
                           :font-size sketch-label-size
                           :stroke "red"
                           :fill "red")
                     (when-let (x (dom-attr node 'transform))
                       (list :transform x))
                     props)))

    (with-no-warnings
    (mapc (lambda (node)
            (pcase (dom-tag node)
              ('rect (sketch-label-text-node
                      node
                      (+ (dom-attr node 'x) 2)
                      (+ (dom-attr node 'y)
                         (- (dom-attr node 'height) 2))))
              ;; ('line (sketch-label-text-node
              ;;         node
              ;;         (dom-attr node 'x1)
              ;;         (dom-attr node 'y1)))
              ;; ((or 'circle 'ellipse)
              ;;  (sketch-label-text-node
              ;;   node
              ;;   (dom-attr node 'cx)
              ;;   (dom-attr node 'cy)))
              ;; ((or 'polyline 'polygon)
              ;;  (let ((coords (split-string
              ;;                 (car (split-string (dom-attr node 'points) ","))
              ;;                 nil
              ;;                 t)))
              ;;    (sketch-label-text-node
              ;;     node
              ;;     (string-to-number (car coords))
              ;;     (string-to-number (cadr coords)))))
              ;; ('text (sketch-label-text-node
              ;;         node
              ;;         (dom-attr node 'x)
              ;;         (+ (dom-attr node 'y)
              ;;            sketch-label-size)))
              ;; ('g (let ((s (dom-attr node
              ;;                        'transform)))
              ;;       (string-match "translate\(\\([0-9]*\\)[, ]*\\([0-9]*\\)" s)
              ;;       (let ((x (match-string 1 s))
              ;;             (y (match-string 2 s)))
              ;;         (sketch-label-text-node
              ;;          node
              ;;          x
              ;;          y))))
              ))
          nodes))
    svg-labels))


(defun sketch-labels-list ()
  (apply #'append
         (mapcar (lambda (l)
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

(defun sketch-maybe-update-modeline ()
  (when sketch-show-coords
    (force-mode-line-update)))

(defun sketch-draw-insert-image ()
  (sketch-insert-image sketch-svg
                       (prin1-to-string sketch-root)
                       :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                               ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                               sketch
                               ,(append '(pointer
                                          arrow)
                                        (when sketch-show-coords
                                          (list 'help-echo (lambda (_ _ _)
                                                             (let ((coords (cdr (mouse-pixel-position))))
                                                               (setq sketch-cursor-position
                                                                     (format "(%s, %s)"
                                                                             (- (car coords) sketch-im-x-offset)
                                                                             (cdr coords))))
                                                             ;; (+ (cdr coords) sketch-im-y-offset))))
                                                             (force-mode-line-update)))))))))

(defun sketch-update-insert-image ()
  (sketch-insert-image sketch-svg
                       nil
                       ;; :pointer 'arrow
                       :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                               ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                               sketch
                               (pointer arrow))))
  (backward-char))

(defun sketch-object-preview-update (object-type node start-coords end-coords &optional start-node)
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
     (message "deze %s" start-node)
     (let ((dx (- (car end-coords) (car start-coords)))
           (dy (- (cdr end-coords) (cdr start-coords))))
       (sketch--svg-move dx dy node start-node)))))

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
                           (list sketch-root)
                           (when sketch-show-labels (list (sketch-labels)))))
  (when sketch-show-grid
    (svg--def sketch-svg (cdr sketch-grid))
    (svg--def sketch-svg (car sketch-grid)))
  (with-current-buffer "*sketch*"
    (let ((inhibit-read-only t))
      (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
      (if update
          (sketch-update-insert-image)
        (sketch-draw-insert-image))
      (goto-char (point-min)))))


(define-key image-map "o" nil)

(defvar sketch-mode-map
  (let ((map (make-sparse-keymap))
        (bindings `(([sketch down-mouse-1] . sketch-interactively)
                   ([sketch mouse-3] . sketch-text-interactively)
                   ([sketch C-S-drag-mouse-1] . sketch-crop)
                   ([sketch S-down-mouse-1] . sketch-select)
                   ("a" . sketch-set-action)
                   ("c" . sketch-set-colors)
                   ("w" . sketch-set-width)
                   ("sd" . sketch-set-dasharray)
                   ("fw" . sketch-set-font-with-keyboard)
                   ("fs" . sketch-set-font-size-by-keyboard)
                   ("fc" . sketch-set-font-color)
                   ("v" . sketch-keyboard-select)
                   ("m" . sketch-modify-object)
                   ("d" . sketch-remove-object)
                   ("tg" . sketch-toggle-grid)
                   ("ts" . sketch-toggle-snap)
                   ("tt" . sketch-toggle-toolbar)
                   ("." . sketch-toggle-key-hints)
                   ("tc" . sketch-toggle-coords)
                   ("l" . sketch-cycle-labels)
                   ("D" . sketch-show-definition)
                   ("u" . sketch-undo)
                   ("U" . sketch-redo)
                   ("S" . image-save)
                   (,(kbd "C-c C-c") . sketch-quick-insert-image)
                   ("?" . sketch-help)
                   ("Q" . sketch-quit))))
    (dolist (b bindings)
      (define-key map (car b) (cdr b)))
    map))

(define-derived-mode sketch-mode special-mode "sketch-mode"
  "Create svg images using the mouse.
In sketch-mode buffer press \\[sketch-transient] to activate the
transient."
  ;; :lighter "sketch-mode"
  ;; :keymap
  ;; `(
  ;;   ([sketch down-mouse-1] . sketch-interactively)
  ;;   ([sketch mouse-3] . sketch-text-interactively)
  ;;   ([sketch C-S-drag-mouse-1] . sketch-crop)
  ;;   ([sketch S-down-mouse-1] . sketch-select)
  ;;   ("a" . sketch-set-action)
  ;;   ("c" . sketch-set-colors)
  ;;   ("w" . sketch-set-width)
  ;;   ("sd" . sketch-set-dasharray)
  ;;   ("fw" . sketch-set-font-with-keyboard)
  ;;   ("fs" . sketch-set-font-size-by-keyboard)
  ;;   ("fc" . sketch-set-font-color)
  ;;   ("v" . sketch-keyboard-select)
  ;;   ("m" . sketch-modify-object)
  ;;   ("d" . sketch-remove-object)
  ;;   ("tg" . sketch-toggle-grid)
  ;;   ("ts" . sketch-toggle-snap)
  ;;   ("tt" . sketch-toggle-toolbar)
  ;;   ("." . sketch-toggle-key-hints)
  ;;   ("tc" . sketch-toggle-coords)
  ;;   ("l" . sketch-cycle-labels)
  ;;   ("D" . sketch-show-definition)
  ;;   ("u" . sketch-undo)
  ;;   ("U" . sketch-redo)
  ;;   ("S" . image-save)
  ;;   (,(kbd "C-c C-c") . sketch-quick-insert-image)
  ;;   ("?" . sketch-help)
  ;;   ("Q" . sketch-quit))
    ;; (,(kbd "C-c C-s") . sketch-transient))
  (with-no-warnings
    (if (boundp 'undo-tree-mode)
        (undo-tree-mode))
    (buffer-enable-undo))
  (setq-local global-hl-line-mode nil)
  (blink-cursor-mode 0))

;; TODO format/propertize key hints
(defun sketch-toggle-key-hints ()
  (interactive)
  (let ((win (get-buffer-window "*sketch-key-hints*")))
    (if win
        (delete-window win)
      (let ((window-sides-vertical t)
            (buffer (get-buffer-create "*sketch-key-hints*")))
        (set-window-dedicated-p
         (display-buffer-in-side-window (get-buffer-create "*sketch-key-hints*")
                                        `((side . bottom)
                                          (slot . -1)
                                          (window-height . 10)))
         t)
        (with-current-buffer buffer
          (insert
           "Stroke/Fill            Font              Edit               Toggle          Definition
-----------------------------------------------------------------------------------------------
[a]      : action      [fw]: font        [v]  : select      [tg]: grid      [D] Show definition
[(C-u) c]: color       [fs]: font-size   [m]  : modify      [ts]: snap
[w]      : width       [fc]: font-color  [d]  : delete      [tt]: toolbar
[sd]     : dasharray                     [u/U]: undo/redo   [tc]: coords

[down-mouse-1] main action, [down-mouse-3] add text")
          (setq cursor-type nil)
          (special-mode))))))

(defun sketch-kill-key-hints ()
  (let ((key-hints (get-buffer "*sketch-key-hints*")))
    (when key-hints
      (kill-buffer key-hints))))

;;;###autoload
(defun sketch (arg)
  "Initialize or switch to (new) SVG image.
With prefix ARG, create sketch using default (customizable)
values"
  (interactive "P")
  (let ((buffer (get-buffer "*sketch*")))
    (cond (buffer
           (switch-to-buffer buffer)
           ;; TODO maybe immprove, i.e. always show on visit
           (sketch-toggle-toolbar)
           (sketch-toggle-key-hints)
           )
          (t
           (let ((call-buffer (current-buffer))
                 (width (if arg (car sketch-size) (read-number "Enter width: ") ))
                 (height (if arg (cdr sketch-size) (read-number "Enter height: "))))
             (switch-to-buffer (get-buffer-create "*sketch*"))
             (setq sketch-action 'line)
             (setq sketch-grid-param (if arg 50 (read-number "Enter grid parameter (enter 0 for no grid): ")))
             (sketch--init width height sketch-grid-param)
             (when sketch-show-coords
               (setq sketch-coordless-mode-line-format mode-line-format)
               (add-to-list 'mode-line-format '(:eval sketch-cursor-position) t))
             (setq sketch-call-buffer call-buffer)))))) ;; variable is buffer local))

(defun sketch--init (width height &optional grid-param minor-grid-freq)
  (setq sketch-grid-param (or grid-param sketch-grid-param))
  (setq sketch-minor-grid-freq (or minor-grid-freq sketch-minor-grid-freq))
  ;; (when sketch-background
  ;; (unless (memq "none" (list sketch-start-marker sketch-mid-marker sketch-end-marker))
  ;;   (svg-marker sketch-canvas "arrow" 8 8 "black" t))
  (sketch--create-canvas width height)
  (setq sketch-svg (seq-copy sketch-canvas))
  (when sketch-show-grid
    (sketch-create-grid grid-param)
    (svg--def sketch-svg (cdr sketch-grid))
    (svg--def sketch-svg (car sketch-grid)))
  (setq sketch-root (sketch-group "root"))
  (setq sketch-layers-list (list (sketch-group "layer-0")))
  (setq show-layers '(0))
  (sketch-draw-insert-image)
  (goto-char (point-min)) ; cursor over image looks better
  (setq sketch-im-x-offset (car (window-absolute-pixel-position)))
  (sketch-toggle-toolbar)
  (sketch-toggle-key-hints)
  (add-hook 'kill-buffer-hook 'sketch-kill-toolbar nil t)
  (add-hook 'kill-buffer-hook 'sketch-kill-key-hints nil t)
  (special-mode)
  (sketch-mode))

(defun sketch-quit-window ()
  "Quit sketch window. The window can be restores with ‘M-x sketch'"
  (interactive)
  (when (get-buffer "*sketch-toolbar*")
    (kill-buffer "*sketch-toolbar*"))
  (quit-window))

(defun sketch-quit ()
  "Quit sketch-mode and kill buffers."
  (interactive)
  (when (get-buffer "*sketch-toolbar*")
    (kill-buffer "*sketch-toolbar*"))
  (kill-buffer "*sketch*"))

;;; Actions
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
         (object-props (if (eq sketch-action 'text)
                           (append (list :font-size sketch-font-size
                                         :font-weight sketch-font-weight)
                                   (when sketch-font
                                     (list :font-family sketch-font))
                                   (when sketch-stroke-color
                                     (list :stroke sketch-stroke-color))
                                   (when sketch-fill-color
                                     (list :fill sketch-fill-color)))
                         (list :stroke-width
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
                               )))
         (start-command-and-coords (pcase sketch-action
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
         (label (unless (memq sketch-action '(move translate))
                  (sketch-create-label sketch-action))))
    (pcase sketch-action
      ('text (let ((text (read-string "Enter text: ")))
               (apply #'svg-text
                      (nth sketch-active-layer sketch-layers-list)
                      text
                      :x (car start-coords)
                      :y (cdr start-coords)
                      :id label object-props)))
      (_ (unless (memq sketch-action '(move translate))
           (apply (car start-command-and-coords)
                  (nth sketch-active-layer sketch-layers-list)
                  `(,@(cdr start-command-and-coords) ,@object-props :id ,label)))
         (let* ((node (car (dom-by-id (nth sketch-active-layer sketch-layers-list)
                                      (if (memq sketch-action '(move translate))
                                          (car sketch-selection)
                                        label))))
                (translate-start (dom-attr node 'transform)))
           (track-mouse
             (pcase sketch-action
               ((or 'line 'rectangle 'circle 'ellipse 'move 'translate)
                (let ((event (read-event)))
                  (while (not (memq (car event) '(mouse-1 drag-mouse-1)))
                    (let* ((end (event-start event))
                           (end-coords (if sketch-snap-to-grid
                                           (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                         (posn-object-x-y end))))
                      (sketch-object-preview-update sketch-action
                                                    node
                                                    start-coords
                                                    end-coords
                                                    (when (eq sketch-action 'translate)
                                                      translate-start))
                      (sketch-redraw nil nil t)
                      (when (and sketch-lisp-buffer-name (buffer-live-p (get-buffer sketch-lisp-buffer-name)))
                        (sketch-update-lisp-window node sketch-lisp-buffer-name))
                      (setq event (read-event))
                      (when sketch-show-coords
                        (setq sketch-cursor-position (format "(%s, %s)"
                                                             (car end-coords)
                                                             (cdr end-coords))))
                      (sketch-maybe-update-modeline)
                      ))
                  (let* ((end (event-end event))
                         (end-coords (if sketch-snap-to-grid
                                         (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                       (posn-object-x-y end))))
                    (if (and (equal (car start-coords) (car end-coords))
                             (equal (cdr start-coords) (cdr end-coords)))
                        (dom-remove-node (nth sketch-active-layer sketch-layers-list) node)
                      (sketch-object-preview-update sketch-action
                                                    node
                                                    start-coords
                                                    end-coords
                                                    (when (eq sketch-action 'translate)
                                                      translate-start))
                      (sketch-redraw nil nil t)
                      (when (and sketch-lisp-buffer-name (buffer-live-p (get-buffer sketch-lisp-buffer-name)))
                        (sketch-update-lisp-window node sketch-lisp-buffer-name))))))


               ((or 'polyline 'polygon)
                (while (not (eq (car event) 'double-mouse-1))
                  (setq event (read-event))
                  (let* ((end (event-start event))
                         (end-coords (if sketch-snap-to-grid
                                         (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)
                                       (posn-object-x-y end))))
                    (let (message-log-max)
                      (message "Press double click to finish by inserting a final node"))
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
                    (sketch-maybe-update-modeline)))
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
                                                           ", "))))


               ('freehand
                (while (not (memq (car event) '(mouse-1 drag-mouse-1)))
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
                    (sketch-maybe-update-modeline))))
               )))))
    (when-let (buf (get-buffer "*sketch-root*"))
      (sketch-update-lisp-window sketch-root buf))
    (sketch-redraw)))

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
    (apply #'svg-text
           (nth sketch-active-layer sketch-layers-list)
           text
           :x (car coords)
           :y (cdr coords)
           :id (sketch-create-label 'text)
           object-props))
  (sketch-redraw))

;;; Modify object-label
(defun sketch-keyboard-select (&optional all)
  "Select labels to include in selection.
Initial input shows current selection. With prefix ARG initial
selection shows all object in sketch."
  (interactive "P")
  (setq sketch-selection (completing-read-multiple "Select labels for selection (separated by ,): "
                                                   (sketch-labels-list)
                                                   nil
                                                   t
                                                   (mapconcat #'identity
                                                              (if all
                                                                  (sketch-labels-list)
                                                                sketch-selection)
                                                              ","))))


(defun sketch-move-object (buffer object-def props coords amount)
  (dolist (coord coords)
    (cl-incf (alist-get coord props) amount))
  (sketch-redraw object-def buffer))

(defun sketch-parse-transform-string (value)
  "Parse SVG transform VALUE (string) to alist.
The elements of the alist cons-cell consisting of the
transform (symbol) and its values (list)."
  (mapcar (lambda (p)
            (cons (intern (car p))
                  (mapcar #'string-to-number (split-string (cadr p)))))
          (seq-partition (split-string value "[()\n]+" t " *") 2)))

(defun sketch-format-transform (transform-alist)
  "Format TRANSFORM-ALIST to transform string.
The TRANSFORM-ALIST generally is a transform of the an alist
returned by the function `sketch-parse-transform-string'"
  (mapconcat #'identity
             (mapcar (lambda (c)
                       (format "%s(%s)"
                               (symbol-name (car c))
                               (mapconcat #'number-to-string (cdr c) " ")))
                     transform-alist)
             "\n"))

(defun sketch-prop-vals (props &rest keys)
  (mapcar (lambda (p) (alist-get p props)) keys))

(defun sketch-bbox-ex-transform (object)
  (pcase object
    (`(line ,props)
     (sketch-prop-vals props
                       'x1 'y1 'x2 'y2))

    (`(rect ,props)
     (pcase-let ((`(,x ,y ,w ,h)
                  (sketch-prop-vals props 'x 'y 'width 'height)))
       (list x y (+ x w) (+ y h))))

    (`(circle ,props)
     (pcase-let ((`(,cx ,cy ,r) (sketch-prop-vals props
                                                'cx 'cy 'r)))
       (print (list (- cx r) (+ cx r) (- cy r) (+ cy r)))))

    (`(ellipse ,props)
     (pcase-let ((`(,cx ,cy ,rx ,ry) (sketch-prop-vals props
                                                     'cx 'cy 'rx 'ry)))
       (print (list (- cx rx) (+ cx rx) (- cy ry) (+ cy ry)))))
    (`(text ,props ,text)
     (pcase-let ((`(,x ,y ,fs) (sketch-prop-vals props
                                                 'x 'y 'font-size))
                 (text-length (length text)))
       (list x y (* text-length (/ fs 1.6)) fs)))))

(defun sketch-rot-2d (x y angle &optional deg)
  (let ((angle (if deg
                   (degrees-to-radians deg)
                 angle)))
    (cons (- (* x (cos angle)) (* y (sin angle)))
          (+ (* x (sin angle)) (* y (cos angle))))))

(defun sketch--object-bbox-transform (object)
  (let* ((area (sketch-bbox-ex-transform object))
         (transform (sketch-parse-transform-string (dom-attr object 'transform)))
         (x1 (cl-first area))
         (y1 (cl-second area))
         (x2 (cl-third area))
         (y2 (cl-fourth area))
         (cx (/ (+ x1 x2) 2)) ; object x center
         (cy (/ (+ y1 y2) 2))
         (x-rad (abs (- cx x1))) ; object half-width
         (y-rad (abs (- cy y1))))
    (dolist (t-vals transform) ;; TODO maybe order first (rotate before
                               ;; translate etc.), check how this is implemented in
                               ;; SVG
      (pcase (car t-vals)
        ('translate (cl-incf  (cl-second t-vals))
                    (when (cl-third t-vals)
                      (cl-incf y1 (cl-third t-vals))))
        ;; To determine the bounding box after a rotation, we separate the
        ;; rotation in a translation of the center (rotation about a 'pivot) of
        ;; the bbox plus a rotation of the bbox around its center. Because the
        ;; bounding box always stays 'upright' (a mouse drag rectangle never
        ;; rotates), we can get the new bounding box by considering how much its
        ;; grows/expands through rotation around its 'translated' center.
        ('rotate (let* ((rad (degrees-to-radians (nth 1 t-vals)))
                        (px (or (nth 2 t-vals) 0)) ; pivot x position
                        (py (or (nth 3 t-vals) 0))
                        (vpx (- cx px)) ; vector pivot to center
                        (vpy (- cy py))
                        (vp-new (sketch-rot-2d vpx vpy rad))
                        (c-new (cons (+ px (car vp-new)) (+ py (cdr vp-new))))
                        (vx x-rad) ;vector-x center to bbox corner
                        (vy y-rad)
                        (v-new (sketch-rot-2d vx vy rad)))
                   (print vp-new)
                   (print (list (- (car vp-new) (car v-new)) (- (cdr vp-new) (cdr v-new))
                                (+ (car vp-new) (car v-new)) (+ (cdr vp-new) (cdr v-new))))))
        ('scale (let* ((new-x-rad (* (nth 1 t-vals) x-rad))
                       (new-y-rad (when-let (sy (nth 2 t-vals))
                                    (* (nth 2 t-vals) y-rad)))
                       (new-y1 (if new-y-rad (- cy new-y-rad) y1))
                       (new-y2 (if new-y-rad (+ cy new-y-rad) y2)))
                  (print (list (- cx new-x-rad) new-y1 (+ cx new-x-rad) new-y2))))))))


;; (defun sketch-bbox ()
;;   (let (())))

(defun sketch--svg-rotate (dt pivot &optional object-def)
  (interactive)
  (let* ((transform (sketch-parse-transform-string
                    (or (dom-attr object-def 'transform)
                        "rotate(0 0 0)")))
         (bbox (print (sketch-bbox-ex-transform object-def)))
        (pivot (if (eq pivot 'center)
                   (cons (/ (+ (nth 2 bbox) (nth 0 bbox)) 2)
                         (/ (+ (nth 3 bbox) (nth 1 bbox)) 2))
               pivot)))
    (cl-decf (cl-first (alist-get 'rotate transform)) dt)
    (when pivot
      (setf (cl-second (alist-get 'rotate transform)) (car pivot))
      (setf (cl-third (alist-get 'rotate transform)) (cdr pivot)))
    (print object-def)
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))
    (print object-def)))

(defun sketch-rotate (deg &optional lisp-buffer)
  (interactive)
  (let ((node (car (dom-by-id sketch-svg (car sketch-selection)))))
    (sketch--svg-rotate deg 'center node)
    (sketch-redraw)
    (when lisp-buffer
      (sketch-update-lisp-window))))

(defun sketch-rotate-by-5 (&optional arg)
  (interactive)
  (let ((node (car (dom-by-id sketch-svg (car sketch-selection)))))
    (sketch--svg-rotate (if arg -5 5) 'center node)
    (sketch-redraw)))

(defun sketch-rotate-by-min-5 ()
  (interactive)
  (sketch-rotate-by-5 t))

(defun sketch--svg-translate (dx dy &optional object-def)
  (interactive)
  (let ((transform (sketch-parse-transform-string
                    (or (dom-attr object-def 'transform)
                        "translate(0,0)"))))
    (cl-decf (cl-first (alist-get 'translate transform)) dx)
    (cl-decf (cl-second (alist-get 'translate transform)) dy)
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))))

(defun sketch--svg-translate (dx dy &optional object-def)
  (interactive)
  (let ((transform (sketch-parse-transform-string
                    (or (dom-attr object-def 'transform)
                        "translate(0,0)"))))
    (cl-decf (cl-first (alist-get 'translate transform)) dx)
    (cl-decf (cl-second (alist-get 'translate transform)) dy)
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))))

(defun sketch--svg-move (dx dy &optional object-def start-node)
  (interactive)
  (let ((transform (sketch-parse-transform-string
                    (if start-node
                        start-node
                      "translate(0,0)"))))

    ;;  (or (print (dom-attr start-node 'transform))
    ;;      "translate(0,0)"))
    (cond
     ;; (end
     ;;  (cl-incf (cl-first (alist-get 'translate transform))  dx)
     ;;  (cl-incf (cl-second (alist-get 'translate transform)) dy))
     (t
      (cl-incf (cl-first (alist-get 'translate transform))  dx)
      (cl-incf (cl-second (alist-get 'translate transform)) dy)))
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))
    start-node))

(defun sketch-group-scale (buffer object-def direction &optional fast)
  (let ((transform (sketch-parse-transform-string
                    (dom-attr object-def
                              'transform)))
        (amount (if fast
                    1
                  0.1)))
    (unless (alist-get 'scale transform)
      (push '(scale 1) transform))
    (pcase direction
      ('up (cl-incf (car (alist-get 'scale transform)) amount))
      ('down (cl-decf (car (alist-get 'scale transform)) amount)))
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))
    (sketch-redraw object-def buffer)))

(define-minor-mode sketch-lisp-mode
  "Minor mode for svg lisp buffers."
  :lighter "sketch"
  :keymap
  `((,(kbd "C-c C-s") . sketch-transient)
    (,(kbd "C-c C-c") . sketch-load-definition)))

(defun sketch-show-definition ()
  ;; :transient 'transient--do-exit
  (interactive)
  (when (get-buffer "*sketch-toolbar*")
    (kill-buffer "*sketch-toolbar*"))
  (if-let (win (get-buffer-window "*sketch-root*"))
      (delete-window win)
    (let ((buffer (get-buffer-create "*sketch-root*"))
          (sketch sketch-root))
      (set-window-dedicated-p
       (get-buffer-window (pop-to-buffer
                           buffer
                           `(display-buffer-in-side-window
                             . ((side . right)
                                (window-width . ,(funcall sketch-side-window-max-width))))))
       t)
      (window-resize (get-buffer-window buffer) -3 t)
      (erase-buffer)
      (with-current-buffer buffer
        (dom-pp sketch)))
    (emacs-lisp-mode)
    (sketch-lisp-mode)))

(defun sketch-load-definition ()
  (interactive)
  (let ((def (read (buffer-string))))
    (with-current-buffer "*sketch*"
      (setq sketch-root def)
      (setq sketch-layers-list (dom-by-id sketch-root "layer"))
      (sketch-redraw))))

(defun sketch-modify-object (&optional group)
  (interactive)
  (let ((show-labels sketch-show-labels))
    (setq sketch-show-labels "all")
    (sketch-toolbar-refresh)
    (sketch-redraw)
    (let* ((object-label (if group
                             group
                           (completing-read "Transform element with id: "
                                            (sketch-labels-list))))
           (buffer (get-buffer-create (format "*sketch-object-%s*" object-label))))
      (setq sketch-selection (list object-label))
      (display-buffer
       buffer
       `(display-buffer-in-side-window
         . ((side . right)
            (window-width . ,(funcall sketch-side-window-max-width)))))
      (window-resize (get-buffer-window buffer) -3 t)
      (pp (cadar (dom-by-id sketch-svg (format "^%s$" object-label))) buffer)
      (setq sketch-action 'translate)
      (with-current-buffer buffer
        (emacs-lisp-mode))
      (setq sketch-lisp-buffer-name buffer))
    (setq sketch-show-labels show-labels)
    (sketch-toolbar-refresh)
    (sketch-redraw)))

(defun sketch-update-lisp-window (lisp buffer)
  ;; (let ((sketch sketch-root))
  (with-current-buffer buffer
    (erase-buffer)
    (pp lisp (current-buffer))
    (goto-char (point-max)))
  (setq sketch-lisp-buffer-name buffer))

(defun sketch-remove-object ()
  (interactive)
  (let ((show-labels sketch-show-labels))
    (setq sketch-show-labels "all")
    (sketch-toolbar-refresh)
    (sketch-redraw)
    (svg-remove sketch-root (completing-read "Remove element with id: "
                                             (sketch-labels-list)))
    (setq sketch-show-labels show-labels)
    (sketch-toolbar-refresh)
    (sketch-redraw)))

;;; Web/SVG colors
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

(defun sketch-crop (event)
  "Crop the image to selection.
Translate the svg-root via its transform attribute and resizes
the canvas.

Because the grid is implemented as a pattern on the background
rectangle, the corners of the cropping area should coincide with
major-grid nodes if the object should stay aligned with the
grid (using snap to grid)."
  (interactive "@e")
  (let* ((start (event-start event))
         (snap sketch-snap-to-grid)
         (start-coords (if (or (not snap) (string= snap "nil"))
                           (posn-object-x-y start)
                         (sketch--snap-to-grid (posn-object-x-y start) sketch-minor-grid-param)))
         (end (event-end event))
         (end-coords (if (or (not snap) (string= snap "nil"))
                         (posn-object-x-y end)
                       (sketch--snap-to-grid (posn-object-x-y end) sketch-minor-grid-param)))
         (new-width (abs (- (car end-coords) (car start-coords))))
         (new-height (abs (- (cdr end-coords) (cdr start-coords)))))
    ;; (dom-set-attribute sketch-svg 'viewBox (format "%s %s %s %s"
    ;;                                                (car start-coords)
    ;;                                                (cdr start-coords)
    ;;                                                (car end-coords)
    ;;                                                (cdr end-coords)))
    (sketch--create-canvas new-width new-height)
    ;; (svg-marker sketch-canvas "arrow" 8 8 "black" t)
    ;; (svg-rectangle sketch-canvas 0 0 new-width new-height :fill "white")
    (sketch--svg-translate (car start-coords) (cdr start-coords) sketch-root)
    (sketch-redraw)))

(defun sketch-undo (&optional count)
  (interactive)
  ;; (let ((inhibit-read-only t))
  (cond ((require 'undo-tree nil t)
         (undo-tree-undo))
        ((require 'evil-commands nil t)
         (with-no-warnings (evil-undo count)))
        (t (undo)))
  ;; )
  (setq sketch-svg (read (buffer-string)))
  (setq sketch-root (car (dom-by-id sketch-svg "root")))
  (setq sketch-layers-list (dom-elements sketch-root 'id "layer"))
  (unless sketch-layers-list (call-interactively #'sketch-add-layer)))

(defun sketch-redo (&optional count)
  (interactive)
  (let ((inhibit-read-only t))
    (cond ((fboundp 'evil-undo)
           (undo-tree-redo))
          ((fboundp 'undo-tree-undo)
           (with-no-warnings (evil-redo count)))
          (t (user-error "This command requires `undo-tree' or `evil-commands' to be available"))))
  (setq sketch-root (read (buffer-string)))
  (setq sketch-layers-list (dom-elements sketch-root 'id "layer"))
  (unless sketch-layers-list (call-interactively #'sketch-add-layer)))

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


;;; Configuration
(defun sketch-set-action ()
  (interactive)
  (setq sketch-action
        (intern
         (let ((read-answer-short t))
           (read-answer "Select object: "
                        '(("freehand"  ?f "draw freehand with mouse drag")
                          ("line"      ?l "draw line with mouse drag")
                          ("rectangle" ?r "draw rectangle with mouse drag")
                          ("circle"    ?c "draw circle with mouse drag")
                          ("ellipse"   ?e "draw-ellipse with mouse drag")
                          ("polyline"  ?p "draw polyline by clicking. Double click to insert end.")
                          ("polygon"   ?g "draw polygon by clicking. Double click to insert end.")
                          ("select"    ?s "select objects")
                          ;; ("move"      ?m "move selected objects")
                          ;; ("translate" ?t "translate selected objects")
                          )))))
  (sketch-toolbar-refresh))

(defun sketch-set-colors (&optional arg)
  "Set stroke, fill or both colors simultaneously.
With single prefix ARG, set fill color. With double prefix ARG,
set stroke and fill color simultaneously. Otherwise set stroke
color."
  (interactive "p")
  (let ((color (substring-no-properties (read-color-web "Select color: ")))
        (fns (pcase arg
               (1 '(sketch-stroke-color))
               (4 '(sketch-fill-color))
               (16 '(sketch-stroke-color
                     sketch-fill-color)))))
    (dolist (fn fns)
      (set fn color)))
  (sketch-toolbar-refresh))

(defun sketch-set-font-color ()
  (interactive)
  (sketch-set-colors 16))

(defun sketch-set-font-with-keyboard (arg)
  (interactive "P")
  (if arg
      (sketch-set-font)
    (completing-read "Select font: " (font-family-list))))

;; TODO merge with `sketch-set-font-size' (using `arg')
(defun sketch-set-font-size-by-keyboard ()
  (interactive)
  (setq sketch-font-size (string-to-number
                          (completing-read "Select font size: " (number-sequence 8 60)))))

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
  (pop-to-buffer "*sketch-fonts*" '(display-buffer-reuse-mode-window (mode . special-mode)))
  (let ((button-width (* 4 5 (default-font-width)))
        (button-height (* 2 (default-font-height)))
        (counter 0))
    (dolist (x (sort (seq-uniq (font-family-list)) #'string-lessp))
      (insert-text-button x
                          'action
                          (lambda (button) (interactive)
                            (setq sketch-font (button-label button))
                            (kill-buffer)
                            (sketch-toolbar-refresh))
                          'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                (svg-rectangle svg 0 0 button-width button-height
                                                               :fill "white")
                                                (svg-text svg "ABC abc"
                                                          :font-size button-height
                                                          :font-family x
                                                          :stroke "black"
                                                          :fill "black"
                                                          :x 4
                                                          :y (- button-height 4))
                                                svg)))
      (insert " ")
      (insert x)
      (setq counter (1+ counter))
      (if (/= counter 2)
          (insert (make-string
                   (- 30 (length x)) (string-to-char " ")))
        (insert "\n\n")
        (setq counter 0)))
    (goto-char (point-min))
    (special-mode)))

(defun sketch-set-font-size ()
  (interactive)
  (pop-to-buffer "*sketch-font-sizes*" '(display-buffer-reuse-mode-window (mode . special-mode)))
  (let ((inhibit-read-only t))
    (dolist (x (nreverse '(8 10 12 14 16 20 24 28 32 40 48 56 64 80 96 112 128 160 192)))
      (let ((button-width (/ (* 8 x) 3))
            (button-height x)
            (s (number-to-string x)))
        (insert-text-button s
                            'action
                            (lambda (button) (interactive)
                              (setq sketch-font-size (string-to-number (button-label button)))
                              (kill-buffer)
                              (sketch-toolbar-refresh))
                            'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                  (svg-rectangle svg 0 0 button-width button-height
                                                                 :fill "white")
                                                  (svg-text svg "Aa"
                                                            :font-size button-height
                                                            :font-family sketch-font
                                                            :stroke "black"
                                                            :fill "black"
                                                            :x 4
                                                            :y (- button-height 4))
                                                  svg)))
        (insert " ")
        (insert s)
        (insert "\n"))
      (goto-char (point-min)))))

(defun sketch-set-color ()
  (interactive)
  (pop-to-buffer "*sketch-color*" '(display-buffer-reuse-mode-window (mode . special-mode)))
  (let ((inhibit-read-only t))
    (dolist (x shr-color-html-colors-alist)
      (let ((button-width (/ (* 8 x) 3))
            (button-height x)
            (s (number-to-string x)))
        (insert-text-button s
                            'action
                            (lambda (button) (interactive)
                              (setq sketch-font-size (string-to-number (button-label button)))
                              (kill-buffer)
                              (sketch-toolbar-refresh))
                            'display (svg-image (let ((svg (svg-create button-width button-height)))
                                                  (svg-rectangle svg 0 0 button-width button-height
                                                                 :fill "white")
                                                  (svg-text svg "Aa"
                                                            :font-size button-height
                                                            :font-family sketch-font
                                                            :stroke "black"
                                                            :fill "black"
                                                            :x 4
                                                            :y (- button-height 4))
                                                  svg)))
        (insert " ")
        (insert s)
        (insert "\n"))
      (goto-char (point-min)))))

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

(defun sketch-cycle-labels ()
  (interactive)
  (setq sketch-show-labels (pcase sketch-show-labels
                             ("layer" "all")
                             ("all" nil)
                             (_ "layer")))
  (sketch-redraw)
  (sketch-toolbar-refresh))

(defun sketch-toggle-coords ()
  (interactive)
  (setq sketch-show-coords (if sketch-show-coords nil t))
  (if (not sketch-show-coords)
      (setq mode-line-format sketch-coordless-mode-line-format)
    (setq sketch-coordless-mode-line-format mode-line-format)
    (add-to-list 'mode-line-format '(:eval sketch-cursor-position) t)))



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
  (if (> emacs-major-version 27)
      (describe-keymap 'sketch-mode-map)
    (let ((help-window-select t))
      (describe-bindings)
      (search-forward "sketch-mode"))))


;;; Toolbar
(defun sketch-toolbar-refresh ()
  (with-current-buffer (get-buffer "*sketch-toolbar*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Press . for key hints or press ? for help\n\n" 'face 'bold))
      (sketch-toolbar-colors)
      (insert "\n\n")
      (sketch-toolbar-widths)
      (insert "\n")
      (sketch-toolbar-objects)
      (insert "\n\n")
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
                                        `((side . right)
                                          (window-width . ,(funcall sketch-side-window-max-width))))
         t)
        (window-resize (get-buffer-window buffer) -3 t)
        (with-current-buffer buffer
          (setq cursor-type nil)
          (special-mode))
        (sketch-toolbar-refresh)))))

(defun sketch-toolbar-colors ()
  ;; STROKE COLOR
  (insert "STROKE COLOR: ")
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
                              (sketch-toolbar-refresh))
                            'face (list
                                   :background (alist-get color
                                                          shr-color-html-colors-alist
                                                          nil nil 'string=)))
      (setq counter (1+ counter))
      (if (not (= counter 8))
          (insert " ")
        (insert "\n\n")
        ;; (when (= counter 8)
        ;;   (insert "\n")
        (setq counter 0))))

  (insert "\n")

  ;; FILL COLOR
  (insert "FILL COLOR: ")
  (apply #'insert-text-button "   "
         'action
         (lambda (_) (interactive)
           (message sketch-fill-color))
         (pcase sketch-fill-color
           ("none" nil)
           (_ (list 'face (when sketch-fill-color
                            (list :background (alist-get sketch-fill-color
                                                         shr-color-html-colors-alist
                                                         nil nil 'string=)))))))
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
                                  (car (rassoc
                                        (plist-get (button-get button 'face) :background)
                                        shr-color-html-colors-alist)))
                            (sketch-toolbar-refresh))
                          'face (list
                                 :background (alist-get color
                                                        shr-color-html-colors-alist
                                                        nil nil 'string=)))
      (setq counter (1+ counter))
      (if (not (= counter 8))
          (insert " ")
        (insert "\n\n")
        (setq counter 0))))
  (insert (propertize "More colors? Press (C-u) c" 'face 'bold)))

(defun sketch-toolbar-widths ()
  (insert "STROKE WIDTH: ")
  (insert (number-to-string sketch-stroke-width))
  (insert "\n")
  (let* ((widths 12)
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
                                                  (svg-line svg 5 stroke-height
                                                            (- button-width 5) stroke-height
                                                            :stroke "black" :stroke-width (1+ w))
                                                  svg)))
        (setq counter (1+ counter))
        (if (not (= counter 6))
            (insert " ")
          (insert "\n\n")
          (setq counter 0))))))

(defun sketch-toolbar-objects ()
  (insert "MOUSE ACTION\n")
  (insert "draw\n")
  (let ((objects '(line polyline circle ellipse rectangle polygon freehand text)))
    (let ((counter 0))
      (while objects
        (let ((o (car objects)))
          (apply #'insert-text-button
                 (symbol-name o)
                 'action (lambda (button) (interactive)
                           (setq sketch-action (intern (button-label button)))
                           (sketch-toolbar-refresh))
                 (when (eq o sketch-action)
                   (list 'face 'link-visited)))
          (setq counter (1+ counter))
          (cond ((/= counter 4)
                 (dotimes (_ (- 10 (length (symbol-name o))))
                   (insert " ")))
                ;; (let ((o (cadr objects)))
                ;;   (apply #'insert-text-button
                ;;          (symbol-name o)
                ;;          'action (lambda (button) (interactive)
                ;;                    (setq sketch-action (intern (button-label button)))
                ;;                    (sketch-toolbar-refresh))
                ;;          (when (eq o sketch-action)
                ;;            (list 'face 'link-visited))))
                ;; ;; (list 'face (if (eq o sketch-action)
                ;; ;;                 'widget-button-pressed
                ;; ;;               'widget-button)))
                (t
                 (insert "\n")
                 (setq counter 0)))
          (setq objects (cdr objects))))))
  (insert "Text can always be added by clicking mouse-3")
  (insert "\n\n")
  (insert "edit\n")
  (dolist (e '(select move translate))
    (apply #'insert-text-button
           (symbol-name e)
           'action (lambda (button) (interactive)
                     ;; (setq sketch-action (intern (button-label button)))
                     (pcase (intern (button-label button))
                       ('select (user-error "Feature not yet implemented, instead press `v' to select with keyboard"))
                       ((or 'move 'translate) (user-error "Feature not yet implemented, instead press `m' to select and translate")))
                     (sketch-toolbar-refresh))
           (when (eq e sketch-action)
             (list 'face 'link-visited)))
    (insert " ")
    ))

(defun sketch-toolbar-toggles ()
  (insert "TOGGLES\n")
  (insert "Grid: ")
  (apply #'insert-text-button (if sketch-show-grid "show" "hide")
         'action
         (lambda (_) (interactive)
           (sketch-toggle-grid)
           (sketch-toolbar-refresh))
         (when sketch-show-grid
           (list 'face 'link-visited)))
  ;; (list 'face (if sketch-grid
  ;;                 'widget-button-pressed
  ;;               'widget-button)))
  (insert "   ")
  (insert "Snap: ")
  (apply #'insert-text-button (if sketch-snap-to-grid "on" "off")
         'action
         (lambda (_) (interactive)
           (sketch-toggle-snap)
           (sketch-toolbar-refresh))
         (when sketch-snap-to-grid
           (list 'face 'link-visited)))
  (insert "   ")
  (insert "Labels: ")
  (apply #'insert-text-button (or sketch-show-labels "hide")
         'action
         (lambda (_) (interactive)
           (sketch-cycle-labels)
           (sketch-toolbar-refresh))
         (when sketch-show-labels
           (list 'face 'link-visited))))
;; (list 'face (if sketch-snap-to-grid
;;                 'widget-button-pressed
;;               'widget-button))))

(defun sketch-toolbar-font ()
  (interactive)
  (insert "FONT\n")
  (insert "family: ")
  (if sketch-font
      (let ((button-width (* 2 5 (default-font-width)))
            (button-height (default-font-height)))
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
  (insert"   ")
  (insert "Size: ")
  (insert-text-button (number-to-string sketch-font-size)
                      'action
                      (lambda (_) (interactive)
                        (sketch-set-font-size))))

(defun sketch-kill-toolbar ()
  (let ((toolbar (get-buffer "*sketch-toolbar*")))
    (when toolbar
      (kill-buffer toolbar))))


(provide 'sketch-mode)
;;; sketch-mode.el ends here
