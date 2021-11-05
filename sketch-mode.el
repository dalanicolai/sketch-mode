;;; sketch-mode.el --- Quickly create svg sketches using keyboard and mouse -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: D.L. Nicolai <dalanicolai@gmail.com>
;; Created: 17 Jul 2021
;; Version: 1.0.4

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
(require 'cl-lib)

(eval-when-compile
  (require 'evil-vars nil t))
(require 'undo-tree nil t)


(defvar sketch-svg nil)
(defvar sketch-size '(1200 . 900))
(defvar sketch-grid nil)
(defvar sketch-show-grid t)
(defvar sketch-background "white")
(defvar sketch-grid-param 100)
(defvar sketch-minor-grid-param nil)
(defvar sketch-minor-grid-freq 5)
(defvar sketch-grid-colors '("black" . "black"))
(defvar sketch-default-stroke "black")
(defvar sketch-snap-to-grid t)

(defvar sketch-canvas nil)
(defvar sketch-root nil)

(defvar sketch-action 'line)
(defvar sketch-stroke-color "Black")
(defvar sketch-fill-color "none")
(defvar sketch-opacity nil)
(defvar sketch-stroke-width 1)
(defvar sketch-stroke-dasharray nil)

(defvar sketch-font nil)
(defvar sketch-font-size 20)
(defvar sketch-font-weight "normal")

(defvar sketch-slider nil)

(defvar sketch-bboxes nil)
(defvar sketch-selection nil)

(defvar sketch-active-layer 0)
(defvar sketch-layers-list nil)
(defvar show-layers nil)

(defvar sketch-show-labels nil)
(defvar sketch-label-size 15)

(defvar sketch-call-buffer nil)

(defvar sketch-snippet nil)

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

;;; Temporary code

;; Overwrite default function until patch in
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=51371 to core is applied
(defun list-colors-display (&optional list buffer-name callback)
  "Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list of
colors that the current display can handle.  Customize
`list-colors-sort' to change the order in which colors are shown.
Type \\<help-mode-map>\\[revert-buffer] after customizing \
`list-colors-sort' to redisplay colors in the new order.

If the optional argument BUFFER-NAME is nil, it defaults to \"*Colors*\".

If the optional argument CALLBACK is non-nil, it should be a
function to call each time the user types RET or clicks on a
color.  The function should accept a single argument, the color name."
  (interactive)
  (when (> (display-color-cells) 0)
    (setq list (list-colors-duplicates (or list (defined-colors))))
    (when list-colors-sort
      ;; Schwartzian transform with `(color key1 key2 key3 ...)'.
      (setq list (mapcar
		  'car
		  (sort (delq nil (mapcar
				   (lambda (c)
				     (let ((key (list-colors-sort-key
						 (car c))))
				       (when key
					 (cons c (if (consp key) key
						   (list key))))))
				   list))
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
    (when (memq (display-visual-class) '(gray-scale pseudo-color direct-color))
      ;; Don't show more than what the display can handle.
      (let ((lc (nthcdr (1- (display-color-cells)) list)))
	(if lc
	    (setcdr lc nil)))))
  (unless buffer-name
    (setq buffer-name "*Colors*"))
  (with-help-window buffer-name
    (with-current-buffer standard-output
      (erase-buffer)
      (list-colors-print list callback)
      (set-buffer-modified-p nil)
      (setq truncate-lines t)
      (setq-local list-colors-callback callback)
      (setq revert-buffer-function 'list-colors-redisplay)))
  (when callback
    (pop-to-buffer buffer-name)
    (message "Click on a color to select it.")))

;;; TODO Some snippet for dom.el?
(defun sketch-set-attrs (node &rest attributes)
  "Set selected attributes (symbols) of NODE to ATTRIBUTES."
  (setq node (dom-ensure-node node))
  (let ((props (cadr node)))
    (while attributes
      (let ((attr (assoc (car attributes) props)))
        (if attr
            (setcdr attr (cadr attributes))
          (setq props (append props
                              (list (cons (car attributes) (cadr attributes)))))))
      (setq attributes (cddr attributes)))
    (setcdr node (list props))))

;;; Rendering

;;; TODO Some snippets for svg.el?
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

(defun sketch-insert-image (svg &optional string &rest props)
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
              ('line (sketch-label-text-node
                      node
                      (dom-attr node 'x1)
                      (dom-attr node 'y1)))
              ((or 'circle 'ellipse)
               (sketch-label-text-node
                node
                (dom-attr node 'cx)
                (dom-attr node 'cy)))
              ((or 'polyline 'polygon)
               (let ((coords (split-string
                              (car (split-string (dom-attr node 'points) ","))
                              nil
                              t)))
                 (sketch-label-text-node
                  node
                  (string-to-number (car coords))
                  (string-to-number (cadr coords)))))
              ('text (sketch-label-text-node
                      node
                      (dom-attr node 'x)
                      (+ (dom-attr node 'y)
                         sketch-label-size)))
              ('g (let ((s (dom-attr node
                                     'transform)))
                    (string-match "translate\(\\([0-9]*\\)[, ]*\\([0-9]*\\)" s)
                    (let ((x (match-string 1 s))
                          (y (match-string 2 s)))
                      (sketch-label-text-node
                       node
                       x
                       y))))
              ))
          nodes))
    svg-labels))

(defun sketch-selections ()
  (let* ((selections (sketch-group "Selections"))
         (bbox (sketch-bbox-ex-transform (car (dom-by-id sketch-root (car sketch-selection)))))
         (start-coords (cons (nth 0 bbox) (nth 1 bbox)))
         (end-coords (cons (nth 2 bbox) (nth 3 bbox))))
    (apply #'svg-rectangle selections `(,@(sketch--rectangle-coords start-coords end-coords)))
    selections))


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
                           ('group "g")
                           ('snippet "s"))))
         (idx 0)
         (label (concat prefix (number-to-string idx)))
         (labels (sketch-labels-list)))
    (while (member label labels)
      (setq idx (1+ idx))
      (setq label (concat prefix (number-to-string idx))))
    label))

(defun sketch--create-canvas (width height)
  (setq sketch-canvas (sketch-create width height nil nil nil))
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
                                                 (stroke-width . 0.4) (stroke . ,(car sketch-grid-colors))
                                                 (fill . "url(#minorGrid)"))))
                     ;; minor grid
                     (dom-node 'pattern
                               `((id . "minorGrid")
                                 (width . ,sketch-minor-grid-param)
                                 (height . ,sketch-minor-grid-param)
                                 (patternUnits . "userSpaceOnUse"))
                               (dom-node 'rect `((width . ,sketch-minor-grid-param) (height . ,sketch-minor-grid-param)
                                                 (x . 0) (y . 0)
                                                 (stroke-width . 0.2) (stroke . ,(cdr sketch-grid-colors))
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
  (pcase-let ((`(,x1 . ,y1) start-coords) ; used in both 'line and 'translate
              (`(,x2 . ,y2) end-coords))
    (pcase object-type
      ('line
       (sketch-set-attrs node
                    'x2 x2
                    'y2 y2))
      ('rectangle
       (pcase-let ((`(,x ,y ,w ,h) (sketch--rectangle-coords start-coords end-coords)))
         (sketch-set-attrs node
                      'x x
                      'y y
                      'width w
                      'height h)))
      ('circle
       (sketch-set-attrs node 'r (sketch--circle-radius start-coords end-coords)))
      ('ellipse
       (pcase-let ((`(,cx ,cy ,rx ,ry) (sketch--ellipse-coords start-coords end-coords)))
         (sketch-set-attrs node
                      'cx cx
                      'cy cy
                      'rx rx
                      'ry ry)))
      ('translate
       (message "deze %s" start-node)
       (let ((dx (- x2 x1))
             (dy (- y2 y1)))
         (sketch--svg-move dx dy node start-node))))))

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
                           (when sketch-show-labels (list (sketch-labels)))
                           (when sketch-selection (list (sketch-selections)))))
  (svg--def sketch-svg
            '(style ((type . "text/css")) "<![CDATA[#Selections
{ stroke: DeepSkyBlue; stroke-width: 2; fill: none; stroke-dasharray: 8 8; }]]>"))
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
                   ([sketch S-down-mouse-1] . sketch-select)
                   ([sketch triple-mouse-4] . sketch-rotate-by-5)
                   ([sketch triple-mouse-5] . sketch-rotate-by-min-5)
                   ("a" . sketch-set-action)
                   ("cs" . sketch-set-colors)
                   ("cf" . sketch-set-fill-color)
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
                   ("X" . sketch-show-xml)
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

(defun sketch-quit-side-windows ()
  "Quit sketch window. The window can be restores with ‘M-x sketch'"
  (when (eq major-mode 'sketch-mode)
    (when (get-buffer "*sketch-toolbar*")
      (kill-buffer "*sketch-toolbar*"))
    (when (get-buffer "*sketch-key-hints*")
      (kill-buffer "*sketch-key-hints*"))))

(add-hook 'quit-window-hook 'sketch-quit-side-windows)

;; TODO format/propertize key hints
(defun sketch-toggle-key-hints (&optional show)
  "Toggle key-hints, when SHOW non-nil then show key-hints."
  (interactive)
  (let ((win (get-buffer-window "*sketch-key-hints*")))
    (if win
        (unless show (delete-window win))
      (let* ((window-sides-vertical t)
            (buffer (get-buffer-create "*sketch-key-hints*"))
            (win (display-buffer-in-side-window (get-buffer-create "*sketch-key-hints*")
                                                   `((side . bottom)
                                                     (slot . -1)
                                                     (window-height . 11)))))
        (set-window-dedicated-p win t)
        (set-window-parameter win 'no-other-window t)
        (with-current-buffer buffer
          (insert
           "Stroke/Fill           Font              Edit                 Toggle          Definition
-------------------------------------------------------------------------------------------------------------------------------
[a] : action          [fw]: font        [m]  : modify object [tg]: grid      [D]      : Show definition
[cs]: stroke-color    [fs]: font-size   [v]  : select        [ts]: snap      [X]      : Show xml
[cf]: fill-color      [fc]: font-color  [d]  : delete        [tt]: toolbar   [C-c C-c]: Quick insert to call buffer
[w] : width                             [u/U]: undo/redo     [tc]: coords
[sd]: dasharray

[down-mouse-1] main action, [down-mouse-3] add text ,[C-S drag-mouse-1] crop image, [sketch triple-mouse-4/5] rotate selection")
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
           (sketch-toggle-toolbar t)
           (sketch-toggle-key-hints t)
           )
          (t
           (let ((call-buffer (current-buffer))
                 (width (if arg (car sketch-size) (read-number "Enter width: ") ))
                 (height (if arg (cdr sketch-size) (read-number "Enter height: "))))
             (switch-to-buffer (get-buffer-create "*sketch*"))
             (setq sketch-action 'line)
             (setq sketch-grid-param (if arg sketch-grid-param (read-number "Enter grid parameter (enter 0 for no grid): ")))
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
  (pcase-let ((`(,xs . ,ys) start-coords) ; used in both 'line and 'translate
              (`(,xe . ,ye) end-coords))
    (sketch-norm (list (- xe xs) (- ye ys)))))

(defun sketch--rectangle-coords (start-coords end-coords)
  (let ((x (apply #'min (list (car start-coords) (car end-coords))))
        (y (apply #'min (list (cdr start-coords) (cdr end-coords))))
        (w (abs (- (car end-coords) (car start-coords))))
        (h (abs (- (cdr end-coords) (cdr start-coords)))))
    (list x y w h)))

(defun sketch--ellipse-coords (start-coords end-coords)
  (list (/ (+ (car start-coords) (car end-coords)) 2)
        (/ (+ (cdr start-coords) (cdr end-coords)) 2)
        (abs (/ (- (car end-coords) (car start-coords)) 2))
        (abs (/ (- (cdr end-coords) (cdr start-coords)) 2))))

(defun sketch--snap-to-grid (coord grid-param)
  (cons (* (round (/ (float (car coord)) grid-param)) grid-param)
        (* (round (/ (float (cdr coord)) grid-param)) grid-param)))

(defun sketch-absolute-coords (svg coords &optional grid-param)
  (pcase-let* ((w (dom-attr svg 'width))
               (h (dom-attr svg 'height))
               (`(,dx ,dy ,vw ,vh) (mapcar #'string-to-number
                                           (split-string (dom-attr svg 'viewBox))))
               (scale-w (/ vw w))
               (scale-h (/ vh h))
               (abs-x (+ dx (* scale-w (car coords))))
               (abs-y (+ dy (* scale-h (cdr coords)))))
    (if grid-param
        (cons (* (round (/ abs-x grid-param)) grid-param)
              (* (round (/ abs-y grid-param)) grid-param))
      (cons abs-x abs-y))))

(defun sketch-pan (event)
  (interactive "@e")
  (let ((sketch-action 'pan))
    (sketch-interactively event)))

(defun sketch-interactively (event)
  "Draw objects interactively via a mouse drag EVENT. "
  (interactive "@e")
  (let* ((start (event-start event))
         (start-rel-coords (posn-object-x-y start))
         (start-coords (sketch-absolute-coords sketch-svg
                                               start-rel-coords
                                               (when sketch-snap-to-grid
                                                 sketch-minor-grid-param)))
         (xs (car start-coords))
         (ys (cdr start-coords))
         (points (list start-coords)) ;; list of point needed for polyline/gon
         (object-props (if (eq sketch-action 'text)
                           (append (list :font-size sketch-font-size
                                         :font-weight sketch-font-weight)
                                   (when sketch-font
                                     (list :font-family sketch-font))
                                   (when sketch-stroke-color
                                     (list :stroke sketch-stroke-color))
                                   (when sketch-fill-color
                                     (list :fill sketch-fill-color))
                                   (when sketch-opacity
                                     (list :opacity sketch-opacity)))
                         (append (list :stroke-width
                                       sketch-stroke-width
                                       :stroke
                                       sketch-stroke-color
                                       :fill
                                       sketch-fill-color
                                       :stroke-dasharray
                                       sketch-stroke-dasharray)
                                 (when sketch-opacity
                                   (list :opacity sketch-opacity))
                                ;; :marker-end (if args (pcase (transient-arg-value "--marker=" args)
                                ;;                        ("arrow" "url(#arrow)")
                                ;;                        ("dot" "url(#dot)")
                                ;;                        (_ "none"))
                                ;;               (if sketch-include-end-marker
                                ;;                   "url(#arrow)"
                                ;;                 "none"))
                                )))
         (start-command-and-coords (pcase sketch-action
                                     ('line (list 'svg-line xs ys xs ys))
                                     ('rectangle `(svg-rectangle
                                                   ,@(sketch--rectangle-coords start-coords start-coords)))
                                     ('circle (list 'svg-circle xs ys (sketch--circle-radius start-coords start-coords)))
                                     ('ellipse `(svg-ellipse ,@(sketch--ellipse-coords start-coords start-coords)))
                                     ((or 'polyline 'polygon 'freehand)
                                      (list (pcase sketch-action
                                              ((or 'polyline 'freehand) 'svg-polyline)
                                              ('polygon 'svg-polygon))
                                            points))))
         (label (unless (memq sketch-action '(select move translate pan))
                  (sketch-create-label sketch-action))))
    (pcase sketch-action
      ('text (let ((text (read-string "Enter text: ")))
               (apply #'svg-text
                      (nth sketch-active-layer sketch-layers-list)
                      text
                      :x xs
                      :y ys
                      :id label object-props)))
      (_ (unless (memq sketch-action '(select move translate))
           (apply (car start-command-and-coords)
                  (nth sketch-active-layer sketch-layers-list)
                  `(,@(cdr start-command-and-coords) ,@object-props :id ,label)))
         (let* ((node (unless (eq sketch-action 'select)
                        (car (dom-by-id (nth sketch-active-layer sketch-layers-list)
                                        (if (memq sketch-action '(move translate))
                                            (car sketch-selection)
                                          label)))))
                (translate-start (dom-attr node 'transform)))
           (track-mouse
             (pcase sketch-action
               ((or 'line 'rectangle 'circle 'ellipse 'move 'translate)
                (let ((event (read-event)))
                  (while (not (memq (car event) '(mouse-1 drag-mouse-1)))
                    (let* ((end (event-start event))
                           (end-rel-coords (posn-object-x-y end))
                           (end-coords (sketch-absolute-coords sketch-svg end-rel-coords
                                                               (when sketch-snap-to-grid
                                                                 sketch-minor-grid-param)))
                           (xe (car end-coords))
                           (ye (cdr end-coords)))
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
                        (setq sketch-cursor-position (format "(%s, %s)" xe ye)))
                      (sketch-maybe-update-modeline)
                      ))
                  (let* ((end (event-end event))
                         (end-rel-coords (posn-object-x-y end))
                         (end-coords (sketch-absolute-coords sketch-svg end-rel-coords
                                                             (when sketch-snap-to-grid
                                                               sketch-minor-grid-param)))
                         (xe (car end-coords))
                         (ye (cdr end-coords)))
                    (if (and (equal xs xe)  ; remove when object has no size
                             (equal ys ye)) ; TODO instead better only create object as soon as it has a size? But then still
                                                                          ; the object must be removed (either with automatically here, or manually by the user)
                                                                          ; when the user draws an object with no size
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
                                       (posn-object-x-y end)))
                         (xe (car end-coords))
                         (ye (cdr end-coords)))
                    (let (message-log-max)
                      (message "Press double click to finish by inserting a final node"))
                    (sketch-set-attrs node 'points (mapconcat (lambda (pair)
                                                               (format "%s %s" (car pair) (cdr pair)))
                                                             (reverse
                                                              (if (eq (car event) 'down-mouse-1)
                                                                  (push end-coords points)
                                                                (cons end-coords points)))
                                                             ", "))
                    (sketch-redraw nil nil t)
                    (when sketch-show-coords
                      (setq sketch-cursor-position (format "(%s, %s)" xe ye))
                      (sketch-maybe-update-modeline))))
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
                    (setq sketch-cursor-position (format "(%s, %s)" xe ye))
                    (sketch-maybe-update-modeline))))

               ('select (let* ((coords (posn-object-x-y (event-start event)))
                               (bboxes (seq-filter (lambda (x)
                                                     (sketch-within-bbox-ex-transform-p coords (cdr x)))
                                                   sketch-bboxes)))
                          (let ((next-selections (member (car sketch-selection) (mapcar #'car bboxes))))
                            (setq sketch-selection (if next-selections
                                                       (when-let (x (cadr next-selections)) (list x))
                                                     (when bboxes (list (caar bboxes)))))))))))))
    (setq sketch-bboxes (mapcar (lambda (x)
                                  (cons (dom-attr x 'id)
                                        (sketch-bbox-ex-transform x)))
                                (dom-children (nth sketch-active-layer sketch-layers-list))))
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
                  (mapcar #'string-to-number (split-string (cadr p) "[, ]" t))))
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
       (list (- cx r) (+ cx r) (- cy r) (+ cy r))))

    (`(ellipse ,props)
     (pcase-let ((`(,cx ,cy ,rx ,ry) (sketch-prop-vals props
                                                     'cx 'cy 'rx 'ry)))
       (print (list (- cx rx) (+ cx rx) (- cy ry) (+ cy ry)))))
    (`(polyline ,props)
     (pcase-let ((`(,points) (sketch-prop-vals props 'points)))
       (let ((coords (mapcar (lambda (x)
                               (mapcar #'string-to-number (split-string x)))
                             (split-string points ", " t))))
         (list (apply #'min (mapcar #'car coords))
               (apply #'min (mapcar #'cadr coords))
               (apply #'max (mapcar #'car coords))
               (apply #'max (mapcar #'cadr coords))))))
    (`(polygon ,props) ; body identical to polyline
     (pcase-let ((`(,points) (sketch-prop-vals props 'points)))
       (let ((coords (mapcar (lambda (x)
                               (mapcar #'string-to-number (split-string x)))
                             (split-string points ", " t))))
         (list (apply #'min (mapcar #'car coords))
               (apply #'min (mapcar #'cadr coords))
               (apply #'max (mapcar #'car coords))
               (apply #'max (mapcar #'cadr coords))))))
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

        ;; TODO correct following comment and 'case' (code); bbox should be
        ;; tightest fitting rectangle see URL
        ;; `https://svgwg.org/svg2-draft/coords.html#BoundingBoxes'

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
                        (c-new (cons (+ px (car vp-new)) (+ py (cdr vp-new)))) ; new center
                        (vx x-rad) ;vector-x center to bbox corner
                        (vy y-rad)
                        (v-new (sketch-rot-2d vx vy rad)))
                   (print vp-new)
                   (print (list (- (car c-new) (car v-new)) (- (cdr c-new) (cdr v-new))
                                (+ (car c-new) (car v-new)) (+ (cdr c-new) (cdr v-new))))))
        ('scale (let* ((new-x-rad (* (nth 1 t-vals) x-rad))
                       (new-y-rad (when-let (sy (nth 2 t-vals))
                                    (* (nth 2 t-vals) y-rad)))
                       (new-y1 (if new-y-rad (- cy new-y-rad) y1))
                       (new-y2 (if new-y-rad (+ cy new-y-rad) y2)))
                  (print (list (- cx new-x-rad) new-y1 (+ cx new-x-rad) new-y2))))))))


(defun sketch-within-bbox-ex-transform-p (coords bbox)
  (and (or (< (nth 0 bbox) (car coords) (nth 2 bbox))
           (< (nth 2 bbox) (car coords) (nth 1 bbox)))
       (or (< (nth 1 bbox) (cdr coords) (nth 3 bbox))
           (< (nth 3 bbox) (cdr coords) (nth 1 bbox)))))


(defun sketch--svg-rotate (dt pivot &optional object-def)
  (interactive)
  (let* ((transform (sketch-parse-transform-string
                    (or (dom-attr object-def 'transform)
                        "rotate(0 0 0)")))
         (bbox (sketch-bbox-ex-transform object-def))
        (pivot (if (eq pivot 'center)
                   (cons (/ (+ (nth 2 bbox) (nth 0 bbox)) 2)
                         (/ (+ (nth 3 bbox) (nth 1 bbox)) 2))
               pivot)))
    (cl-decf (cl-first (alist-get 'rotate transform)) dt)
    (when pivot
      (setf (cl-second (alist-get 'rotate transform)) (car pivot))
      (setf (cl-third (alist-get 'rotate transform)) (cdr pivot)))
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transform transform))))

;; (defun sketch-rotate (deg &optional lisp-buffer)
;;   (interactive)
;;   (let ((node (car (dom-by-id sketch-svg (car sketch-selection)))))
;;     (sketch--svg-rotate deg 'center node)
;;     (sketch-redraw)
;;     (when lisp-buffer
;;       (sketch-update-lisp-window))))

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

(defun sketch-show-xml ()
  ;; :transient 'transient--do-exit
  (interactive)
  (when (get-buffer "*sketch-toolbar*")
    (kill-buffer "*sketch-toolbar*"))
  (if-let (win (get-buffer-window "*sketch-xml*"))
      (delete-window win)
    (let ((buffer (get-buffer-create "*sketch-xml*"))
          (xml (image-property (get-text-property (point) 'display)
                               :data)))
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
        (insert xml)))
    (sgml-mode)
    (sgml-pretty-print (point-min) (point-max))))

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

(defun sketch-modify-object (&optional node)
  (interactive)
  (let ((show-labels sketch-show-labels))
    (setq sketch-show-labels "all")
    (sketch-toolbar-refresh)
    (sketch-redraw)
    (let* ((object-label (if node
                             node
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

(defun sketch-set-fill-color ()
  (interactive)
  (sketch-set-colors 4))

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
      (goto-char (point-min))
      (special-mode))))

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
         (lines-vectors-cross (pcase (sketch-coords-cross r s)
                                (0 (user-error "Lines are parallel"))
                                (val val)))
         (u (/ (float (sketch-coords-cross (sketch-coords-diff q p) r))
               lines-vectors-cross)))
    (sketch-coords-sum q (sketch-coords-scale s u))))

(defun sketch-calculate-angle-arc (line1-coords line2-coords arc-radius part)
  "Draw angle arc in positive angle direction."
  (goto-char (point-min))
  (let* ((image-size (image-size (get-char-property (point) 'display) t))
         (intersection (pcase (sketch-lines-intersection line1-coords line2-coords)
                         ((pred (lambda (x) (or (> (car x) (car image-size))
                                                (> (cdr x) (cdr image-size)))))
                          (user-error "Lines cross outside of image"))
                         (val val)))
         (slope1-angle (atan (/ (float (- (nth 3 line1-coords) (nth 1 line1-coords)))
                                (- (nth 2 line1-coords) (nth 0 line1-coords)))))
         (slope2-angle (atan(/ (float (- (nth 3 line2-coords) (nth 1 line2-coords)))
                               (- (nth 2 line2-coords) (nth 0 line2-coords)))))
         (start-angle (min slope1-angle slope2-angle))
         (stop-angle (max slope1-angle slope2-angle))
         (p1 (cons (+ (car intersection) (* arc-radius (cos start-angle)))
                   (+ (cdr intersection) (* arc-radius (sin start-angle)))))
         (p2 (cons (+ (car intersection) (* arc-radius (cos stop-angle)))
                   (+ (cdr intersection) (* arc-radius (sin stop-angle)))))
         (p3 (cons (- (car intersection) (* arc-radius (cos start-angle)))
                   (- (cdr intersection) (* arc-radius (sin start-angle)))))
         (p4 (cons (- (car intersection) (* arc-radius (cos stop-angle)))
                   (- (cdr intersection) (* arc-radius (sin stop-angle))))))
    (pcase part
      (1 (cons p1 p2))
      (2 (cons p2 p3))
      (3 (cons p3 p4))
      (4 (cons p4 p1))
      (_ (user-error "Part should be a number between 1 and 4")))))

(defun sketch-read-label (prompt)
  (completing-read prompt
                   (sketch-labels-list)))

(defun sketch-add-angle-arc (orientation &optional label-1 label-2)
  (interactive (list 1))
  (let* ((label-1 (or label-1 (sketch-read-label "From line: ")))
         (label-2 (or label-2 (sketch-read-label "To line: ")))
         (line-1 (car (dom-by-id sketch-svg (format "^%s$" label-1))))
         (line-2 (car (dom-by-id sketch-svg (format "^%s$" label-2))))
         (label (sketch-create-label "arc")))

    (defun sketch-draw-arc (orientation label-1 label-2)
      (let ((end-points (sketch-calculate-angle-arc (mapcar (lambda (coord)
                                                              (alist-get coord (cadr line-1)))
                                                            '(x1 y1 x2 y2))
                                                    (mapcar (lambda (coord)
                                                              (alist-get coord (cadr line-2)))
                                                            '(x1 y1 x2 y2))
                                                    20
                                                    orientation)))
        (svg-path (nth sketch-active-layer sketch-layers-list) `((moveto (,(car end-points)))
                                                                 (elliptical-arc
                                                                  ((20 20 ,(cadr end-points) ,(cddr end-points) :sweep t))))
                  :id label :part orientation :lines (cons label-1 label-2) :fill "transparent" :stroke "black")
        (print (sketch-redraw))))
    ;; (sketch-modify-angle label)))

    (defun sketch-rotate-angle ()
      (let* ((angle (print (car (dom-by-id sketch-root (format "^%s$" label)))))
             (part (print (dom-attr angle 'part))))
        (svg-remove (nth sketch-active-layer sketch-layers-list) (format "%s" label))
        (sketch-draw-arc (if (= part 4) 1 (1+ part)) label-1 label-2)))

    (sketch-draw-arc 1 label-1 label-2)
      (while (yes-or-no-p "Different part?")
        (sketch-rotate-angle))))

(defun sketch-snippet-get-dom (svg-file)
  (interactive "fCreate dom from file: ")
  (with-temp-buffer "svg"
                    (insert-file-contents-literally svg-file)
                    (xml-remove-comments (point-min) (point-max))
                    (libxml-parse-xml-region (point-min) (point-max))))

(defun sketch-snippets-add-ids (dom)
  (let ((idx 0))
    (dolist (n (dom-by-tag dom 'g))
      (dom-set-attribute n 'id (number-to-string idx))
      (setq idx (1+ idx)))))

(defun sketch-snippets-add-labels (dom)
  (interactive "f")
  (sketch-snippets-add-ids dom)
  (mapc (lambda (n)
          (let* ((s (dom-attr n 'transform))
                 (coords (when s
                           (split-string
                            (string-trim
                             s
                             "translate(" ")")
                            ","))))
            (svg-text dom
                      (dom-attr n 'id)
                      :x (car coords)
                      :y (cadr coords)
                      :font-size 10
                      :stroke "red"
                      :fill "red")))
        (cdr (dom-by-tag dom 'g)))
  ;; (save-current-buffer
  ;; (when lisp-buffer
  ;;   (sketch-update-lisp-window lisp lisp-buffer))
  ;; (let ((lisp-window (or (get-buffer-window "*sketch-root*")
  ;;                        (get-buffer-window lisp-buffer))))
  ;;   (unless (string= (buffer-name (window-buffer lisp-window)) "*sketch*")
  ;;     (if-let (buf (get-buffer"*sketch-root*"))
  ;;         (sketch-update-lisp-window sketch-root buf)
  ;;       (sketch-update-lisp-window lisp lisp-buffer))))
  ;; (setq sketch-root (append (seq-subseq sketch-root 0 2) (list (nth (car show-layers) svg-layers))))
  ;; (dolist (layer (cdr show-layers))
  ;;   (setq sketch-root (append sketch-root (list (nth layer svg-layers)))))
  ;; (setq sketch-svg (append svg-canvas
  ;;                          (when sketch-show-grid (list sketch-grid))
  ;;                          (when sketch-show-labels (list (sketch-labels)))
  ;;                          (list sketch-root)))
  (let ((inhibit-read-only t))
    (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
    (insert-image (svg-image dom))))

(defun sketch-insert-snippet (coords)
  ;; (interactive "@e")
  (let (
        ;; (coords (posn-object-x-y (event-start event)))
        (label (sketch-create-label 'snippet)))
    (dom-set-attribute sketch-snippet
                       'transform
                       (format "translate(%s,%s)" (car coords) (cdr coords)))
    (dom-set-attribute sketch-snippet
                       'id
                       label)
    (dom-append-child (nth sketch-active-layer sketch-layers-list) sketch-snippet)
    (sketch-redraw)))
    ;; (sketch-modify-object label)))

(defun sketch-import (svg-file)
  (interactive (list (let ((default-directory (concat
                                               (file-name-directory (locate-library "sketch-mode"))
                                               "snippet-files/")))
                       (read-file-name "Import (object) from file: "))))
  (let* ((dom (sketch-snippet-get-dom svg-file))
         (has-groups (dom-by-tag dom 'g)))
    (when has-groups (sketch-snippets-add-labels dom))
    (let* ((idx (when has-groups (read-number "Number of object for import: ")))
           (snippet (if (dom-by-tag dom 'g)
                        (dom-elements dom 'id (number-to-string idx))
                      (list (dom-append-child
                             (sketch-group (sketch-create-label 'group))
                             (car (dom-children dom)))))))
      (setq sketch-snippet (car snippet))
      (sketch-redraw)
      ;; (while (not (eq (car event) 'down-mouse-1))
        (let ((event (read-event "Click mouse-1 to insert")))
          (sketch-insert-snippet (posn-object-x-y (event-start event)))))))

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
(defvar sketch-toolbar-mode-map
  (let ((map (make-sparse-keymap))
        (bindings `(([slider down-mouse-1] . sketch-set-slider)
                   ("a" . sketch-set-action)
                   ("cs" . sketch-set-colors)
                   ("cf" . sketch-set-fill-color)
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
                   ("X" . sketch-show-xml)
                   ("S" . image-save)
                   ("?" . sketch-help)
                   ("Q" . sketch-quit))))
    (dolist (b bindings)
      (define-key map (car b) (cdr b)))
    map))

  ;; (with-no-warnings
  ;;   (if (boundp 'undo-tree-mode)
  ;;       (undo-tree-mode))
  ;;   (buffer-enable-undo))
  ;; (setq-local global-hl-line-mode nil)
  ;; (blink-cursor-mode 0))

(define-derived-mode sketch-toolbar-mode special-mode "Skecth-Toolbar"
  "Major mode for sketch toolbar")

(defun sketch-set-slider (event)
  (interactive "@e")
  (let* ((start (event-start event))
         (start-coords (posn-object-x-y start))
         (h (default-font-height))
         (slide-pixel-width (print (- 220 (* 2 h)))))
    (setq sketch-opacity (/ (float (- (car start-coords) h)) slide-pixel-width))
    (track-mouse
      (let ((event (read-event)))
        (while (not (memq (car event) '(mouse-1 drag-mouse-1)))
            (setq event (read-event))
            (let* ((end (event-start event))
                   (end-x-coord (car (posn-object-x-y end))))
              (when (< h end-x-coord (- 220 h))
                (setq sketch-opacity (/ (float (- end-x-coord h)) slide-pixel-width))))
            (sketch-slider-refresh))
        (let* ((end (event-end event))
               (end-x-coord (car (posn-object-x-y end))))
          (setq sketch-opacity (/ (print (pcase (- end-x-coord h)
                                           ((pred (>= h)) 0)
                                           ((pred (<= slide-pixel-width)) slide-pixel-width)
                                           (var (float var))))
                                  slide-pixel-width))
          (sketch-slider-refresh))))))

(defun sketch-slider-refresh ()
  (pcase-let ((`(,w ,h ,s ,e) (dom-attr sketch-slider :image)))
    (when (and s
	             (buffer-live-p (marker-buffer s)))
      (with-current-buffer (print (marker-buffer s))
          (let ((inhibit-read-only t))
            (replace-region-contents s e (lambda () (concat "OPACITY: "
                                                            (format (if sketch-opacity
                                                                        "%.2f"
                                                                      "%s"
                                                                      )
                                                                    sketch-opacity)
                                                            "\n ")))
	          (put-text-property (1- e) e 'display (svg-image 
                                                (let* ((w 220)
                                                       (h (default-font-height))
                                                       (half-h (/ h 2))
                                                       (level (if-let (x sketch-opacity) x 0))
                                                       (slider-pos (+ h (* (- w (* 2 h)) level))))
                                                  (setq sketch-slider (svg-create w h :stroke "black"))
                                                  (svg-circle sketch-slider half-h half-h (- half-h 4) :stroke "black" :fill "black")
                                                  (svg-rectangle sketch-slider 0 0 w h :stroke "black ":fill "white" :fill-opacity sketch-opacity)
                                                  (svg-line sketch-slider h (/ h 2) (- w h) (/ h 2))
                                                  (when sketch-opacity
                                                    (svg-line sketch-slider slider-pos (* 0.2 h) slider-pos (* 0.8 h) :stroke-width 3))
                                                  (dom-set-attribute sketch-slider :image (list w h s e))
                                                  sketch-slider)
                                                :map `(((rect . ((0 . 0) . (,w . ,h)))
                                                        slider
                                                        ,(append '(pointer
                                                                   hand)))))))))))
;; (defun sketch-set-slider (event)
;;   (interactive "@e")
;;   (let* ((start (event-start event))
;;          (start-coords (posn-object-x-y start)))
;;     (setq sketch-opacity (/ (float (- (car start-coords) 10)) 200)))
;;   (sketch-toolbar-refresh))
  ;; (track-mouse
  ;;      (let ((event (read-event)))
  ;;        (while (not (memq (car event) '(mouse-1 drag-mouse-1)))
  ;;          (let* ((end (event-start event))
  ;;                 (end-coords (posn-object-x-y end)))
  ;;            (setq sketch-opacity (/ (float (- (car end-coords) 10)) 200))
  ;;            (sketch-toolbar-refresh)))
  ;;        (let* ((end (event-end event))
  ;;               (end-coords (posn-object-x-y end)))
  ;;          (setq sketch-opacity (/ (float (- (car end-coords) 10)) 200))
  ;;          (sketch-toolbar-refresh)))))

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
      (goto-char (point-min))
      (sketch-toolbar-mode))))


(defun sketch-toggle-toolbar (&optional show)
  "Toggle toolbar, when SHOW non-nil then show toolbar."
  (interactive)
  (let ((win (get-buffer-window "*sketch-toolbar*")))
    (if win
        (unless show (delete-window win))
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
  (apply #'insert-text-button "   "
         'help-echo
         "Select from additional colors"
         'action
         (lambda (button) (interactive)
           (let ((list-colors-sort 'hsv))
             (list-colors-display (mapcar #'car shr-color-html-colors-alist)
                                  nil
                                  (lambda (c)
                                    (setq sketch-stroke-color c)
                                    (kill-buffer)
                                    (sketch-toolbar-refresh)))))
         (pcase sketch-fill-color
           ("none" nil)
           (_ (list 'face (when sketch-fill-color
                            (list :background (alist-get sketch-stroke-color
                                                         shr-color-html-colors-alist
                                                         nil nil 'string=)))))))
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
         'help-echo
         "Select from additional colors"
         'action
         (lambda (button) (interactive)
           (let ((list-colors-sort 'hsv))
             (list-colors-display (mapcar #'car shr-color-html-colors-alist)
                                  nil
                                  (lambda (c)
                                    (setq sketch-fill-color c)
                                    (kill-buffer)
                                    (sketch-toolbar-refresh)))))
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
  (insert (propertize "More colors? Press (C-u) c" 'face 'bold))

  (insert "\n\n")
  (let* ((start-marker (point-marker))
         (w 220)
         (h (default-font-height))
         (half-h (/ h 2))
         (level (if-let (x sketch-opacity) x 0))
         (slider-pos (+ h (* (- w (* 2 h)) level))))
    (setq sketch-slider (svg-create w h :stroke "black"))
    (insert (concat "OPACITY: "
                    (format (if sketch-opacity
                                "%.2f"
                              "%s"
                              )
                            sketch-opacity)
                    "\n "))
    (svg-circle sketch-slider half-h half-h (- half-h 4) :stroke "black" :fill "black")
    (svg-rectangle sketch-slider 0 0 w h :stroke "black ":fill "white" :fill-opacity sketch-opacity)
    (svg-line sketch-slider h (/ h 2) (- w h) (/ h 2))
    (when sketch-opacity
      (svg-line sketch-slider slider-pos (* 0.2 h) slider-pos (* 0.8 h) :stroke-width 3))
    (sketch-insert-image sketch-slider nil
                         :map `(((rect . ((0 . 0) . (,w . ,h)))
                                 slider
                                 ,(append '(pointer
                                            hand)))))
    (dom-set-attribute sketch-slider :image (list w h start-marker (point-marker)))
    (insert "\n")
    (apply #'insert-text-button "none"
           'help-echo
           "Deactivate opacity"
           'action
           (lambda (button) (interactive)
             (setq sketch-opacity nil)
             (sketch-slider-refresh))
           (unless sketch-opacity (list 'face 'link-visited)))))



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
