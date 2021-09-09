;; sketch-mode.el --- Quickly create svg sketches using keyboard and mouse -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.


;; Author: D.L. Nicolai <dalanicolai@gmail.com>
;; Created: 17 Jul 2021
;; Version: 0

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

;; DONE add remove (objects) functionality (see `svg-remove')

;; DONE move font transient (also its suffix) into main sketch transient (suffix)

;; DONE add functionality to crop/select part of image (on/before save)

;; DONE(-partially) add functionality to modify objects (see `add-object-modify-feature' branch)

;; TODO add functionality to customize markers

;; TODO Add options to hide transient suffixes (e.g. commands are trivial and could be hidden to get more drawing space.
;; unfortunately transient levels (de)activate instead of hide/show suffixes)

;; TODO enable defining global svg settings (object properties)

;; TODO maybe transform relevant transient argument (strings) to variables

;; TODO add function to open svg code in 'other buffer' and quickly reload
;; (after editing, DONE see `add-object-modify-feature' branch)

;; TODO add functionality to start drawing from org-mode source block and update
;; source block after each draw/edit

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


;;; Code:
(require 'svg)
(require 'transient)

(defgroup sketch nil
  "Configure default sketch (object) properties."
  :group 'Applications)

(defcustom sketch-im-x-offset 7
  "Horizontal offset in pixels of image position within frame.
Set this value to correct for cursor 'bias'."
  :type 'integer)

(defcustom sketch-im-y-offset 1
  "Vertical offset in pixels of image position within frame.
Set this value to correct for cursor 'bias'."
  :type 'integer)

(defcustom sketch-default-image-size '(800 . 600)
  "Default size for sketch canvas.
Cons cell with car and cdr both integers, respectively
representing the image width and image height
default: (800 . 600)."
  :type '(cons integer integer))

(defcustom sketch-show-grid t
  "When non-nil, show grid lines (default: t)."
  :type 'boolean)

(defcustom sketch-show-labels nil
  "When non-nil, show object labels (default: t)."
  :type 'boolean)

(defcustom sketch-default-grid-parameter 25
  "Default grid line separation distance (integer)."
  :type 'integer)

(defcustom sketch-label-size 15
  "Size of object labels."
  :type 'integer)

(defcustom sketch-default-shape 'line
  "Default object type for `sketch-interactively.'"
  :type '(choice
	        (const :tag "Line" 'line)
	        (const :tag "Rectangle" 'rectangle)
          (const :tag "Circle" 'circle)
          (const :tag "Ellipse" 'ellipse)))

(defcustom sketch--snap-to-grid t
  "Default value of snap to grid.
If non-nil then snap to grid."
  :type 'boolean)

(defcustom sketch-include-start-marker nil
  "Start marker type"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Dot" 'dot)))

(defcustom sketch-include-mid-marker nil
  "Mid marker type"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Dot" 'dot)))

(defcustom sketch-include-end-marker nil
  "End marker type"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Dot" 'dot)))


;;; SVG-definitions

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


;;; Resume sketch-code

(defun sketch-group (id &rest args)
  (apply #'svg-group
         :id id
         :transform "translate(0,0)"
         args))

(define-minor-mode sketch-mode
  "Create svg images using the mouse.
In sketch-mode buffer press \\[sketch-transient] to activate the
transient."
  :lighter "sketch-mode"
  :keymap
  `(
    ;; ([sketch drag-mouse-1] . sketch-interactively)
    ;; ([C-S-drag-mouse-1] . sketch-interactively)
    (,(kbd "C-c C-s") . sketch-transient))
  (undo-tree-mode))

(defun sketch-mapcons (fn &rest cons-cells)
  "Apply FN to list of car's and cdr's of CONS-CELLS.
Return a single cons cell."
  (cons (apply fn (mapcar #'car cons-cells))
        (apply fn (mapcar #'cdr cons-cells))))

(defun sketch-norm (vec)
  "Return norm of a vector.
VEC should be a cons or a list containing only number elements."
  (let ((sum-of-squares (if (consp vec)
                            (+ (expt (car vec) 2)
                               (expt (cdr vec) 2))
                          (apply #'+
                                 (mapcar (lambda (x) (* x x))
                                         vec)))))
    (expt sum-of-squares (/ 1.0 (if (consp vec)
                                    2
                                  (length vec))))))

(defun sketch--circle-radius (start-coords end-coords)
  (sketch-norm
   (sketch-mapcons #'- end-coords start-coords)))

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

(defvar sketch-svg)
(defvar-local svg-canvas nil)
(defvar-local sketch-grid nil)
(defvar-local sketch-root nil)
(defvar-local sketch-layers-list nil)
(defvar-local show-layers '(0))

(defun sketch--create-canvas (width height &optional grid-parameter)
  "Create canvas for drawing svg using the mouse."
  (let ((width width)
        (height height))
    (setq svg-canvas (svg-create width height :stroke "gray"))
    (svg-marker svg-canvas "arrow" 8 8 "black" t)
    (svg-rectangle svg-canvas 0 0 width height :fill "white")
    (setq sketch-grid (sketch-group "grid"))
    (let ((dash t))
      (dotimes (x (1- (/ width grid-parameter)))
        (let ((pos (* (1+ x) grid-parameter)))
          (svg-line sketch-grid pos 0 pos height :stroke-dasharray (when dash "2,4"))
          (setq dash (if dash nil t)))))
    (let ((dash t))
      (dotimes (x (1- (/ height grid-parameter)))
        (let ((pos (* (1+ x) grid-parameter)))
          (svg-line sketch-grid 0 pos width pos :stroke-dasharray (when dash "2,4"))
          (setq dash (if dash nil t)))))
    (setq sketch-svg (append svg-canvas (when sketch-show-grid (list sketch-grid))))
    (setq sketch-root (sketch-group "root"))
    (sketch-add-layer)
    (insert-image (sketch-image sketch-svg
                                :grid-param grid-parameter
                                :pointer 'arrow
                                :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                                        ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                                        sketch
                                        (pointer arrow help-echo (lambda (_ _ pos)
                                                                   (let ((message-log-max nil)
                                                                         (coords (cdr (mouse-pixel-position))))
                                                                     (print (format "(%s, %s)"
                                                                                    (- (car coords) sketch-im-x-offset)
                                                                                    (+ (cdr coords) sketch-im-y-offset)))))))))
                  (prin1-to-string sketch-root))))

;; FIXME: `defvar' can't be meaningfully inside a function like that.
;; FIXME: Use a `sketch-' prefix for all dynbound vars.
(defvar-local sketch-elements nil)
(defvar-local grid-param 25)
(defvar-local active-layer 0)

;;;###autoload
(defun sketch (arg)
  "Initialize or switch to (new) SVG image.
With prefix argument, create sketch using default (customizable)
values"
  (interactive "P")
  (let ((buffer (get-buffer "*sketch*")))
    (if buffer
        (progn (switch-to-buffer buffer)
               (call-interactively 'sketch-transient))
      (let ((width (if arg (car sketch-default-image-size) (read-number "Enter width: ") ))
            (height (if arg 600 (read-number "Enter height: "))))
        (switch-to-buffer (get-buffer-create "*sketch*"))
        (setq grid-param (if arg 25 (read-number "Enter grid parameter (enter 0 for no grid): ")))
        (sketch--create-canvas width height grid-param))
      (sketch-mode)
      (call-interactively 'sketch-transient))))


(defun sketch--snap-to-grid (coord grid-parameter)
  (cons (* (round (/ (float (car coord)) grid-parameter)) grid-parameter)
        (* (round (/ (float (cdr coord)) grid-parameter)) grid-parameter)))


;;; Transient

(defclass sketch-variable:choices (transient-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((obj sketch-variable:choices))
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-infix-value ((obj sketch-variable:choices))
  "Return the value of OBJ's `value' slot if not nil,
     else return value of OBJ's `default' slot if not nil,
     else return nil"
  (let ((default (oref obj default)))
    (if-let ((value (oref obj value)))
        (concat (oref obj argument) value)
      (when default
        (concat (oref obj argument) default)))))

(cl-defmethod transient-format-value ((obj sketch-variable:choices))
  (let ((value (oref obj value))
        (choices (oref obj choices))
        (default  (oref obj default)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                choices
                (propertize "|" 'face 'transient-inactive-value))
     (and (or default)
          (concat
           (propertize "|" 'face 'transient-inactive-value)
           (cond
            (default
              (propertize (concat "default:" default)
                          'face
                          (if value
                              'transient-inactive-value
                            'transient-value))))))
     (propertize "]" 'face 'transient-inactive-value))))

(defclass sketch-variable:colors (transient-variable)
  ((fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((_obj sketch-variable:colors))
  (read-color "Select color: "))

(cl-defmethod transient-infix-value ((obj sketch-variable:colors))
  (let ((default (oref obj default)))
    (if-let ((value (oref obj value)))
        (concat (oref obj argument) (substring-no-properties value))
      (when default
        (concat (oref obj argument) (substring-no-properties default))))))

;; We always call the autoloaded `color-name-to-rgb' before calling this
;; function, so we know it's available even tho the compiler doesn't.
(declare-function color-rgb-to-hex "color"
                                     (red green blue &optional digits-per-component))

(cl-defmethod transient-format-value ((obj sketch-variable:colors))
  (let ((value (oref obj value))
        (default  (oref obj default)))
    (if value
        (format "%s (%s)"
                (propertize value 'face (cons 'foreground-color value))
                (propertize (apply #'color-rgb-to-hex (color-name-to-rgb value))
                            'face 'transient-inactive-argument))
      (if (string= default "none")
          (propertize "none" 'face 'transient-inactive-argument)
        (format "%s (%s)"
                (propertize default 'face (cons 'foreground-color default))
                (propertize (apply #'color-rgb-to-hex (color-name-to-rgb default))
                            'face 'transient-inactive-argument))))))

  ;; (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
  ;;        (print event))))
    ;;      (start (event-start event))
    ;;      (grid-param (plist-get (cdr (posn-image start)) :grid-param))
    ;;      (snap (transient-arg-value "--snap-to-grid=" args))
    ;;      (start-coords (if (or (not snap) (string= snap "nil"))
    ;;                       (posn-object-x-y start)
    ;;                     (sketch--snap-to-grid (posn-object-x-y start) grid-param)))
    ;;      (end (event-end event))
    ;;      (end-coords (if (or (not snap) (string= snap "nil"))
    ;;                     (posn-object-x-y end)
    ;;                   (sketch--snap-to-grid (posn-object-x-y end) grid-param)))
    ;;      (object-props (list :stroke-width
    ;;                          (transient-arg-value "--stroke-width=" args)
    ;;                          :stroke
    ;;                          (transient-arg-value "--stroke-color=" args)
    ;;                          :fill
    ;;                          (transient-arg-value "--fill-color=" args)
    ;;                          :marker-end (if args (pcase (transient-arg-value "--marker=" args)
    ;;                                                 ("arrow" "url(#arrow)")
    ;;                                                 ("point" "url(#point)")
    ;;                                                 (_ "none"))
    ;;                                        (if sketch-include-end-marker
    ;;                                            "url(#arrow)"
    ;;                                          "none"))))
    ;;      (command-and-coords (pcase (transient-arg-value "--object=" args)
    ;;                            ("line" (list 'svg-line
    ;;                                          (car start-coords) (cdr start-coords) (car end-coords) (cdr end-coords)))
    ;;                            ("rectangle" `(svg-rectangle ,@(sketch--rectangle-coords start-coords end-coords)))
    ;;                            ("circle" (list 'svg-circle
    ;;                                            (car start-coords) (cdr start-coords)
    ;;                                            (sketch--circle-radius start-coords end-coords)))
    ;;                            ("ellipse" `(svg-ellipse ,@(sketch--ellipse-coords start-coords end-coords))))))
    ;; (apply (car command-and-coords) sketch-root `(,@(cdr command-and-coords) ,@object-props :id ,(sketch-create-label)))
    ;; (sketch-redraw)))

(transient-define-prefix sketch-transient ()
  "Some Emacs magic"
  :transient-suffix     'transient--do-call
  :transient-non-suffix 'transient--do-stay
  [["General definitions"
    ("c" "stroke-color" sketch-stroke-color)
    ("C" "fill-color" sketch-fill-color)
    ("w" "stroke-width" sketch-stroke-width)]
   ["Object definitions"
    ("o" "object" sketch-object)
    ("m" "end-marker" sketch-object-marker)]
   ["Font definitions"
    ("-f" "family" sketch-select-font)
    ("-w" "font-weight" sketch-font-weight)
    ("-s" "font-size" sketch-font-size)]]
   [["Grid"
    ("s" "Snap to grid" sketch-snap)
    ("g" "Toggle grid" sketch-toggle-grid)]
   ["Labels"
    ("l" sketch-cycle-labels)]
   ["Layers"
    ("L" sketch-layer)
    ("-L" sketch-layers)
    ("A" "Add layer" sketch-add-layer)]]
  ["Commands"
   [([sketch drag-mouse-1] "Draw object"  sketch-interactively-1)
    ([sketch mouse-1] "Draw text"  sketch-text-interactively)
    ([sketch C-S-drag-mouse-1] "Crop image" sketch-crop)]
   [("t" "Transform object" sketch-modify-object)
    ("r" "Remove object" sketch-remove-object)
    ("i" "Import object" sketch-import)]
    [("u" "Undo" sketch-undo)
    ("U" "Redo" sketch-redo)]
   [("d" "Show definition" sketch-show-definition)
    ("D" "Copy definition" sketch-copy-definition)
    ("S" "Save image" sketch-save)]
   [("q" "Quit transient"           transient-quit-one)]])

(transient-define-infix sketch-object ()
  :description "Option with list"
  :class 'sketch-variable:choices
  :argument "--object="
  :choices '("rectangle" "circle" "ellipse")
  :default "line")

(transient-define-infix sketch-stroke-width ()
  :description "Option with list"
  :class 'transient-option
  :argument "--stroke-width="
  :choices (mapcar (lambda (x)
                     (number-to-string x))
                   (number-sequence 1 100)))

(transient-define-infix sketch-stroke-color ()
  :description "Option with list"
  :class 'sketch-variable:colors
  :argument "--stroke-color="
  :default "black")

(transient-define-infix sketch-fill-color ()
  :description "Option with list"
  :class 'sketch-variable:colors
  :argument "--fill-color="
  :default "none")

(transient-define-infix sketch-object-marker ()
  :description "Option with list"
  :class 'sketch-variable:choices
  :argument "--marker="
  :choices '("arrow" "dot")
  :default "none")

(transient-define-infix sketch-snap ()
  :description "Option with list"
  :class 'sketch-variable:choices
  :argument "--snap-to-grid="
  :choices '("t")
  :default "nil")

(defun sketch-toggle-grid ()
  (interactive)
  (with-current-buffer "*sketch*"
    (setq sketch-show-grid (if sketch-show-grid nil t))
    (sketch-redraw)))

(cl-defmethod transient-infix-set ((obj sketch-variable:choices) value)
  ;; (let ((variable (oref obj variable)))
  (oset obj value value)
  (setq sketch-show-labels value)
  (magit-refresh)
  (sketch-redraw)
  (unless (or value transient--prefix)
    (message "Unset %s" variable)))

(transient-define-infix sketch-cycle-labels ()
  :description "Show labels"
  :class 'sketch-variable:choices
  ;; :variable "sketch-show-labels"
  :variable 'sketch-show-labels
  :argument "--labels="
  :choices '("layer" "all")
  :default "nil")

(defun sketch-labels ()
  (interactive)
  (let ((nodes (pcase sketch-show-labels
                 ("layer" (dom-children (nth active-layer sketch-layers-list)))
                 ("all" (apply #'append (mapcar (lambda (l)
                                                  (dom-children (nth l sketch-layers-list)))
                                                show-layers)))))
        (svg-labels (sketch-group "labels")))
    (mapc (lambda (node)
            (pcase (car node)
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

;; (defun sketch-create-label (type)
;;   (interactive)
;;   (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
;;          (labels-list (mapcar #'string (concat alphabet (upcase alphabet))))
;;          (labels (sketch-labels-list)))
;;     (while (member (car labels-list) labels)
;;       (setq labels-list (cdr labels-list)))
;;     (car labels-list)))

(defun sketch-create-label (type)
  (interactive)
  (let* ((prefix (concat (when (/= active-layer 0)
                           (number-to-string active-layer))
                         (pcase type
                           ("line" "l")
                           ("rectangle" "r")
                           ("circle" "c")
                           ("ellipse" "e")
                           ("group" "g"))))
         (idx 0)
         (label (concat prefix (number-to-string idx)))
         (labels (sketch-labels-list)))
    (while (member label labels)
      (setq idx (1+ idx))
      (setq label (concat prefix (number-to-string idx))))
    label))

(transient-define-infix sketch-layer ()
  "Layer that is currently active when sketching."
  :description "Active layer"
  :class 'transient-lisp-variable
  :variable 'active-layer)

(defun sketch-list-layers ()
  (mapcar #'number-to-string (number-sequence 0 (length sketch-layers-list))))
  ;; (with-current-buffer (get-buffer "*sketch*")
  ;;   (mapcar (lambda (layer) (alist-get 'id (cadr layer))) sketch-layers-list)))

(defun sketch-translate-node-coords (node amount &rest args)
  (dolist (coord args node)
    (cl-decf (alist-get coord (cadr node)) amount)))

(defun sketch--svg-translate (dx dy)
  (interactive)
  (mapcar (lambda (node)
            (pcase (car node)
              ('line (sketch-translate-node-coords node dx 'x1 'x2)
                     (sketch-translate-node-coords node dy 'y1 'y2))
              ('rect (sketch-translate-node-coords node dx 'x)
                     (sketch-translate-node-coords node dy 'y))
              ((or 'circle 'ellipse)
               (sketch-translate-node-coords node dx 'cx)
               (sketch-translate-node-coords node dy 'cy))
              ('text (sketch-translate-node-coords node dx 'x)
                     (sketch-translate-node-coords node dy 'y))))
          (cddr (nth active-layer sketch-layers-list))))
  ;; (let ((node (car (dom-by-id svg-sketch label))))
  ;;   (pcase (car node)
  ;;     ('g (setf (alist-get 'transform (cadr node))
  ;;               (format "translate(%s %s)" (- dx) (- dy))))
  ;;     ;; ('line (sketch-translate-node-coords node dx 'x1 'x2)
  ;;     ;;        (sketch-translate-node-coords node dy 'y1 'y2))
  ;;     ;; ('rect (sketch-translate-node-coords node dx 'x)
  ;;     ;;        (sketch-translate-node-coords node dy 'y))
  ;;     ;; ((or 'circle 'ellipse)
  ;;     ;;  (sketch-translate-node-coords node dx 'cx)
  ;;     ;;  (sketch-translate-node-coords node dy 'cy))
  ;;     ;; ('text (sketch-translate-node-coords node dx 'x)
  ;;     ;;        (sketch-translate-node-coords node dy 'y)))

  ;;     ) ;; TODO make it work for all types of elements
  ;;   node))

(defun sketch-redraw (&optional lisp lisp-buffer)
  (unless sketch-mode
    (user-error "Not in sketch-mode buffer"))
  (save-current-buffer
    (when lisp-buffer
      (sketch-update-lisp-window lisp lisp-buffer))
    ;; (let ((lisp-window (or (get-buffer-window "*sketch-root*")
    ;;                        (get-buffer-window lisp-buffer))))
    ;;   (unless (string= (buffer-name (window-buffer lisp-window)) "*sketch*")
    ;;     (if-let (buf (get-buffer"*sketch-root*"))
    ;;         (sketch-update-lisp-window sketch-root buf)
    ;;       (sketch-update-lisp-window lisp lisp-buffer))))
    (setq sketch-root (append (subseq sketch-root 0 2) (list (nth (car show-layers) sketch-layers-list))))
    (dolist (layer (cdr show-layers))
      (setq sketch-root (append sketch-root (list (nth layer sketch-layers-list)))))
    (setq sketch-svg (append svg-canvas
                             (when sketch-show-grid (list sketch-grid))
                             (when sketch-show-labels (list (sketch-labels)))
                             (list sketch-root)))
    (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
    (insert-image (sketch-image sketch-svg
                                :pointer 'arrow
                                :grid-param grid-param
                                :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                                        ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                                        sketch
                                        (pointer arrow help-echo (lambda (_ _ pos)
                                                                   (let ((message-log-max nil)
                                                                         (coords (mouse-pixel-position)))
                                                                     (print (format "(%s, %s)"
                                                                                    (- (cadr coords) pos)
                                                                                    (cddr coords)))))))))
                  (prin1-to-string sketch-root))))


(transient-define-suffix sketch-interactively-1 (event)
  (interactive "@e")
  (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" args))
         (start-coords (if (or (not snap) (string= snap "nil"))
                          (posn-object-x-y start)
                        (sketch--snap-to-grid (posn-object-x-y start) grid-param)))
         (end (event-end event))
         (end-coords (if (or (not snap) (string= snap "nil"))
                        (posn-object-x-y end)
                      (sketch--snap-to-grid (posn-object-x-y end) grid-param)))
         (object-props (list :stroke-width
                             (transient-arg-value "--stroke-width=" args)
                             :stroke
                             (transient-arg-value "--stroke-color=" args)
                             :fill
                             (transient-arg-value "--fill-color=" args)
                             :marker-end (if args (pcase (transient-arg-value "--marker=" args)
                                                    ("arrow" "url(#arrow)")
                                                    ("dot" "url(#dot)")
                                                    (_ "none"))
                                           (if sketch-include-end-marker
                                               "url(#arrow)"
                                             "none"))))
         (object-type (transient-arg-value "--object=" args))
         (command-and-coords (pcase object-type
                               ("line" (list 'svg-line
                                             (car start-coords) (cdr start-coords)
                                             (car end-coords) (cdr end-coords)))
                               ("rectangle" `(svg-rectangle
                                              ,@(sketch--rectangle-coords start-coords end-coords)))
                               ("circle" (list 'svg-circle
                                               (car start-coords) (cdr start-coords)
                                               (sketch--circle-radius start-coords end-coords)))
                               ("ellipse" `(svg-ellipse ,@(sketch--ellipse-coords start-coords end-coords))))))
    (apply (car command-and-coords) (nth active-layer sketch-layers-list) `(,@(cdr command-and-coords) ,@object-props :id ,(sketch-create-label object-type)))
    (when-let (buf (get-buffer "*sketch-root*"))
      (sketch-update-lisp-window sketch-root buf))
    (sketch-redraw)))

(transient-define-suffix sketch-remove-object ()
  (interactive)
  (svg-remove sketch-root (completing-read "Remove element with id: "
                                          (sketch-labels-list)))
  (sketch-redraw))

(transient-define-suffix sketch-insert-snippet (event)
  (interactive "@e")
  (let ((coords (posn-object-x-y (event-start event)))
        (node (oref transient-current-prefix :value))
        (label (sketch-create-label "group")))
    (dom-set-attribute node
                       'transform
                       (format "translate(%s,%s)" (car coords) (cdr coords)))
    (dom-set-attribute node
                       'id
                       label)
    (dom-append-child (nth active-layer sketch-layers-list) node)
    (sketch-redraw)
    (sketch-modify-object label)))

;; (transient-define-suffix print-hello ()
;;   (interactive)

(transient-define-prefix sketch-import (svg-file)
  [("p" "print args" print-hello)
   ([sketch mouse-1] "Insert"  sketch-insert-snippet)]
  (interactive "fImport object from file: ")
  (let ((dom (sketch-snippet-get-dom svg-file)))
    (sketch-snippets-add-labels dom)
    (let* ((idx (read-number "Number of object for import: "))
           (snippet (dom-elements dom 'id (number-to-string idx))))
      (sketch-redraw)
      (transient-setup 'sketch-import nil nil :value (car snippet)))))

;; (transient-define-suffix sketch-import-object (svg-file)
;;   (interactive "fImport object from file: ")

(define-minor-mode sketch-lisp-mode
  "Minor mode for svg lisp buffers."
  :lighter "sketch"
  :keymap
  `((,(kbd "C-c C-s") . sketch-transient)
    (,(kbd "C-c C-c") . sketch-load-definition)))

(transient-define-suffix sketch-show-definition ()
  ;; :transient 'transient--do-exit
  (interactive)
  (if-let (win (get-buffer-window "*sketch-root*"))
      (delete-window win)
    (let ((buffer (get-buffer-create "*sketch-root*"))
          (sketch sketch-root))
      (set-window-dedicated-p
       (get-buffer-window
        (pop-to-buffer buffer
                       '(display-buffer-in-side-window . ((side . right) (window-width . 70)))))
       t)
    (erase-buffer)
    (with-current-buffer buffer
      (dom-pp sketch)))
    (emacs-lisp-mode)
    (sketch-lisp-mode)))

(transient-define-suffix sketch-copy-definition ()
  (interactive)
  (with-temp-buffer
    (dom-pp sketch-svg)
    (kill-new (buffer-string)))
  (message "SVG definition added to kill-ring"))

(defun sketch-load-definition ()
  (interactive)
  (let ((def (read (buffer-string))))
    (with-current-buffer "*sketch*"
      (setq sketch-root def)
      (sketch-redraw))))

;; (defvar sketch-undo-redo nil)

(transient-define-suffix sketch-undo ()
  (interactive)
  (undo-tree-undo)
  (setq sketch-root (read (buffer-string)))
  (setq sketch-layers-list (dom-elements sketch-root 'id "layer")))
  ;; (let ((sketch-reverse (nreverse sketch-root)))
  ;;   (push (pop sketch-reverse) sketch-undo-redo)
  ;;   (setq sketch-root (nreverse sketch-reverse)))
  ;; (sketch-redraw))

(transient-define-suffix sketch-redo ()
  (interactive)
  (undo-tree-redo)
  (setq sketch-root (read (buffer-string)))
  (setq sketch-layers-list (dom-elements sketch-root 'id "layer")))
  ;; (let ((sketch-reverse (nreverse sketch-root)))
  ;;   (push (pop sketch-undo-redo) sketch-reverse)
  ;;   (setq sketch-root (nreverse sketch-reverse)))
  ;; (sketch-redraw))

(transient-define-suffix sketch-text-interactively (event)
  (interactive "@e")
  (let* ((sketch-args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" sketch-args))
         (coords (if (or (not snap) (string= snap "nil"))
                           (posn-object-x-y start)
                         (sketch--snap-to-grid (posn-object-x-y start) grid-param)))
         (text (read-string "Enter text: "))
         (object-props (list :font-size
                             (transient-arg-value "--font-size=" sketch-args)
                             :font-weight
                             (transient-arg-value "--font-weight=" sketch-args)
                             )))
                             ;; :fill
                             ;; (transient-arg-value "--fill-color=" sketch-args)
                             ;; :marker-end (if sketch-args (pcase (transient-arg-value "--marker=" sketch-args)
                             ;;                        ("arrow" "url(#arrow)")
                             ;;                        ("dot" "url(#dot)")
                             ;;                        (_ "none"))
                             ;;               (if sketch-include-end-marker
                             ;;                   "url(#arrow)"
                             ;;                 "none"))))
    (apply #'svg-text sketch-root text :x (car coords) :y (cdr coords) object-props))
    (sketch-redraw))

(transient-define-infix sketch-select-font ()
  :description "Option with list"
  :class 'transient-option
  :argument "--family="
  :choices (font-family-list))

(transient-define-infix sketch-font-size ()
  :description "Option with list"
  :class 'transient-option
  :argument "--font-size="
  :choices (mapcar (lambda (x)
                     (number-to-string x))
                   (number-sequence 1 100)))

(transient-define-infix sketch-font-weight ()
  :description "Option with list"
  :class 'sketch-variable:choices
  :argument "--font-weight="
  :choices '("bold")
  :default "normal")

;; (defclass sketch-variable:layers (transient-variable)
;;   ((fallback    :initarg :fallback    :initform nil)
;;    (default     :initarg :default     :initform nil)))

;; (cl-defmethod transient-infix-read ((obj sketch-variable:layers))
;;   (let ((value (if-let (val (oref obj value))
;;                    val
;;                  (oref obj default)))
;;         (layer (read-number "Type number of layer for toggle: ")))
;;     (if (memq layer value)
;;         (delq layer value)
;;       (push layer value))))

;; (cl-defmethod transient-infix-value ((obj sketch-variable:layers))
;;   (let ((default (oref obj default)))
;;     (if-let ((value (oref obj value)))
;;         value)
;;     (when default
;;       default)))

;; (cl-defmethod transient-format-value ((obj sketch-variable:layers))
;;   (let ((value (oref obj value))
;;         (default  (oref obj default)))
;;     (format "%s" (if value
;;                      (oref obj value)
;;                    (oref obj default)))))
  ;; (let ((value (oref obj value))
  ;;       (default  (oref obj default)))
  ;;   (if value
  ;;       (format "%s (%s)"
  ;;               (propertize value 'face (cons 'foreground-color value))
  ;;               (propertize (apply 'color-rgb-to-hex (color-name-to-rgb value))
  ;;                           'face 'transient-inactive-argument))
  ;;     (if (string= default "none")
  ;;         (propertize "none" 'face 'transient-inactive-argument)
  ;;       (format "%s (%s)"
  ;;               (propertize default 'face (cons 'foreground-color default))
  ;;               (propertize (apply 'color-rgb-to-hex (color-name-to-rgb default))
  ;;                           'face 'transient-inactive-argument))))))

(transient-define-suffix sketch-add-layer ()
  (interactive)
  (setq sketch-layers-list (append sketch-layers-list
                           (list (sketch-group (format "layer-%s" (length sketch-layers-list))))))
  (message "Existing layers (indices): %s" (mapconcat #'number-to-string
                                            (number-sequence 0 (1- (length sketch-layers-list)))
                                            ", ")))

(transient-define-infix sketch-layers ()
  "List with layers that should be added to the image.
Should be a list of numbers containing the number of the layers
that should be added to the image. Initial value: (0)"
  :description "Show layers"
  :class 'transient-lisp-variable
  :variable 'show-layers)
  ;; :argument "--layers="
  ;; :default '(0))
  ;; :default (number-sequence (length sketch-layers-list)))

(transient-define-suffix sketch-crop (event)
  (interactive "@e")
  (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" args))
         (start-coords (if (or (not snap) (string= snap "nil"))
                           (posn-object-x-y start)
                         (sketch--snap-to-grid (posn-object-x-y start) grid-param)))
         (end (event-end event))
         (end-coords (if (or (not snap) (string= snap "nil"))
                         (posn-object-x-y end)
                       (sketch--snap-to-grid (posn-object-x-y end) grid-param)))
         (new-width (abs (- (car end-coords) (car start-coords))))
         (new-height (abs (- (cdr end-coords) (cdr start-coords)))))
    (setq svg-canvas (svg-create new-width new-height :stroke "gray"))
    (svg-marker svg-canvas "arrow" 8 8 "black" t)
    (svg-rectangle svg-canvas 0 0 new-width new-height :fill "white")
    (setq sketch-root (svg-translate "root" (car start-coords) (cdr start-coords)))
    (sketch-redraw)))

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

(transient-define-suffix sketch-save ()
  (interactive)
  (image-save))


;;; Modify object

(defun sketch-translate-object (buffer object-def props coords amount)
  (dolist (coord coords)
    (cl-incf (alist-get coord props) amount))
  (sketch-redraw object-def buffer))

(defun sketch-parse-transform-value (value)
  (let ((transforms (mapcar (lambda (val)
                              (split-string val "[(,)]" t))
                            (split-string value))))
    (mapcar (lambda (x)
              (cons (intern (car x)) (mapcar (lambda (val)
                                      (string-to-number val))
                                    (cdr x))))
            transforms)))

(defun sketch-format-transfrom-value (value)
  (string-join (mapcar (lambda (x) (concat (symbol-name (car x))
                                           "("
                                           (number-to-string (cadr x))
                                           (if-let (y (caddr x))
                                               (concat "," (number-to-string y)))
                                           ")"))
                       value)
               " "))

(defun sketch-group-translate (buffer object-def direction &optional fast)
  (let ((transform (sketch-parse-transform-value
                (dom-attr object-def
                          'transform)))
        (dist (if fast
                  10
                1)))
    (pcase direction
      ('up (cl-decf (cadr (alist-get 'translate transform)) dist))
      ('down (cl-incf (cadr (alist-get 'translate transform)) dist)))
    (dom-set-attribute object-def
                       'transform
                       (sketch-format-transfrom-value transform))
    object-def
    (sketch-redraw object-def buffer)))

;; TODO 'refactor' subsequent suffixes (e.g. create general function/macro)
(transient-define-suffix sketch-translate-down (args)
  (interactive (list (oref transient-current-prefix :value)))
  (let* ((object (transient-arg-value "--object=" args))
         (buffer (transient-arg-value "--buffer=" args))
         (object-def (dom-by-id sketch-svg (format "^%s$" object)))
         (props (cadar object-def)))
    (if (eq (caar object-def) 'g)
        (sketch-group-translate buffer (car object-def) 'down)
    (sketch-translate-object buffer
                             object-def
                             props
                             '(y1 y2) 1))))

(transient-define-suffix sketch-translate-fast-down (args)
  (interactive (list (oref transient-current-prefix :value)))
  (let* ((object (transient-arg-value "--object=" args))
         (buffer (transient-arg-value "--buffer=" args))
         (object-def (dom-by-id sketch-svg (format "^%s$" object)))
         (props (cadar object-def)))
    (if (eq (caar object-def) 'g)
        (sketch-group-translate buffer (car object-def) 'down t)
    (sketch-translate-object buffer
                             object-def
                             props
                             (pcase (caar object-def)
                               ('line '(y1 y2)))
                             10))))

(transient-define-suffix sketch-translate-up (args)
  (interactive (list (oref transient-current-prefix :value)))
  (let* ((object (transient-arg-value "--object=" args))
         (buffer (transient-arg-value "--buffer=" args))
         (object-def (dom-by-id sketch-svg (format "^%s$" object)))
         (props (cadar object-def)))
    (if (eq (caar object-def) 'g)
        (sketch-group-translate buffer (car object-def) 'up)
    (sketch-translate-object buffer
                             object-def
                             props
                             (pcase (caar object-def)
                               ('line '(y1 y2)))
                             -1))))

(transient-define-suffix sketch-translate-fast-up (args)
  (interactive (list (oref transient-current-prefix :value)))
  (let* ((object (transient-arg-value "--object=" args))
         (buffer (transient-arg-value "--buffer=" args))
         (object-def (dom-by-id sketch-svg (format "^%s$" object)))
         (props (cadar object-def)))
    (if (eq (caar object-def) 'g)
        (sketch-group-translate buffer (car object-def) 'up t)
    (sketch-translate-object buffer
                             object-def
                             props
                             (pcase (caar object-def)
                               ('line '(y1 y2)))
                             -10))))

(transient-define-prefix sketch-modify-object (&optional group)
  "Set object properties."
  :transient-suffix 'transient--do-call
  ["Properties"
   [("o" "object" sketch-modify-object 'transient--do-exit)]]
  [[("<down>" "down" sketch-translate-down)
   ("<up>" "up" sketch-translate-up)]
   [("S-<down>" "fast down" sketch-translate-fast-down)
    ("S-<up>" "fast up" sketch-translate-fast-up)]]
  [("l" sketch-cycle-labels)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (let* ((object (if group
                     group
                   (completing-read "Transform element with id: "
                                    (sketch-labels-list))))
         (buffer (get-buffer-create (format "*sketch-object-%s*" object))))
    (display-buffer buffer '(display-buffer-in-side-window . ((side . right) (window-width . 70))))
    (pp (cadar (dom-by-id sketch-svg (format "^%s$" object))) buffer)
    (with-current-buffer buffer
      (emacs-lisp-mode))
    (transient-setup 'sketch-modify-object
                      nil
                      nil
                      :value (list (format "--object=%s" object)
                                   (format "--buffer=%s" buffer)))))

(defun sketch-update-lisp-window (lisp buffer)
  ;; (let ((sketch sketch-root))
    (with-current-buffer buffer
      (erase-buffer)
      (pp lisp (current-buffer))
      (end-of-buffer)))

;;; import/snippets

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
  (unless sketch-mode
    (user-error "Not in sketch-mode buffer"))
  ;; (save-current-buffer
  ;; (when lisp-buffer
  ;;   (sketch-update-lisp-window lisp lisp-buffer))
  ;; (let ((lisp-window (or (get-buffer-window "*sketch-root*")
  ;;                        (get-buffer-window lisp-buffer))))
  ;;   (unless (string= (buffer-name (window-buffer lisp-window)) "*sketch*")
  ;;     (if-let (buf (get-buffer"*sketch-root*"))
  ;;         (sketch-update-lisp-window sketch-root buf)
  ;;       (sketch-update-lisp-window lisp lisp-buffer))))
  ;; (setq sketch-root (append (subseq sketch-root 0 2) (list (nth (car show-layers) svg-layers))))
  ;; (dolist (layer (cdr show-layers))
  ;;   (setq sketch-root (append sketch-root (list (nth layer svg-layers)))))
  ;; (setq sketch-svg (append svg-canvas
  ;;                          (when sketch-show-grid (list sketch-grid))
  ;;                          (when sketch-show-labels (list (sketch-labels)))
  ;;                          (list sketch-root)))
  (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
  (insert-image (svg-image dom)))

(provide 'sketch-mode)
;;; sketch-mode.el ends here
