;;; sketch-mode.el --- Support for the Foo programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2021 Daniel Nicolai


;; Author: D.L. Nicolai <dalanicolai@gmail.com>
;; Created: 17 Jul 2021

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

;; TODO add functionality to modify objects

;; TODO enable defining global svg settings (object properties)

;; TODO maybe transform relevant transient argument (strings) to variables

;; TODO add function to open svg code in 'other buffer' and quickly reload
;; (after editing)

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
  "Configure default sketch (object) properties.")

(defcustom sketch-default-image-size '(800 . 600)
  "Default size for sketch canvas.
Cons cell with car and cdr both integers, respectively
representing the image width and image height
(default: '(800 . 600))."
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

(defcustom sketch-default-shape 'line
  "Default object type for `sketch-interactively.'"
  :type '(choice
	        (const :tag "Line" 'line)
	        (const :tag "Rectangle" 'rectangle)
          (const :tag "Circle" 'circle)
          (const :tag "Ellipse" 'ellipse)))

(defcustom sketch-snap-to-grid t
  "When non-nil snap to grid."
  :type 'boolean)

(defcustom sketch-include-start-marker nil
  "Start start-marker"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Point" 'point)))

(defcustom sketch-include-mid-marker nil
  "Mid marker type"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Point" 'point)))

(defcustom sketch-include-end-marker nil
  "End marker type"
  :type '(choice
	        (const :tag "No marker" nil)
	        (const :tag "Arrow" 'arrow)
          (const :tag "Point" 'point)))

(defun svg-marker (svg id width height &optional color reverse)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (svg--def
   svg
   (apply
    'dom-node
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

(define-minor-mode sketch-mode
  "Create svg images using the mouse.
In sketch-mode buffer press \\[sketch-transient] to activate the
transient."
  :lighter "sketch-mode"
  :keymap
  '(([drag-mouse-1] . sketch-interactively)
    ;; ([C-S-drag-mouse-1] . sketch-interactively)
    ("" . sketch-transient)))

(defun sketch--circle-radius (start-coords end-coords)
  (sqrt (+ (expt (- (car end-coords) (car start-coords)) 2)
           (expt (- (cdr end-coords) (cdr start-coords)) 2))))

(defun sketch--rectangle-coords (start-coords end-coords)
  (let ((base-coords (cons (apply 'min (list (car start-coords) (car end-coords)))
                           (apply 'min (list (cdr start-coords) (cdr end-coords))))))
  (list (car base-coords)
        (cdr base-coords)
        (abs (- (car end-coords) (car start-coords)))
        (abs (- (cdr end-coords) (cdr start-coords))))))

(defun sketch--ellipse-coords (start-coords end-coords)
  (list (/ (+ (car start-coords) (car end-coords)) 2)
        (/ (+ (cdr start-coords) (cdr end-coords)) 2)
        (abs (/ (- (car end-coords) (car start-coords)) 2))
        (abs (/ (- (cdr end-coords) (cdr start-coords)) 2))))

(defun sketch--create-canvas (width height &optional grid-param)
  "Create canvas for drawing svg using the mouse."
  (defvar svg)
  (defvar svg-canvas)
  (defvar svg-grid)
  (defvar svg-sketch)
    (insert-image
     (let ((width width)
           (height height))
       (setq svg-canvas (svg-create width height :stroke "gray"))
       (svg-marker svg-canvas "arrow" 8 8 "black" t)
       (svg-rectangle svg-canvas 0 0 width height :fill "white")
       (setq svg-grid (svg-create width height))
       (let ((dash t))
         (dotimes (x (1- (/ width grid-param)))
           (let ((pos (* (1+ x) grid-param)))
             (svg-line svg-grid pos 0 pos height :stroke-dasharray (when dash "2,4"))
             (setq dash (if dash nil t)))))
       (let ((dash t))
         (dotimes (x (1- (/ height grid-param)))
           (let ((pos (* (1+ x) grid-param)))
             (svg-line svg-grid 0 pos width pos :stroke-dasharray (when dash "2,4"))
             (setq dash (if dash nil t)))))
       (setq svg (append svg-canvas (when sketch-show-grid (cddr svg-grid))))
       (svg-image svg :pointer 'arrow :grid-param grid-param)))
    (sketch-mode)
    (call-interactively 'sketch-transient)
    (setq svg-sketch (svg-create width height)))

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
        (defvar-local sketch-elements nil)
        (defvar-local grid-param 25)
        (setq grid-param (if arg 25 (read-number "Enter grid parameter (enter 0 for no grid): ")))
        (sketch--create-canvas width height grid-param)))))


(defun sketch-snap-to-grid (coord grid-param)
  (cons (* (round (/ (float (car coord)) grid-param)) grid-param)
        (* (round (/ (float (cdr coord)) grid-param)) grid-param)))


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

(cl-defmethod transient-infix-read ((obj sketch-variable:colors))
  (read-color "Select color: "))

(cl-defmethod transient-infix-value ((obj sketch-variable:colors))
  (let ((default (oref obj default)))
    (if-let ((value (oref obj value)))
        (concat (oref obj argument) (substring-no-properties value))
      (when default
        (concat (oref obj argument) (substring-no-properties default))))))

(cl-defmethod transient-format-value ((obj sketch-variable:colors))
  (let ((value (oref obj value))
        (default  (oref obj default)))
    (if value
        (format "%s (%s)"
                (propertize value 'face (cons 'foreground-color value))
                (propertize (apply 'color-rgb-to-hex (color-name-to-rgb value))
                            'face 'transient-inactive-argument))
      (if (string= default "none")
          (propertize "none" 'face 'transient-inactive-argument)
        (format "%s (%s)"
                (propertize default 'face (cons 'foreground-color default))
                (propertize (apply 'color-rgb-to-hex (color-name-to-rgb default))
                            'face 'transient-inactive-argument))))))

  ;; (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
  ;;        (print event))))
    ;;      (start (event-start event))
    ;;      (grid-param (plist-get (cdr (posn-image start)) :grid-param))
    ;;      (snap (transient-arg-value "--snap-to-grid=" args))
    ;;      (start-coords (if (or (not snap) (string= snap "nil"))
    ;;                       (posn-object-x-y start)
    ;;                     (sketch-snap-to-grid (posn-object-x-y start) grid-param)))
    ;;      (end (event-end event))
    ;;      (end-coords (if (or (not snap) (string= snap "nil"))
    ;;                     (posn-object-x-y end)
    ;;                   (sketch-snap-to-grid (posn-object-x-y end) grid-param)))
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
    ;; (apply (car command-and-coords) svg-sketch `(,@(cdr command-and-coords) ,@object-props :id ,(sketch-create-label)))
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
    ("-s" "font-size" sketch-font-size)]
   ["Grid"
    ("s" "Snap to grid" sketch-snap)
    ("g" "Toggle grid" sketch-toggle-grid)]
   ["Labels"
    ("l" "Toggle labels" sketch-toggle-labels)]]
  ["Commands"
   [([drag-mouse-1] "Draw object"  sketch-interactively-1)
    ([mouse-1] "Draw text"  sketch-text-interactively)
    ([C-S-drag-mouse-1] "Crop image" sketch-crop)]
   [("R" "Remove object" sketch-remove-object)
    ("u" "Undo" sketch-undo)
    ("r" "Redo" sketch-redo)]
   [("d" "Show definition" sketch-show-definition)
    ("D" "Copy definition" sketch-copy-definition)
    ("S" "Save image" sketch-save)]]
  [("q" "Quit"           transient-quit-one)])

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
  :choices '("arrow" "point")
  :default "none")

(transient-define-infix sketch-snap ()
  :description "Option with list"
  :class 'sketch-variable:choices
  :argument "--snap-to-grid="
  :choices '("t")
  :default "nil")

(defun sketch-toggle-grid ()
  (interactive)
  (setq sketch-show-grid (if sketch-show-grid nil t))
  (sketch-redraw))

(defun sketch-labels ()
  (interactive)
  (let ((svg-labels (svg-create 100 100)))
    (mapc (lambda (node)
            (pcase (car node)
              ('rect (svg-text svg-labels
                               (dom-attr node 'id)
                               :x (+ (dom-attr node 'x) 2)
                               :y (+ (dom-attr node 'y)
                                     (- (dom-attr node 'height) 2))
                               :font-size 20
                               :stroke "red"
                               :fill "red"))
              ('line (svg-text svg-labels
                               (dom-attr node 'id)
                               :x (dom-attr node 'x1)
                               :y (dom-attr node 'y1)
                               :font-size 20
                               :stroke "red"
                               :fill "red"))))
        (cddr svg-sketch))
    (cddr svg-labels)))

(defun sketch-labels-list ()
  (mapcar (lambda (node)
            (dom-attr node 'id))
          (cddr svg-sketch)))

(defun sketch-create-label ()
  (interactive)
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (labels-list (mapcar 'string (concat alphabet (upcase alphabet))))
         (labels (sketch-labels-list)))
    (while (member (car labels-list) labels)
      (setq labels-list (cdr labels-list)))
    (car labels-list)))

(defun sketch-toggle-labels ()
  (interactive)
  (setq sketch-show-labels (if sketch-show-labels nil t))
  (sketch-redraw))

(defun sketch-translate-node-coords (node amount &rest args)
  (dolist (coord args node)
    (cl-decf (alist-get coord (cadr node)) amount)))

(defun svg-translate (dx dy)
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
          (cddr svg-sketch)))

(defun sketch-redraw ()
  (unless sketch-mode
    (user-error "Not in sketch-mode buffer"))
  (setq svg (append svg-canvas
                    (when sketch-show-grid (cddr svg-grid))
                    (cddr svg-sketch)
                    (when sketch-show-labels (sketch-labels))))
  (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
  (insert-image (svg-image svg :pointer 'arrow :grid-param grid-param)))

(transient-define-suffix sketch-interactively-1 (event)
  (interactive "@e")
  (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" args))
         (start-coords (if (or (not snap) (string= snap "nil"))
                          (posn-object-x-y start)
                        (sketch-snap-to-grid (posn-object-x-y start) grid-param)))
         (end (event-end event))
         (end-coords (if (or (not snap) (string= snap "nil"))
                        (posn-object-x-y end)
                      (sketch-snap-to-grid (posn-object-x-y end) grid-param)))
         (object-props (list :stroke-width
                             (transient-arg-value "--stroke-width=" args)
                             :stroke
                             (transient-arg-value "--stroke-color=" args)
                             :fill
                             (transient-arg-value "--fill-color=" args)
                             :marker-end (if args (pcase (transient-arg-value "--marker=" args)
                                                    ("arrow" "url(#arrow)")
                                                    ("point" "url(#point)")
                                                    (_ "none"))
                                           (if sketch-include-end-marker
                                               "url(#arrow)"
                                             "none"))))
         (command-and-coords (pcase (transient-arg-value "--object=" args)
                               ("line" (list 'svg-line
                                             (car start-coords) (cdr start-coords) (car end-coords) (cdr end-coords)))
                               ("rectangle" `(svg-rectangle ,@(sketch--rectangle-coords start-coords end-coords)))
                               ("circle" (list 'svg-circle
                                               (car start-coords) (cdr start-coords)
                                               (sketch--circle-radius start-coords end-coords)))
                               ("ellipse" `(svg-ellipse ,@(sketch--ellipse-coords start-coords end-coords))))))
    (apply (car command-and-coords) svg-sketch `(,@(cdr command-and-coords) ,@object-props :id ,(sketch-create-label)))
    (sketch-redraw)))

(transient-define-suffix sketch-remove-object ()
  (interactive)
  (svg-remove svg-sketch (completing-read "Remove element with id: "
                                          (sketch-labels-list)))
  (sketch-redraw))
;; (defun sketch-interactively (event)
;;   "Draw object interactively, interpreting mouse event."
;;   (interactive "e")
;;   (let* ((start (event-start event))
;;          (start-coords (posn-object-x-y start))
;;          (end (event-end event))
;;          (end-coords (posn-object-x-y end))
;;          (grid-param (plist-get (cdr (posn-image start)) :grid-param)))
;;     (when (or (not grid-param) (= grid-param 0))
;;       (setq sketch-snap-to-grid nil))
;;     (when sketch-snap-to-grid
;;       (setq start-coords (sketch-snap-to-grid start-coords grid-param))
;;       (setq end-coords (sketch-snap-to-grid end-coords grid-param)))
;;     (pcase sketch-default-shape
;;       ('line   (svg-line svg (car start-coords) (cdr start-coords) (car end-coords) (cdr end-coords)
;;                          :marker-start (if sketch-include-start-marker
;;                                          "url(#arrow)"
;;                                        "none")
;;                          :marker-mid (if sketch-include-mid-marker
;;                                            "url(#arrow)"
;;                                          "none")
;;                          :marker-end (if sketch-include-end-marker
;;                                          "url(#arrow)"
;;                                        "none")))
;;       ('rectangle (apply 'svg-rectangle svg (append (sketch--rectangle-coords start-coords end-coords) '(:fill "none"))))
;;       ('circle (svg-circle svg (car start-coords) (cdr start-coords) (sketch--circle-radius start-coords end-coords)
;;                            :fill "none"))
;;       ('ellipse (apply 'svg-ellipse svg  (append (sketch--ellipse-coords start-coords end-coords) '(:fill "none")))))
;;     (kill-backward-chars 1)
;;     (insert-image (svg-image svg :pointer 'arrow :grid-param grid-param))))
  ;; (call-interactively 'tutorial-transient)

(transient-define-suffix sketch-show-definition ()
  :transient 'transient--do-exit
  (interactive)
  (let ((buffer (get-buffer-create "svg"))
        (sketch svg-sketch))
    (transient-quit-one)
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (pp svg-sketch (current-buffer)))
    (emacs-lisp-mode))

(transient-define-suffix sketch-copy-definition ()
  (interactive)
  (with-temp-buffer
    (pp svg (current-buffer))
    (kill-new (buffer-string)))
  (message "SVG definition added to kill-ring"))

(defun sketch-load-definition ()
  (interactive)
  (setq svg-sketch (read (buffer-string))))

(transient-define-suffix sketch-undo ()
  (interactive)
  (defvar sketch-undo-redo nil)
  (let ((sketch-reverse (nreverse svg-sketch)))
    (push (pop sketch-reverse) sketch-undo-redo)
    (setq svg-sketch (nreverse sketch-reverse)))
  (sketch-redraw))

(transient-define-suffix sketch-redo ()
  (interactive)
  (let ((sketch-reverse (nreverse svg-sketch)))
    (push (pop sketch-undo-redo) sketch-reverse)
    (setq svg-sketch (nreverse sketch-reverse)))
  (sketch-redraw))

(transient-define-suffix sketch-text-interactively (event)
  (interactive "@e")
  (let* ((sketch-args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" sketch-args))
         (coords (if (or (not snap) (string= snap "nil"))
                           (posn-object-x-y start)
                         (sketch-snap-to-grid (posn-object-x-y start) grid-param)))
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
                             ;;                        ("point" "url(#point)")
                             ;;                        (_ "none"))
                             ;;               (if sketch-include-end-marker
                             ;;                   "url(#arrow)"
                             ;;                 "none"))))
    (apply 'svg-text svg-sketch text :x (car coords) :y (cdr coords) object-props))
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

(transient-define-suffix sketch-crop (event)
  (interactive "@e")
  (let* ((args (when transient-current-prefix (transient-args 'sketch-transient)))
         (start (event-start event))
         (grid-param (plist-get (cdr (posn-image start)) :grid-param))
         (snap (transient-arg-value "--snap-to-grid=" args))
         (start-coords (if (or (not snap) (string= snap "nil"))
                           (posn-object-x-y start)
                         (sketch-snap-to-grid (posn-object-x-y start) grid-param)))
         (end (event-end event))
         (end-coords (if (or (not snap) (string= snap "nil"))
                         (posn-object-x-y end)
                       (sketch-snap-to-grid (posn-object-x-y end) grid-param)))
         (new-width (abs (- (car end-coords) (car start-coords))))
         (new-height (abs (- (cdr end-coords) (cdr start-coords)))))
    (setq svg-canvas (svg-create new-width new-height :stroke "gray"))
    (svg-marker svg-canvas "arrow" 8 8 "black" t)
    (svg-rectangle svg-canvas 0 0 new-width new-height :fill "white")
    (setf (cddr svg-sketch) (svg-translate (car start-coords) (cdr start-coords)))
    (sketch-redraw)))

(transient-define-suffix sketch-save ()
  (interactive)
  (image-save))

(provide 'sketch-mode)
;;; filename ends here
