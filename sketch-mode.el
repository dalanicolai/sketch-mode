;;; sketch-mode.el --- Quickly create svg sketches using keyboard and mouse -*- lexical-binding: t; -*-

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

;; DONE add functionality to crop/select part of image (on save)

;; TODO add functionality to modify objects (see `add-object-modify-feature' branch)

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

;; NOTE this is a most straightforward sketch-mode. A more advanced/general version
;; could implement a drawing DSL based on nodes (a la tikz/asymptote etc.)


;;; Code:
(require 'svg)
(require 'transient)

(defgroup sketch nil
  "Configure default sketch (object) properties.")

(defcustom sketch-im-x-offset 7
  "Default grid line separation distance (integer)."
  :type 'integer)

(defcustom sketch-im-y-offset 1
  "Default grid line separation distance (integer)."
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

(define-minor-mode sketch-mode
  "Create svg images using the mouse.
In sketch-mode buffer press \\[sketch-transient] to activate the
transient."
  :lighter "sketch-mode"
  :keymap
  `(([sketch drag-mouse-1] . sketch-interactively)
    ;; ([C-S-drag-mouse-1] . sketch-interactively)
    (,(kbd "C-c C-s") . sketch-transient)))

(defun sketch--circle-radius (start-coords end-coords)
  (sqrt (+ (expt (- (car end-coords) (car start-coords)) 2)
           (expt (- (cdr end-coords) (cdr start-coords)) 2))))
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

(defun sketch--create-canvas (width height &optional grid-param)
  "Create canvas for drawing svg using the mouse."
  (defvar sketch-svg)
  (defvar svg-canvas)
  (defvar svg-grid)
  (defvar sketch-root)
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
       (setq sketch-svg (append svg-canvas (when sketch-show-grid (cddr svg-grid))))
       (svg-image sketch-svg
                  :grid-param grid-param
                  :pointer 'arrow
                  :map `(((rect . ((0 . 0) . (,(dom-attr sketch-svg 'width) . ,(dom-attr sketch-svg 'height))))
                          ;; :map '(((rect . ((0 . 0) . (800 . 600)))
                          sketch
                          (pointer arrow help-echo (lambda (_ _ pos)
                                                     (let ((message-log-max nil)
                                                           (coords (cdr (mouse-pixel-position))))
                                                       (print (format "(%s, %s)"
                                                                      (- (car coords) sketch-im-x-offset)
                                                                      (+ (cdr coords) sketch-im-y-offset)))))))))))
    (sketch-mode)
    (call-interactively 'sketch-transient)
    (setq sketch-root (svg-create width height)))

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
        ;; FIXME: `defvar' can't be meaningfully inside a function like that.
        ;; FIXME: Use a `sketch-' prefix for all dynbound vars.
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
    ("-s" "font-size" sketch-font-size)]
   ["Grid"
    ("s" "Snap to grid" sketch-snap)
    ("g" "Toggle grid" sketch-toggle-grid)]
   ["Labels"
    ("l" "Toggle labels" sketch-toggle-labels)]]
  ["Commands"
   [([sketch drag-mouse-1] "Draw object"  sketch-interactively-1)
    ([sketch mouse-1] "Draw text"  sketch-text-interactively)
    ([sketch C-S-drag-mouse-1] "Crop image" sketch-crop)]
   [("T" "Transfrom object" sketch-modify-object)
    ("R" "Remove object" sketch-remove-object)]
    [("u" "Undo" sketch-undo)
    ("r" "Redo" sketch-redo)]
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
        (cddr sketch-root))
    (cddr svg-labels)))

(defun sketch-labels-list ()
  (mapcar (lambda (node)
            (dom-attr node 'id))
          (cddr sketch-root)))

(defun sketch-create-label ()
  (interactive)
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (labels-list (mapcar #'string (concat alphabet (upcase alphabet))))
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
          (cddr sketch-root)))

(defun sketch-redraw (&optional lisp lisp-buffer)
  (unless sketch-mode
    (user-error "Not in sketch-mode buffer"))
  (let ((lisp-window (or (get-buffer-window "*sketch-root*")
                         (get-buffer-window lisp-buffer))))
    (unless (string= (buffer-name (window-buffer lisp-window)) "*sketch*")
      (if-let (buf (get-buffer"*sketch-root*"))
          (sketch-update-lisp-window sketch-root buf)
        (sketch-update-lisp-window lisp lisp-buffer))))
  (setq sketch-svg (append svg-canvas
                    (when sketch-show-grid (cddr svg-grid))
                    (cddr sketch-root)
                    (when sketch-show-labels (sketch-labels))))
  (erase-buffer) ;; a (not exact) alternative is to use (kill-backward-chars 1)
  (insert-image (svg-image sketch-svg
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
                                                                               (cddr coords)))))))))))

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
    (apply (car command-and-coords) sketch-root `(,@(cdr command-and-coords) ,@object-props :id ,(sketch-create-label)))
    (sketch-redraw)))

(transient-define-suffix sketch-remove-object ()
  (interactive)
  (svg-remove sketch-root (completing-read "Remove element with id: "
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
;;       ('line   (svg-line sketch-svg (car start-coords) (cdr start-coords) (car end-coords) (cdr end-coords)
;;                          :marker-start (if sketch-include-start-marker
;;                                          "url(#arrow)"
;;                                        "none")
;;                          :marker-mid (if sketch-include-mid-marker
;;                                            "url(#arrow)"
;;                                          "none")
;;                          :marker-end (if sketch-include-end-marker
;;                                          "url(#arrow)"
;;                                        "none")))
;;       ('rectangle (apply 'svg-rectangle sketch-svg (append (sketch--rectangle-coords start-coords end-coords) '(:fill "none"))))
;;       ('circle (svg-circle sketch-svg (car start-coords) (cdr start-coords) (sketch--circle-radius start-coords end-coords)
;;                            :fill "none"))
;;       ('ellipse (apply 'svg-ellipse sketch-svg  (append (sketch--ellipse-coords start-coords end-coords) '(:fill "none")))))
;;     (kill-backward-chars 1)
;;     (insert-image (svg-image sketch-svg :pointer 'arrow :grid-param grid-param))))
  ;; (call-interactively 'tutorial-transient)

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
        (pop-to-buffer buffer '(display-buffer-in-side-window . ((side . right) (window-width . 70)))))
       t)
    (erase-buffer)
    (pp sketch buffer))
    (emacs-lisp-mode)
    (sketch-lisp-mode)))

(transient-define-suffix sketch-copy-definition ()
  (interactive)
  (with-temp-buffer
    (pp sketch-svg (current-buffer))
    (kill-new (buffer-string)))
  (message "SVG definition added to kill-ring"))

(defun sketch-load-definition ()
  (interactive)
  (setq sketch-root (read (buffer-string)))
  (with-current-buffer "*sketch*"
    (sketch-redraw)))

(defvar sketch-undo-redo nil)

(transient-define-suffix sketch-undo ()
  (interactive)
  (let ((sketch-reverse (nreverse sketch-root)))
    (push (pop sketch-reverse) sketch-undo-redo)
    (setq sketch-root (nreverse sketch-reverse)))
  (sketch-redraw))

(transient-define-suffix sketch-redo ()
  (interactive)
  (let ((sketch-reverse (nreverse sketch-root)))
    (push (pop sketch-undo-redo) sketch-reverse)
    (setq sketch-root (nreverse sketch-reverse)))
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
    (setf (cddr sketch-root) (sketch--svg-translate (car start-coords) (cdr start-coords)))
    (sketch-redraw)))

(transient-define-suffix sketch-save ()
  (interactive)
  (image-save))


;;; Modify object

(transient-define-suffix sketch-translate-down (args)
  (interactive (list (transient-args 'sketch-modify-object)))
  (let* ((object (transient-arg-value "--object=" args))
         (object-def (dom-by-id sketch-svg (format "^a$" object)))
         (props (cadar object-def)))
    (dolist (coord '(y1 y2))
      (cl-incf (alist-get coord props) 10))
    (sketch-redraw object-def)))

(transient-define-prefix sketch-modify-object ()
  "Set object properties."
  :transient-suffix     'transient--do-call
  ["Properties"
   [("o" "object" "--object=")]]
  [("<down>" "Down" sketch-translate-down)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (let* ((object (completing-read "Transform element with id: "
                                 (sketch-labels-list)))
         (buffer (get-buffer-create (format "*sketch-object-%s*" object))))
    (display-buffer buffer '(display-buffer-in-side-window . ((side . right) (window-width . 70))))
    (pp (cadar (dom-by-id sketch-svg (format "^%s$" object))) buffer)
    (transient-setup 'sketch-modify-object nil nil :value (list (format "--object=%s" object)))))

(defun sketch-update-lisp-window (lisp buffer)
  ;; (let ((sketch sketch-root))
  (with-current-buffer buffer
    (erase-buffer)
    (pp lisp (current-buffer))))


 (provide 'sketch-mode)
;;; sketch-mode.el ends here
