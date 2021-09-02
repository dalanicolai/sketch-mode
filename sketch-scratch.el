;; (setq svg-scratch (svg-create 100 100))
;; (svg-rectangle svg-scratch 25 25 50 50 :id "a")
;; (svg-line svg-scratch 25 25 75 75 :id "b" :stroke-color "black")

;; ;; (svg-remove svg-scratch "a")

;; (insert-image (svg-image (append svg-scratch (nthcdr 2 svg-labels))))

(defun sketch-modify-line-entry (node)
  (let* ((props (copy-alist (cadr node)))
         (id (alist-get 'id props)))
    (assq-delete-all 'id props)
    (vconcat [("id" 4 t)]
             (map 'vector (lambda (prop)
                            ;; (let* ((key (car prop))
                            ;;        (val (cdr prop))
                            ;;        (length (when (stringp val)
                            ;;                  (length val))))
                            (list (symbol-name (car prop))
                                  (pcase (car prop)
                                    ((or 'x1 'y1 'x2 'y2) 5)
                                    ('marker-end 7)
                                    ('fill 18)
                                    ('stroke 18)
                                    (_ 10))
                                  t))
                  props))))

(define-derived-mode sketch-modify-mode tabulated-list-mode "sketch-modify"
  (setq tabulated-list-format (sketch-modify-line-entry (car (dom-by-id svg "^a$"))))
  (let* ((props (copy-alist (cadar (dom-by-id svg "^a$"))))
         (id (alist-get 'id props)))
    (assq-delete-all 'id props)
    (setq tabulated-list-entries(list
                                 (list
                                  nil
                                  (vconcat (vector id)
                                           (map 'vector (lambda (prop) (let ((val (cdr prop)))
                                                                         (if (stringp val)
                                                                             val
                                                                           (number-to-string val))))
                                                props)))))
    (tabulated-list-print)))

