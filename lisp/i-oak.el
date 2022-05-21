;;; i-oak.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Oak is a lite replacement for imenu-list
;;
;;; Code:
(require 'button)
(require 'imenu)

(defconst oak-buffer-name "*oak*")
(defvar oak-buffer nil "Oak's buffer")
(defvar oak-prev-buffer nil "Oak's previous buffer")
(defvar oak-cursor-location 0 "Oak previous buffer's cursor point")

(defmacro oak-defface-level (depth fg)
  `(defface ,(intern (format "oak-level-%d" depth))
     '((t (:foreground ,fg)))
     ,(format "Oak level %d face" depth)))
(oak-defface-level 1 "#519F50")
(oak-defface-level 2 "#FCFEBC")
(oak-defface-level 3 "#98BC37")
(oak-defface-level 4 "#BAA67F")
(oak-defface-level 5 "#699C78")
(oak-defface-level 6 "#0AAEB3")
(oak-defface-level 7 "#FACF5A")
(oak-defface-level 8 "#455D7A")

(defface oak-hidden-block-face
  '((t (:inherit 'font-lock-comment-face))) "Oak hidden block face")

(defface oak-indicator-highlight-face
  '((t (:background "#8B3A3A" :underline t))) "Oak indicator highlight face")

(defmacro with-oak-buffer-window (&rest body)
  "Ensure to manipulating in oak-buffer window"
  `(let ((w (get-buffer-window oak-buffer)))
     (if (window-live-p w)
         (with-selected-window w ,@body))))

(defmacro with-oak-prev-buffer-window (&rest body)
  "Ensure to manipulating in oak-prev-buffer window"
  `(let ((w (get-buffer-window oak-prev-buffer)))
     (if (window-live-p w)
         (with-selected-window w ,@body))))

(defun oak--display (tree-list)
  (setq oak-buffer (get-buffer-create "*oak*"))
  (if (not (eq oak-buffer (current-buffer)))
      (setq oak-prev-buffer (current-buffer)))
  (display-buffer-in-side-window
   oak-buffer '((side . right) (window-width . 0.3)))
  (with-oak-buffer-window
    (read-only-mode 0)
    (erase-buffer)
    (oak--print-list-tree tree-list 0)
    (read-only-mode 1)))

(defun oak--make-indent (indent depth)
  (if (> depth 0)
      (mapconcat #'identity (make-list depth indent) "") ""))

(defun oak--insert-item (item depth &optional isnode)
  (let* ((indents (oak--make-indent " " depth))
         (level-face (list (intern (format "oak-level-%d" (1+ (% depth 8))))))
         (action (if isnode 'oak--toggle-fold 'oak--action)))
    (insert-button
     (concat indents (car item))
     'face (list (intern (format "oak-level-%d" (1+ (% depth 8)))))
     'help-echo (format "%S" item)
     'follow-link t
     'depth depth
     'data (cdr item)
     'action action)
    (insert "\n")))

(defun oak--print-list-tree (index-alist depth)
  (dolist (item index-alist)
    (if (not (listp (cdr item)))
        (oak--insert-item item depth))
    (when (sequencep (cdr item))
      (oak--insert-item (cons (car item) (cdr item)) depth t)
      (oak--print-list-tree (cdr item) (1+ depth)))))

(defun oak--action (event)
  (let ((m (button-get event 'data)))
    (when m
      (with-oak-prev-buffer-window
       (if (not (window-live-p (get-buffer-window oak-prev-buffer)))
           (switch-to-buffer-other-window oak-prev-buffer))
       (cond
        ((markerp m)
         (switch-to-buffer (marker-buffer m))
         (goto-char (marker-position m)))
        ((numberp m)
         (goto-char m)))
       (recenter)))))

(defun oak--hide-fold (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'oak-hidden-block-face)
    (overlay-put ov 'invisible t)
    (overlay-put ov 'intangible t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display " ..")
    (overlay-put ov 'category 'oak-hidden-block)))

(defun oak--show-fold (beg end)
  ;; (remove-overlays beg end 'category 'oak-hidden-block))
  (let ((ovs (overlays-in beg end))
        (ret nil))
    (mapcar (lambda (ov)
              (when (eq 'oak-hidden-block (overlay-get ov 'category))
                (delete-overlay ov)
                (setq ret t)))
            ovs)
    ret))

(defun oak--toggle-fold (event)
  ;; (message "%s" (button-get event 'data)
  (let* ((cur-btn event)
         (ori-btn-depth (button-get event 'depth))
         (beg (overlay-end cur-btn))
         (end beg))
    (setq cur-btn (next-button (overlay-end cur-btn)))
    (setq cur-btn-depth (button-get cur-btn 'depth))
    (catch 'e
      (while (< ori-btn-depth cur-btn-depth)
        ;; (message "%s ~ %d ~ %d" cur-btn cur-btn-depth (overlay-end cur-btn))
        (when (= (1- (point-max)) (overlay-end cur-btn))
          (setq end (point-max))
          (throw 'e nil))
        (setq cur-btn (next-button (overlay-end cur-btn)))
        (setq cur-btn-depth (button-get cur-btn 'depth))))
    (if (not (= end (point-max)))
        (setq end (1- (overlay-start cur-btn))))
    ;; (message "%d - %d" beg end)
    (if (< beg end)
        (if (not (oak--show-fold beg end))
            (oak--hide-fold beg end)))))

;;
;; oak imenu
;;

(defun marker-or-integer-p (a)
  (or (markerp a) (integerp a)))

(defvar oak-indicator nil "Store oak current indicator highlight overlay")

(defun oak-imenu-indicator-put (beg end)
  (setq oak-indicator (make-overlay beg end))
  (overlay-put oak-indicator 'face 'oak-indicator-highlight-face)
  (overlay-put oak-indicator 'category 'oak-indicator-highlight))

(defun oak-imenu-put-overlay-indicator (pos)
  (with-oak-buffer-window
   (catch 'e
     (dolist (ov oak-cache-order)
       (let ((data (overlay-get ov 'data)))
         (when (marker-or-integer-p data)
           ;; (message "%s - %d = %d" data pos (- data pos))
           ;; If data <= pos, means we meet the closest item
           (unless (> (- data pos) 0)
             ;; when oak-indicator initial
             (if (not (overlayp oak-indicator))
                 (oak-imenu-indicator-put (overlay-start ov) (overlay-end ov))
               ;; when oak-indicator doesn't equal to current ov
               (unless (eq (overlay-get ov 'category) 'oak-indicator-highlight)
                 (delete-overlay oak-indicator)
                 (oak-imenu-indicator-put (overlay-start ov) (overlay-end ov))))
             (throw 'e nil))))))))

(defvar oak-cache-order nil "Oak cache order store overlays in oak buffer,
those overlays sorted descend by it's prop DATA's mark or positions")

(defun oak-imenu-cache-order ()
  (with-oak-buffer-window
   (let ((ovs (flatten-tree (overlay-lists))))
     (sort
      ovs
      (lambda (ov1 ov2)
        (let ((data1 (overlay-get ov1 'data))
              (data2 (overlay-get ov2 'data)))
          (if (and (marker-or-integer-p data1)
                   (marker-or-integer-p data2))
              (> data1 data2)
            nil)))))))

;;;###autoload
(defun oak-imenu ()
  (interactive)
  (setq imenu-auto-rescan t)
  (setq oak-cursor-location (point))
  (condition-case err
      (progn
        (imenu--make-index-alist)
        (oak--display
         (buffer-local-value 'imenu--index-alist (current-buffer)))
        (setq oak-cache-order (oak-imenu-cache-order))
        (oak-imenu-put-overlay-indicator oak-cursor-location)
        (oak-imenu-run-timer))
    (imenu-unavailable
     (message "!!!%s" (error-message-string err)))))

(defun oak-imenu-update-indicator ()
  (if (eq (current-buffer) oak-prev-buffer)
    (when (not (= oak-cursor-location (point)))
      (setq oak-cursor-location (point))
      (oak-imenu-put-overlay-indicator oak-cursor-location))))

(defvar oak-imenu-timer nil "Oak imenu timer, used for automatic update the
indicator")

;;;###autoload
(defun oak-imenu-run-timer ()
  (interactive)
  (setq oak-imenu-timer (run-with-timer 5 5 'oak-imenu-update-indicator)))

;;;###autoload
(defun oak-imenu-cancel-timer ()
  (interactive)
  (cancel-timer oak-imenu-timer))

(defun oak-imenu-cancel-timer-buffer-killed ()
  (if (or (eq (current-buffer) oak-prev-buffer)
          (eq (current-buffer) oak-buffer))
      (oak-imenu-cancel-timer)))

(add-hook 'kill-buffer-hook 'oak-imenu-cancel-timer-buffer-killed)

(provide 'i-oak)

;;; i-oak.el ends here
