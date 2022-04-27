;;; i-lvf.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Live View Filter, it suppose to replacement the ioccur
;;
;;; Code:
(require 'imenu)

;;;
;;;
;;; lvf core
;;;
(defvar lvf-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-g") 'lvf-interrupt)
    (define-key map (kbd "C-n") 'lvf-next)
    (define-key map (kbd "C-p") 'lvf-prev)
    (define-key map (kbd "C-v") 'lvf-pgdn)
    (define-key map (kbd "M-v") 'lvf-pgup)
    (define-key map (kbd "M-<") 'lvf-top)
    (define-key map (kbd "M->") 'lvf-bot)
    map)
  "Keymap used by `lvf' in the minibuffer")

(defvar lvf-last-input nil "Latest input in lvf minibuffer")
(defvar lvf-run-timer nil "Monitor the user input")
(defvar lvf-result "" "Result line returned")
(defvar lvf-prefix-len 0 "Prefix length in result")
(defvar lvf-cache nil "Cache for lvf first build source list")
(defvar lvf-history nil "history input of lvf")

(defmacro with-lvf-buffer-window (&rest body)
  "Ensure to manipulating in lvf-buffer window"
  `(with-selected-window (get-buffer-window lvf-buffer) ,@body))

(defmacro with-lvf-prev-buffer-window (&rest body)
  "Ensure to manipulating in lvf-prev-buffer window"
  `(with-selected-window (get-buffer-window lvf-prev-buffer) ,@body))

(defun lvf-next ()
  (interactive)
  (with-lvf-buffer-window (forward-line)))

(defun lvf-prev ()
  (interactive)
  (with-lvf-buffer-window (forward-line -1)))

(defun lvf-pgdn ()
  (interactive)
  (with-lvf-buffer-window (scroll-up)))

(defun lvf-pgup ()
  (interactive)
  (with-lvf-buffer-window (scroll-down)))

(defun lvf-top()
  (interactive)
  (with-lvf-buffer-window (goto-char (point-min))))

(defun lvf-bot()
  (interactive)
  (with-lvf-buffer-window (goto-char (point-max))))

(defun lvf-interrupt ()
  (interactive)
  (cancel-timer lvf-run-timer)
  (setq lvf-result "")
  (abort-recursive-edit)
  (lvf-leave-minibuffer))

(defun lvf-end ()
  "The execute end of lvf"
  (interactive)
  (cancel-timer lvf-run-timer)
  (setq lvf-result (lvf-get-cursor-line))
  (lvf-leave-window)
  (lvf-leave-minibuffer)
  (lvf-act-fn)
  (setq lvf-result "")
  (setq lvf-prefix-len 0)
  (setq lvf-cache nil))

(defun lvf-leave-window ()
  (condition-case nil
      (delete-window (get-buffer-window lvf-buffer))
    (error nil)))

(defun lvf-leave-minibuffer ()
  (interactive)
  (ignore-errors (exit-minibuffer)))

(defun lvf-get-cursor-line ()
  (interactive)
  (with-current-buffer lvf-buffer
    (string-trim-right (thing-at-point 'line t))))

(defun lvf-display (str)
  (with-lvf-buffer-window
   (erase-buffer)
   (insert str)
   (goto-char (point-min))))

(defun lvf-run ()
  "Do not use this function directly, it is main entrance of lvf."
  ;; save prev buffer context
  (setq lvf-prev-buffer (current-buffer))
  (setq lvf-buffer (get-buffer-create "*lvf*"))
  (switch-to-buffer-other-window lvf-buffer t)
  (lvf-build-source)
  (setq lvf-run-timer (run-with-idle-timer 1 30 'lvf-run-fn))
  (add-to-history
   'lvf-history
   (read-from-minibuffer ">" nil lvf-minibuffer-map nil 'lvf-history))
  (lvf-end))

(defmacro lvf-define-type (name &rest body)
  `(defun ,(intern (format "lvf-%s" name)) ()
     ,(format "Use lvf query result, type (%s)." name)
     (interactive)
     (defalias 'lvf-build-source ',(intern (format "lvf-%s--build-source" name)))
     (defalias 'lvf-run-fn ',(intern (format "lvf-%s--run-fn" name)))
     (defalias 'lvf-act-fn ',(intern (format "lvf-%s--act-fn" name)))
     (lvf-run)))

(defmacro lvf-define-common-run-fn (str-list)
  "Define common lvf run function. Note the STR-LIST should alwauys be a string list"
  `(let ((input (minibuffer-contents-no-properties)))
     (if (not (string= lvf-last-input input))
         (progn
           (lvf-display
            (concat-list-to-string
             (str-list-regex-filter input ,str-list lvf-prefix-len)
             "\n"))
           (setq lvf-last-input input)))))


;;;
;;; lvf util
;;;
(defun str-list-regex-filter (regex str-list start)
  (let ((out-list
         (mapcar
          (lambda (x)
            (if (string-match-p regex x start) x nil))
          str-list)))
    (delete nil out-list)))

(defun concat-list-to-string (str-list sp)
  (mapconcat (lambda (x) x) str-list sp))

(defun get-buf-str-list (buf)
  (with-current-buffer buf
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))

(defun int-len-pad(n)
  "NUM is integer. Return padding string length that fits smaller integers"
  (int-to-string (length (int-to-string n))))

(defun large-buffer-p (buf)
  (with-current-buffer buf
    (< (* 1024 1024 2) (- (point-max) (point-min)))))


;;;
;;; lvf line
;;;
(defun lvf-line--build-source ()
  (if (large-buffer-p lvf-prev-buffer)
      (progn
        (user-error "LARGE FILE PANIC")))
  (with-current-buffer lvf-prev-buffer
    (let* ((line-list (get-buf-str-list lvf-prev-buffer))
           (pad (int-len-pad (length line-list)))
           (line-num 1)
           (out-list '()))
      (dolist (line line-list)
        (if (not (string= line ""))
            (setq out-list
                  (append out-list
                          (list (format (concat "%" pad "d|%s") line-num line)))))
        (setq line-num (1+ line-num)))
      (setq lvf-prefix-len (string-match "|" (car out-list)))
      (setq lvf-cache out-list))))

(defun lvf-line--run-fn ()
  (lvf-define-common-run-fn lvf-cache))

(defun lvf-line--act-fn ()
  (let ((line-num (string-to-number (string-trim (car (split-string lvf-result "|"))))))
    (with-lvf-prev-buffer-window
     (goto-char (point-min))
     (forward-line (1- line-num))
     (recenter nil))))

(lvf-define-type "line")


;;;
;;; lvf imenu
;;;
(defun setcar-prefix-str-fmt (s pad v)
  "Add prefix to a cons's car (the car's type must be the string)"
  (cons (format (concat "%" (int-to-string pad) "s %s") s (car v)) (cdr v)))

(defun lvf-imenu--build-source ()
  (with-lvf-prev-buffer-window
   (let ((min-pad 1)
         (out-alist '())
         (imenu-alist (imenu--make-index-alist)))
     (dolist (v imenu-alist)
       (if (not (markerp (cdr v)))
           (setq min-pad (max (length (car v)) min-pad))))
     (dolist (v imenu-alist)
       (if (markerp (cdr v))
           (setq out-alist
                 (append out-alist
                         (list (setcar-prefix-str-fmt "*" min-pad v)))))
       (if (listp (cdr v))
           (dolist (i (cdr v))
             (setq out-alist
                   (append out-alist
                           (list (setcar-prefix-str-fmt (car v) min-pad i)))))))
     ;; (message "%s" out-alist)
     ;; (dolist (v out-alist) (message "%s" (car v)))
     (setq lvf-prefix-len (string-match "[^ ] " (car (car out-alist))))
     (setq lvf-cache out-alist))))

(defun lvf-imenu--run-fn ()
  (lvf-define-common-run-fn (mapcar (lambda (x) (car x)) lvf-cache)))

(defun lvf-imenu--act-fn ()
  (with-lvf-prev-buffer-window
   (let ((m (cdr (assoc lvf-result lvf-cache))))
     (switch-to-buffer (marker-buffer m))
     (goto-char (marker-position m)))))

(lvf-define-type "imenu")

(provide 'i-lvf)

;;; i-lvf.el ends here
