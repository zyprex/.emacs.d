;;; i-lib.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    My library, utilities function or macro for emacs etc.
;;
;;; Code:
(defmacro concat-line (str-list) `(mapconcat 'identity ,str-list "\n"))

(defmacro add-hook-list (hook-list function)
  "Add many hook in one function"
  `(let ((hk)) (dolist (hk ,hook-list) (add-hook hk ,function))))

(defmacro global-set-key-list (list)
  "Global set key by list of (key command), key can be a vector or string."
  `(dolist (i ,list)
     (global-set-key
      (cond
       ((eq 'vector (type-of (car i))) (car i))
       ((eq 'string (type-of (car i))) (kbd (car i)))
       (t nil))
      (car (cdr i)))))

(defmacro define-key-list (map list)
  "Define key in MAP, use a list of (key command)"
  `(dolist (i ,list)
     (define-key ,map
      (cond
       ((eq 'vector (type-of (car i))) (car i))
       ((eq 'string (type-of (car i))) (kbd (car i)))
       (t nil))
      (car (cdr i)))))

(defmacro measure-elisp-time (&rest body)
  "Measure the time it takes to evaluate elisp BODY."
  `(let ((time (current-time)))
     ,@body
     (format "%.06fs" (float-time (time-since time)))))

(defun find-init-file ()
  "Find init.el file."
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))

(defun kill-other-buffers ()
  "Kill all but current buffers"
  (interactive)
  (if (yes-or-no-p "Kill other buffers? ")
      (dolist (buffer (buffer-list))
        (unless (or (eq buffer (current-buffer))
                    (string-equal (buffer-name buffer) "*scratch*")
                    (string-equal (buffer-name buffer) "*Messages*"))
          (kill-buffer buffer)))))

(defun clear-register-alist () (interactive) (setq register-alist '()))
;; (defun clear-recentf-list () (interactive) (setq recentf-list '()))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line 1)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun guess-font (fonts-list)
  "Guess first accessible font in fonts-list"
  (catch 'e
    (dolist (font fonts-list)
      (if (member font (font-family-list))
	    (throw 'e font)
	nil))))

(defun gui-font-set ()
  "Set GUI font, this func should be load after theme load,
use `fc-list | fzf` search all fonts"
  (interactive)
  (when (display-graphic-p)
    ;; font size
    (set-face-attribute 'default nil :height 140) ; unit 1/10pt
    (set-face-attribute 'mode-line nil :height 100)
    (set-face-attribute 'mode-line-inactive nil :height 100)
    ;; font family
    (set-frame-font
     (guess-font '("CamingoCode" "Consolas" "DejaVu Sans Mono")))
    (set-face-font
     'fixed-pitch
     (guess-font '("Fixedsys" "Consolas" "Inconsolata")))
    (set-fontset-font
     t 'symbol (guess-font '("Segoe UI Symbol" "Symbola")))
    (set-fontset-font
     t 'han
     (guess-font '("微软雅黑" "等距更纱黑体 SC" "Microsoft JhengHei" "黑体"
                   "楷体" "Noto Sans CJK")))
    (set-fontset-font
     t '(#x1f300 . #x1fad0)
     (guess-font '("Noto Color Emoji" "Noto Emoji" "Segoe UI Emoji"
                   "Symbola" "Apple Color Emoji")))))

(defun package-get ()
  "Install selected package from list `package-list'"
  (interactive)
  (package-initialize)
  (setq package-selected-packages package-list)
  (package-install-selected-packages))

(defun package-sync ()
  "Ensure all required package downloaded"
  (catch 'e
    (dolist (pkg package-list)
      (when (not (require 'pkg nil 'noerror))
        (throw 'e (package-get))))))

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tags-names)
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tags-names)))
	  tags-completion-table)
    (xref-find-definitions (ido-completing-read "TAGS:" tags-names))))

(defmacro ft-init-function (regex fn)
  "Run file type function, when the open file match the REGEX"
  `(when (and (buffer-file-name) (string-match ,regex (buffer-name)))
     (funcall ,fn)))

(defmacro major-mode-init-function (mode fn)
  "Run function FN if mode equal major-mode"
  `(if (equal major-mode ,mode) (funcall ,fn)))

(defun memo-open ()
  (interactive)
  (let ((f (concat "~/.emacs.d/memo/m-" (symbol-name major-mode) ".org")))
    (if (file-exists-p f)
        (find-file-other-window f)
      (find-file-other-window "~/.emacs.d/memo/m-emacs.org"))))

(defun project-x-try-local (dir)
  ;; Steal from project-x.el
  "Determine if DIR is a non-VC project.
   DIR must include a .project file to be considered a project."
  (if-let ((root (locate-dominating-file dir ".project")))
      (cons 'local root)))
(add-hook 'project-find-functions 'project-x-try-local 50)

(defun exex-cmd ()
  "Call `execute-extended-command' the default way."
  (interactive)
  (let ((completion-styles '(basic partial-completion emacs22)))
    (call-interactively 'execute-extended-command)))

(defmacro def-ex-prog-cmd (cmd-name where)
  "Define the command that execute exteral program"
  `(defun ,(intern cmd-name) ()
       (interactive) (start-process ,cmd-name nil ,where)))

;; duplicate line
(defun duplicate-line (&optional n)
  "Duplicate Current Line, make more than 1 copy if given a numeric argument"
  (interactive "p")
  (message "%s" n)
  (move-beginning-of-line 1)
  (dotimes (i (abs (or n 1)))
    (insert (thing-at-point 'line))))

(defun f2-quick-bind (&optional arg)
  "Bind F2 to command quickly"
  (interactive "Cf2 quick bind (default is last-command):")
  (if (commandp arg)
      (progn
        (local-set-key [f2] arg)
        (message "F2 bind to command: %s" arg))
    (local-set-key [f2] last-command)
    (message "F2 bind to last command: %s" last-command)))

(defun get-buffers-same-mode (mode)
  "Return a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defmacro define-fn-continuous (name key &rest body)
  "Define function called NAME-continuous,
 and this function can repeat its self by continuous press KEY"
  (let ((fun-name (format "%s-continuous" name)))
    `(defun ,(intern fun-name) ()
       (interactive)
       ,@body
       (message (format "Press '%s' to repeat '%s'" ,key ,fun-name))
       (set-transient-map
        (let ((kmap (make-sparse-keymap)))
          (define-key kmap ,key ',(intern fun-name))
          kmap)))))

(define-fn-continuous "other-window-prev" (kbd "i") (other-window -1))
(define-fn-continuous "other-window-next" (kbd "o") (other-window 1))
(define-fn-continuous "next-buffer" (kbd "j") (next-buffer))
(define-fn-continuous "prev-buffer" (kbd "k") (previous-buffer))

(defvar window-layout-list '() "Store windows layouts")

(defun save-window-layout ()
  "Save current window layout to alias (a char)"
  (interactive)
  (let ((i '(:alias nil :name nil :layout nil))
        (ch (read-char "Set current windows layout alias (press one key)...")))
    ;; clear duplicates
    (dolist (l window-layout-list)
      (if (= ch (plist-get l :alias))
          (setq window-layout-list (remove l window-layout-list))))
    (setq i (plist-put i :alias ch))
    (setq i (plist-put i :name (buffer-name)))
    (setq i (plist-put i :layout (current-window-configuration)))
    (add-to-list 'window-layout-list (copy-tree i))))

(defun load-window-layout ()
  "Load a window layout from `window-layout-list'"
  (interactive)
  (let* ((echo-prompt
          (mapcar
           (lambda (x)
             (concat
              (propertize (format "%c:" (plist-get x :alias))
                          'face 'minibuffer-prompt)
              (format "%s" (plist-get x :name))))
           window-layout-list))
         (echo-prompt
          (sort
           echo-prompt
           (lambda (str1 str2)
             (< (string-to-char (substring str1 0 1))
                (string-to-char (substring str2 0 1))))))
         (echo-prompt (mapconcat (lambda (x) x) echo-prompt " "))
         (ch (read-char echo-prompt)))
    (dolist (l window-layout-list)
      (if (= ch (plist-get l :alias))
          (set-window-configuration (plist-get l :layout))))))

(provide 'i-lib)

;;; i-lib.el ends here
