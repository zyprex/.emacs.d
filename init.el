;;; init.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (dolist (subdir (directory-files (locate-user-emacs-file "lisp") t "\\w+"))
;;   (if (file-directory-p subdir)
;;       (add-to-list 'load-path subdir)))

;;; hooks
;; (defun before-init-setup ())
;; (add-hook 'before-init-hook 'before-init-setup)
;; init
(defun after-init-setup ()
;;; lazy load
  (setq lazy-load-time (float-time))
  (run-with-idle-timer
   0 nil
   (lambda ()
     ;; global minor mode
     ;; (desktop-save-mode)
     ;; (setq desktop-save 'ask-if-exists
     ;;       desktop-auto-save-timeout nil)
     (recentf-mode)
     (setq recentf-exclude '()
           recentf-max-menu-items 20
           recentf-max-saved-items 999)
     (savehist-mode)
     (setq savehist-additional-variables
           '(register-alist)
           savehist-autosave-interval 60)
     (save-place-mode)
     (setq save-place-limit 400)
     (show-paren-mode)
     (setq show-paren-when-point-inside-paren t
           show-paren-when-point-in-periphery t)
     (winner-mode) ;; C-c <left> and C-c <right>
     (global-auto-revert-mode)
     (delete-selection-mode)
     (global-so-long-mode)
     (if enable-recursive-minibuffers
         (minibuffer-depth-indicate-mode))
     (require 'i-pkg) ;; ~/.emacs.d/lisp/i-pkg.el
     (require 'i-abb) ;; ~/.emacs.d/lisp/i-abb.el
     (setq lazy-load-time (float-time (time-since lazy-load-time)))
     (message (format "Emacs is armed to the teeth in %.6f seconds" lazy-load-time))
     (setq lazy-load-time nil)
     (setq inhibit-message nil))) ;; end of all configuration!
  (run-with-idle-timer
   1 nil
   (lambda ()
     (put 'narrow-to-region 'disabled nil)
     (put 'narrow-to-page 'disabled nil)
     (with-eval-after-load 'calendar
       (setq
        calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
        calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]
        calendar-chinese-all-holidays-flag t
        calendar-latitude +120.214928
        calendar-longitude +30.252465
        ;; calendar-location-name ""
        calendar-mark-holidays-flag t
        calendar-mark-diary-entries-flag t
        calendar-view-holidays-initially-flag t)
       ;; p C
       (add-hook 'calendar-today-visible-hook 'calendar-mark-today))
     ))
  (run-with-idle-timer
   9 nil
   (lambda ()
     (setq gc-cons-threshold 20971520) ;; 20MB
     (setq gc-cons-percentage 0.1)))
  (run-with-timer
   300 nil
   (lambda ()
     (setq confirm-kill-emacs 'yes-or-no-p)))

;;; prerequisites
  (add-to-list 'load-path (locate-user-emacs-file "lisp"))
  (require 'i-lib) ;; ~/.emacs.d/lisp/i-lib.el
  (require 'i-lvf) ;; ~/.emacs.d/lisp/i-lvf.el
  ;; replace isearch but not the isearch-regex!
  ;; defined those keys early to undisturb minibuffer keymaps
  ;; (global-set-key-list
  ;;  '(("C-s"   search-n-char-forward)
  ;;    ("C-r"   search-n-char-backward)
  ;;    ("C-S-s" search-n-char-forward-repeat)
  ;;    ("C-S-r" search-n-char-backward-repeat)))

;;; defaults
  ;; (add-hook-list '(prog-mode-hook text-mode-hook) 'subword-mode)
  ;; (setq initial-buffer-choice "~/.emacs.d/init.el")
  (setq
   inhibit-splash-screen t
   initial-scratch-message ""
   echo-keystrokes 1 ; don't mess up minibuffer message
   site-run-file nil
   ;; split-width-threshold 100
   ;; split-height-threshold 60
   ;; delete-by-moving-to-trash t
   shift-select-mode nil
   visible-bell t
   ring-bell-function 'ignore
   use-dialog-box nil
   large-file-warning-threshold 104857600 ; 100M
   read-process-output-max 1048576 ; 1MB
   blink-cursor-mode nil
   ;; electric-pair-mode t
   fill-column 80
   ;; indicate-unused-lines t
   scroll-step 0
   scroll-margin 0
   hscroll-step 7
   hscroll-margin 7
   scroll-conservatively 101
   scroll-up-aggressively 1
   scroll-down-aggressively 1
   scroll-preserve-screen-position t
   ;; auto-revert-check-vc-info t
   auto-revert-interval 9)
  (setq-default
   tab-width 4
   indent-tabs-mode nil
   kill-ring-max 300
   history-length 500
   case-fold-search nil
   case-replace nil)
  ;; (setq enable-recursive-minibuffers t)
  ;; use C-M-c exit-recursive-edit, use C-] abort-recurse-edit
  ;; also have a look at `completing-read-multiple'
  ;; (fset 'yes-or-no-p 'y-or-n-p)
  (defalias 'list-buffers 'ibuffer)
  ;; (bookmark-bmenu-list) (switch-to-buffer "*Bookmark List*")
  ;; (ignore-errors)
  (make-directory "~/.cache/emacs/backup" t)
  (setq backup-directory-alist '(("" . "~/.cache/emacs/backup"))
        delete-old-versions t)
  (setq browse-url-browser-function 'eww-browse-url
        eww-search-prefix "https://www.bing.com/search?q="
        eww-form-checkbox-selected-symbol "☑"
        eww-form-checkbox-symbol "☐"
        eww-history-limit 100)
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 1)
  (setq isearch-lazy-count t
        lazy-count-prefix-format ""
        lazy-count-suffix-format " [%s/%s]")
  ;; enforce UTF-8 as the default encoding for all files
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (if (fboundp 'set-w32-system-coding-system)
      (set-w32-system-coding-system 'utf-8))
  ;; minibuffer completion
  (setq completion-show-help nil
        completion-show-inline-help t
        tmm-completion-prompt nil)
  (global-set-key (kbd "M-/") 'hippie-expand)
  ;; incremental minibuffer completion (or just use fido-mode)
  ;; recentf-open-files -> rof
  ;; bookmark-bmenu-list -> bbl
  (setq-default completion-styles '(initials flex))
  (icomplete-mode)
  (define-key minibuffer-local-map (kbd "C-c i") 'icomplete-mode)
  (define-key-list icomplete-minibuffer-map
    '(("C-c i" icomplete-mode)
      ([right] icomplete-forward-completions)
      ([left]  icomplete-backward-completions)
      ("C-n"   icomplete-forward-completions)
      ("C-p"   icomplete-backward-completions)
      ("C-s"   icomplete-forward-completions)
      ("C-r"   icomplete-backward-completions)
      ("C-j"   icomplete-force-complete)
      ("C-m"   icomplete-force-complete-and-exit)))
  (setq icomplete-hide-common-prefix nil
        icomplete-show-matches-on-no-input nil
        icomplete-tidy-shadowed-file-names t
        icomplete-compute-delay 0.3
        icomplete-delay-completions-threshold 20000
        icomplete-max-delay-chars 1
        icomplete-prospects-height 1
        icomplete-in-buffer t
        icomplete-separator " ")
  ;; interactively do things with buffers files tags
  (ido-mode)
  (define-key-list ido-common-completion-map
    '(("C-n" ido-next-match) ("C-p" ido-prev-match)))
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-enable-regexp t
        ido-case-fold nil
        ido-completion-buffer-all-completions t
        ido-enter-matching-directory t
        ido-max-dir-file-cache 500
        ido-max-prospects 20
        ido-max-window-height 1
        ido-decorations '("" "" " " " ···" "[" "]"
                          " [No match]" " [Matched]"
                          " [Not readable]" " [Too big]" " [Confirm]")
        ido-use-virtual-buffers t ;; virtual buffer from recentf-list and bookmark
        ido-auto-merge-delay-time 5)
  ;; imenu
  (setq imenu-max-items 250
        imenu-use-popup-menu nil)

;;; keymaps
  (global-set-key-list
   '(("M-;" nil) ;; prefix key
     ("M-; M-;" comment-dwim)
     ("M-; m" bookmark-set)
     ("M-; w" winner-undo)
     ("M-; W" winner-redo)
     ("M-; o" other-window-continuous)
     ("M-; c" delete-other-windows)
     ("M-; C" delete-window)
     ("M-; h" minimize-window)
     ("M-; l" maximize-window)
     ("M-; j" next-buffer-continuous)
     ("M-; k" previous-buffer-continuous)
     ("M-; b" ido-switch-buffer)
     ("M-; M-b" ido-switch-buffer-other-window)
     ("M-; <SPC>b" ibuffer)
     ("M-; <SPC>M-b" ibuffer-other-window)
     ("M-; f" ido-find-file)
     ("M-; M-f" ido-find-file-other-window)
     ("M-; <SPC>f" dired)
     ("M-; <SPC>M-f" dired-other-window)
     ;; need package in ~/.emacs.d/lisp/i-pkg.el
     ("M-; /" lvf-line)
     ("M-; ?" imenu)
     ("M-; M-?" imenu-anywhere)
     ;; see `windmove-default-keybindings'
     ;; ([left]  windmove-left)
     ;; ([right] windmove-right)
     ;; ([up]    windmove-up)
     ;; ([down]  windmove-down)
     ;; adjust defaults
     ("M-; M-:" eval-region)
     ("M-; M-z" zap-up-to-char)
     ("C-^"   mode-line-other-buffer) ; the vim-way
     ("<escape>" keyboard-escape-quit) ; same as C-g
     ;; need functions in ~/.emacs.d/lisp/i-lib.el
     ("<C-return>"   open-line-below)
     ("<S-return>"   open-line-below)
     ("<C-S-return>" open-line-above)
     ("<C-S-up>"     move-line-up)
     ("<C-S-down>"   move-line-down)
     ("<C-S-right>"  duplicate-line)
     ("M-; d" duplicate-line)
     ("M-; M-x" exex-cmd)
     ("M-; M-s" isearch-window-forward)
     ("M-; M-r" isearch-window-backward)
     ("M-n" rabbit-jump-forward)
     ("M-p" rabbit-jump-backward)
     ("M-; M-n" rabbit-jump-bot)
     ("M-; M-p" rabbit-jump-top)
     ([f2] nil) ;; redefined
     ;; ([f5] nil)
     ;; ([f6] nil)
     ;; ([f7] compile)
     ;; ([f8] recompile)
     ;; ([f9] nil)
     ;; ([f12]) nil)
     ))
  ;; (with-eval-after-load 'evil
  ;;   ;; (evil-global-set-key 'normal (kbd "C-.") 'next-buffer)
  ;;   (evil-global-set-key 'normal [left]  'windmove-left)
  ;;   (evil-global-set-key 'normal [right] 'windmove-right)
  ;;   (evil-global-set-key 'normal [up]    'windmove-up)
  ;;   (evil-global-set-key 'normal [down]  'windmove-down))

;;; local settings
  (make-directory "~/.emacs.d/data" t)
  (defvar local-data-dir "~/.emacs.d/data"
    "Put many data here to keep user emacs directory tidy")
  (defmacro local-data (f) `(expand-file-name ,f local-data-dir))
  (setq custom-file                  (local-data "custom.el")
        ido-save-directory-list-file (local-data "ido-last")
        recentf-save-file            (local-data "recentf")
        savehist-file                (local-data "history")
        eww-bookmarks-directory      (local-data "")
        save-place-file              (local-data "places")
        bookmark-default-file        (local-data "bookmarks"))
  (defvar local-init-file (locate-user-emacs-file "init-local.el")
    "Local emacs lisp for temporary use")
  (if (file-exists-p local-init-file)
      (load local-init-file)
    (write-region
     (concat-line
      '(";;; init-local.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-"
        ";;; Commentary:"
        ";;; Code:"
        ";; run: ctags -eR --kinds-c=+p -f TAGS_E"
        "(setq local-tags-table-list '())"
        "(setq local-c-compile-command \"make -k \")"
        ";;; -- auto generated by 'init.el'--" ))
     nil local-init-file)))
(add-hook 'after-init-hook 'after-init-setup)
;; terminal
;; (defun tty-setup-init ())
;; (add-hook 'tty-setup-hook 'tty-setup-init)
;; emacs
(defun emacs-startup-setup ()
  (with-current-buffer "*scratch*"
    (insert (concat ";; " (emacs-init-time) "\n")))
  ;; customiz interface
  (when (file-exists-p custom-file)
   (load custom-file)))
(add-hook 'emacs-startup-hook 'emacs-startup-setup)
;; window setup
(defun window-setup ()
  (require 'i-bar) ;; ~/.emacs.d/lisp/i-bar.el
  ;; themes: nord chocolate screry
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  ;; (load-theme 'wheatgrass t)
  ;; (load-theme 'dwale t)
  (load-theme 'nightraid t)
  (gui-font-set))
(add-hook 'window-setup-hook 'window-setup)
;; eshell
(defun eshell-setup ()
  "Setup for eshell"
  (define-key eshell-mode-map [up] 'windmove-up)
  (define-key eshell-mode-map [down] 'windmove-down)
  (setq-local eshell-hist-ignoredups t
              eshell-cmpl-autolist t
              eshell-cmpl-cycle-completions nil
              eshell-cmpl-ignore-case t
              eshell-history-size 200))
(add-hook 'eshell-mode-hook 'eshell-setup)
;; org
(defun org-mode-setup ()
  "Setup for org mode"
  ;; (dolist (k '("C-'" "C-." "C-,"))
    ;; (define-key org-mode-map (kbd k) nil))
  (setq org-hide-leading-stars t
        org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers nil
        org-image-actual-width 600
        org-startup-with-inline-images t))
(add-hook 'org-mode-hook 'org-mode-setup)
;; view
(defun view-mode-setup ()
  "Setup for view mode"
  (evil-emacs-state))
(add-hook 'view-mode-hook 'view-mode-setup)
;; dired
;; (put 'dired-find-alternate-file 'disabled nil)
;; (with-eval-after-load 'dired
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
(setq dired-listing-switches "-alh")
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ;; use ( toggle
;; ibuffer
(with-eval-after-load 'ibuffer
  (setq ibuffer-default-sorting-mode 'alphabetic
        ibuffer-default-sorting-reversep t)
  ;; use human readable size column instead of original one
  ;; modify the default ibuffer-formats
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " " (name 16 -1) " " filename))))
;; (add-hook 'ibuffer-mode-hook (lambda ()))
;; prog
(defun prog-mode-setup ()
  "Setup for programing mode"
  ;; (setq display-line-numbers-type 'relative)
  (setq display-line-numbers-type t)
  (setq display-line-numbers-width 1)
  (display-line-numbers-mode)
  ;; (linum-on)
  (setq-local fill-column 80)
  (display-fill-column-indicator-mode)
  (hl-line-mode)
  (toggle-truncate-lines)
  ;; (which-func-mode)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (setq compilation-window-height 10)
  (setq show-trailing-whitespace 1))
(add-hook 'prog-mode-hook 'prog-mode-setup)
;; c
(defun c-mode-setup ()
  "Setup for c files"
  (setq compile-command local-c-compile-command)
  ;; (delete 'company-clang company-backends)
  (if (boundp 'company-mode)
      (setq-local company-backends
                  '(company-capf
                    company-dabbrev-code
                    company-dabbrev
                    company-files
                    company-keywords)))
  ;; (if (boundp 'company-fuzzy-mode)
  ;;     (company-fuzzy-mode))
  (if (boundp 'local-tags-table-list)
      (progn
        (push 'company-etags company-backends)
        (setq-default tags-revert-without-query t
                      tags-add-tables nil
                      tags-table-list local-tags-table-list))))
(add-hook 'c-mode-hook #'c-mode-setup)
;; elisp
(defun elisp-mode-setup ()
  "Setup for elisp files"
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-setup)
;; prog & text
;; html, web
(defun html-web-mode-setup()
  (setq-local tab-width 2)
  (define-key web-mode-map (kbd "M-;") nil)
  (define-key web-mode-map (kbd "M-; M-;") 'web-mode-comment-or-uncomment)
  ;; (setq-local tab-width 2)
  (if (fboundp 'company-web-html)
      (push 'company-web-html company-backends))
  (push 'company-css company-backends))
;; (add-hook 'mhtml-mode-hook 'html-web-mode-setup)
(add-hook 'web-mode-hook 'html-web-mode-setup)


;; use menu
;; (easy-menu-define git-menu nil
;;   "Menu for git alias."
;;   '("Git"
;;     "---"
;;     ("Common"
;;      ["Add all" (message "OK")]
;;      )
;;     ))
;; (easy-menu-add-item nil nil git-menu)


;;; init.el ends here
