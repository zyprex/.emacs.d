;;; i-pkg.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(defvar package-list
  '(evil
    undo-tree
    key-chord
    keyfreq
    which-key
    company-web
    company-fuzzy
    company
    imenu-list
    dim
    eglot
    manage-minor-mode
    web-mode
    markdown-mode
    go-mode
    chocolate-theme
    nord-theme
    srcery-theme
    rainbow-mode)
  "Package list")
;; allowed old gnu packages to be installed
(setq package-check-signature nil)
;; check all package downloaded!
;; will execute (package-initialize)
(package-sync)
;; (unless package-archive-contents
;;   (package-refresh-contents)
;;   (message "First run, refresh content down!"))
;; ========================================
;;                keyfreq
;; ========================================
(keyfreq-mode)
;; ========================================
;;                which-key
;; ========================================
(setq which-key-idle-delay 2
      which-key-popup-type 'side-window)
(which-key-mode)
;; ========================================
;;                dim
;; ========================================
(dim-minor-names
 '((auto-fill-function " ↵")
   (isearch-mode       "")
   (which-key-mode     ""   which-key)
   (eldoc-mode         ""   eldoc)
   (flymake-mode       ""   flymake)
   ;; (subword-mode       ""   subword)
   ;; (company-mode       ""   company)
   (company-fuzzy-mode " *" company-fuzzy)
   (whitespace-mode    " _" whitespace)))
;; ========================================
;;                key-chord
;; ========================================
(key-chord-mode)
(defun pair-in (pair) (insert pair) (left-char))
(dolist (pair '("()" "[]" "{}" "<>" "\"\"" "''"))
  (key-chord-define-global
   (kbd pair)
   (lambda () (interactive)(pair-in pair))))
;; ========================================
;;                markdown
;; ========================================
(add-hook 'markdown-mode-hook
  (lambda () (setq markdown-inline-image-overlays t)))
;; ========================================
;;                company
;;      http://company-mode.github.io/
;; ========================================
(global-company-mode)
;; (global-company-fuzzy-mode)
(setq company-fuzzy-prefix-on-top t)
(setq company-idle-delay 0.7
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-tooltip-maximum-width 60
      company-show-quick-access t)
;; ========================================
;;                undo-tree
;; ========================================
(global-undo-tree-mode)
(setq undo-tree-mode-lighter ""
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("" . "~/.cache/emacs/undo")))
;; ========================================
;;                evil
;; https://evil.readthedocs.io/en/latest/overview.html
;; ========================================
(evil-mode)
(setq evil-default-state 'emacs
      evil-echo-state nil
      evil-motion-state-modes '()
      evil-mode-line-format '(before . mode-line-front-space)
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)
(evil-set-undo-system 'undo-tree)
(evil-set-initial-state 'prog-mode 'normal)
(evil-set-initial-state 'text-mode 'emacs)
(evil-set-initial-state 'evil-command-window-mode 'insert)
(setq
 evil-normal-state-tag
 (propertize " N " 'face `(:foreground "Black" :background "chocolate2"))
 evil-insert-state-tag
 (propertize " I " 'face `(:foreground "Black" :background "green1"))
 evil-visual-char-tag
 (propertize " V " 'face `(:foreground "Black" :background "royal blue"))
 evil-visual-line-tag
 (propertize "V-L" 'face `(:foreground "Black" :background "royal blue"))
 evil-visual-block-tag
 (propertize "V-B" 'face `(:foreground "Black" :background "royal blue"))
 evil-motion-state-tag
 (propertize " M " 'face `(:foreground "Black" :background "dark olive green"))
 evil-emacs-state-tag
 (propertize " E " 'face `(:foreground "Black" :background "White"))
 evil-replace-state-tag
 (propertize " R " 'face `(:foreground "Black" :background "Red"))
 evil-operator-state-tag
 (propertize " O " 'face `(:foreground "Black" :background "yellow1")))
;; (add-hook 'evil-after-load-hook 'force-mode-line-update)
(evil-set-leader 'normal "s")
(evil-define-operator evil-comment (beg end)
  (comment-or-uncomment-region beg end))
(evil-define-key 'normal 'global (kbd "gc") 'evil-comment)
(evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>a") 'evil-buffer)
;; evil's normal state is not perfect
(evil-define-key 'normal 'global (kbd "C-n") 'next-line)
(evil-define-key 'normal 'global (kbd "C-p") 'previous-line)
(evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up) ;; ESC + num
;; (evil-define-key 'normal 'global (kbd "C-a") 'increment-number-at-point)
;; (evil-define-key 'normal 'global (kbd "C-x") 'decrement-number-at-point)
(evil-define-key 'insert 'global (kbd "C-h") 'backward-delete-char-untabify)
(evil-define-key 'insert 'global (kbd "C-x a") 'evil-paste-last-insertion)
(evil-define-key 'insert 'global (kbd "C-a") 'beginning-of-line)
;; evil's emacs state is not perfect
(evil-define-key 'emacs 'global (kbd "C-SPC") 'set-mark-command)
(evil-define-key 'emacs 'global (kbd "C-x SPC") 'rectangle-mark-mode)
;; ========================================
;;                eglot
;; ========================================
(setq eglot-send-changes-idle-time 1)
;; ========================================
;;                web-mode
;;         https://web-mode.org/
;; ========================================
(add-to-list 'auto-mode-alist '("\\.\\(?:html\\|htm\\|.xhtml\\)\\'" . web-mode))

;;; after all packages load
;; due to lazy load, mode package should load manually!
;; not elegant code , but it just work.
(ft-init-function
 "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
 'markdown-mode)
(ft-init-function "\\.\\(?:go\\|.mod\\)\\'" 'go-mode)
(ft-init-function "\\.\\(?:html\\|htm\\|.xhtml\\)\\'" 'web-mode)

(provide 'i-pkg)

;;; i-pkg.el ends here
