;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; Commentary:
;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.
;;; Code:

;; nifty trick for speed up
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
;; (setq file-name-handler-alist nil)
;; (setq term-file-prefix nil)

;; minimalism's flavor
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(setq-default mode-line-format nil)
(setq initial-frame-alist '((fullscreen . maximized)))

;; quiet message when startup or close emacs
(setq inhibit-message t) ;; must set back to nil after load finished!
;; bellow line only kill message "For information about GNU Emacs..."
;; (advice-add #'display-startup-echo-area-message :override #'ignore)
;; quickly quit without verbose message
;; (advice-add #'kill-emacs :before #'(lambda () (setq inhibit-message t)))

(setq emacs-prepare-time (float-time)) ;; start to measure initial time
;;; early-init.el ends here
