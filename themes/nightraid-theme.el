;;; nightraid-theme.el --- A dark theme suitable for deep night
;; Copyright (C) 2011-2021 Free Software Foundation, Inc.
;; Put this file to your custom theme directory
;; Code:
(deftheme nightraid "Nightraid color theme,
A dark theme suitable for deep night")

(defun termcolor-fallback (gui term)
  "Check true color support state, if not support,
use TERM color as fallback"
  (if (or (= (display-color-cells) 16777216)
          (display-graphic-p))
      gui term))

(let ((class '((class color) (min-colors 16)))
      ;; background
      (bg0 (termcolor-fallback "#000000" "#000000"))
      (bg1 (termcolor-fallback "#242424" "#242424"))
      ;; foreground
      (fg0 (termcolor-fallback "#228b22" "#228b22"))
      (fg1 (termcolor-fallback "#60b060" "white"))
      ;; dim
      (dm0 (termcolor-fallback "#242424" "#242424"))
      (dm1 (termcolor-fallback "#666666" "#666666"))
      ;; highlight
      (hl0 (termcolor-fallback "red" "red"))
      (hl1 (termcolor-fallback "DarkGoldenrod" "yellow"))
      (hl2 (termcolor-fallback "brown" "brown"))
      (hl3 (termcolor-fallback "#2C78BF" "blue"))
      (hl4 (termcolor-fallback "magenta" "magenta"))
      (hl5 (termcolor-fallback "cyan" "cyan")))
;;; paint all face color!
  (custom-theme-set-faces
   'nightraid
;; default
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:weight bold :slant italic))))
   `(default ((,class (:background ,bg0 :foreground ,fg0))))
   `(match ((,class (:background ,fg1 :foreground ,bg0))))
   `(error ((,class (:foreground ,hl0 :weight bold))))
   `(escape-glyph ((,class (:foreground ,hl5))))
   `(font-lock-builtin-face ((,class (:foreground ,fg0))))
   `(font-lock-string-face ((,class (:foreground ,fg1))))
   `(font-lock-comment-face ((,class (:foreground ,dm1))))
   `(font-lock-keyword-face ((,class (:foreground ,hl2 :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,hl1))))
   `(font-lock-doc-face ((,class (:foreground ,dm1))))
   `(font-lock-function-name-face ((,class (:foreground ,fg0 :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg0 :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,fg0))))
   `(font-lock-type-face ((,class (:foreground ,hl3))))
   `(font-lock-warning-face ((,class (:foreground ,hl0 :weight bold))))
;; diff
   `(diff-header ((,class (:inherit 'default :inverse-video t))))
   `(diff-hunk-header ((,class (:inherit 'default :inverse-video t))))
   `(diff-file-header ((,class (:inherit 'escape-glyph :inverse-video t))))
   `(diff-added ((,class (:inherit 'default :inverse-video t))))
   `(diff-change ((,class (:background ,hl0))))
   `(diff-removed ((,class (:inherit 'error :inverse-video t))))
;; UI
    `(highlight ((,class (:foreground ,bg0 :background ,hl0))))
    `(cursor ((,class (:foreground ,bg0 :background ,hl1))))
    `(fringe ((,class (:background ,bg0))))
    `(mode-line ((,class (:foreground ,fg0 :background ,bg1))))
    `(mode-line-buffer-id ((,class (:weight bold))))
    `(mode-line-inactive ((,class (:foreground ,dm1 :background ,bg0))))
    `(header-line ((,class (:inherit 'mode-line))))
    `(hl-line ((,class (:background ,dm0))))
    `(line-number ((,class (:foreground ,dm1 :background ,bg0))))
    `(line-number-current-line ((,class (:foreground ,hl1 :background ,bg0))))
    `(minibuffer-prompt ((,class (:foreground ,hl1 :weight bold))))
    `(region ((,class (:foreground ,bg0 :background ,dm1))))
    `(secondary-selection ((,class (:foreground ,bg0 :background ,dm0))))
;; custom
    `(custom-button ((,class (:background ,bg0 :foreground ,fg0 :box (:line-width 2 :color ,hl3 :style sunken-button)))))
;; built-in packages
    `(link ((,class (:foreground ,hl3 :underline t))))
    `(link-visited ((,class (:foreground ,hl4 :underline t))))
    `(widget-field ((,class (:background ,bg0 :foreground ,fg1 :box (:color ,fg1)))))
    `(icomplete-first-match ((,class (:foreground ,fg1))))
    `(ido-first-match ((,class (:foreground ,fg1))))
    `(ido-only-match ((,class (:foreground ,hl0))))
    `(ido-subdir ((,class (:foreground ,hl1))))
    `(ido-virtual ((,class (:foreground ,dm1))))
    `(which-func ((,class (:foreground ,fg1))))
    `(fill-column-indicator ((,class (:foreground ,dm0))))
    `(show-paren-match ((,class (:foreground ,hl4))))
    `(show-paren-mismatch ((,class (:foreground ,bg0 :background ,hl0))))
    `(vc-state-base ((,class (:foreground ,hl4))))
;; outline
    `(outline-1 ((,class (:foreground "cyan1" :weight bold))))
    `(outline-2 ((,class (:foreground "SeaGreen1" :weight bold))))
    `(outline-3 ((,class (:foreground "cyan3" :weight bold))))
    `(outline-4 ((,class (:foreground "SeaGreen3" :weight bold))))
    `(outline-5 ((,class (:foreground "LightGoldenrod1" :weight bold))))
    `(outline-6 ((,class (:foreground "light salmon" :weight bold))))
    `(outline-7 ((,class (:foreground "pale goldenrod" :weight bold))))
    `(outline-8 ((,class (:foreground "OliveDrab1" :weight bold))))
;; 3rd part packages
    ;; > company
    `(company-tooltip ((,class (:foreground ,dm1 :background ,bg0))))
    `(company-tooltip-common ((,class (:foreground ,hl2))))
    `(company-tooltip-selection ((,class (:background ,dm0))))
    `(company-tooltip-mouse ((,class (:foreground ,bg0 :background ,hl0))))
    ;; > markdown
    `(markdown-header-face-1 ((,class (:inherit 'outline-1))))
    `(markdown-header-face-2 ((,class (:inherit 'outline-2))))
    `(markdown-header-face-3 ((,class (:inherit 'outline-3))))
    `(markdown-header-face-4 ((,class (:inherit 'outline-4))))
    `(markdown-header-face-5 ((,class (:inherit 'outline-5))))
    `(markdown-header-face-6 ((,class (:inherit 'outline-6))))
 )
)

(provide-theme 'nightraid)

;;; nightraid-theme.el ends here
