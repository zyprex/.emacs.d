;;; dwale-theme.el --- dwale dark custom theme

;; Copyright (C) 2011-2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; Code:
(deftheme dwale "Dwale color theme")

(custom-theme-set-faces
 'dwale
 '(default ((t (:background "gray10" :foreground "gray80"))))
 ;; diff
 '(diff-added ((t (:inherit diff-changed :background "dark green"))) t)
 '(diff-changed ((t (:background "midnight blue"))) t)
 '(diff-indicator-added ((t (:inherit diff-indicator-changed))) t)
 '(diff-indicator-changed ((t (:weight bold))) t)
 '(diff-indicator-removed ((t (:inherit diff-indicator-changed))) t)
 '(diff-removed ((t (:inherit diff-changed :background "dark red"))) t)
 ;; dired
 '(dired-directory ((t (:foreground "DodgerBlue" :weight bold))))
 ;; eshell
 '(eshell-prompt ((t (:inherit font-lock-function-name-face :weight bold))))
 ;; font lock
 '(font-lock-builtin-face ((t (:foreground "chartreuse2"))))
 '(font-lock-comment-face ((t (:foreground "gray60"))))
 '(font-lock-constant-face ((t (:foreground "dodger blue"))))
 '(font-lock-doc-face ((t (:foreground "indian red"))))
 '(font-lock-function-name-face ((t (:foreground "spring green"))))
 '(font-lock-keyword-face ((t (:foreground "light sea green" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "cornflower blue"))))
 '(font-lock-string-face ((t (:foreground "light salmon"))))
 '(font-lock-type-face ((t (:foreground "medium purple"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow green"))))
 '(font-lock-warning-face ((t (:foreground "hot pink" :weight bold))))
 ;; interface
 '(minibuffer-prompt ((t (:foreground "gold2" :weight normal))))
 '(scroll-bar ((t (:background "gray20" :foreground "dark turquoise"))))
 '(show-paren-match ((t (:background "DeepSkyBlue4"))))
 '(show-paren-mismatch ((t (:background "dark magenta"))))
 ;; line
 '(mode-line ((t (:background "gray30" :box (:line-width 1 :color "red")))))
 `(mode-line-buffer-id ((t (:foreground "coral" :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray20" :foreground "thistle"))))
 ;; '(header-line ((t (:inherit mode-line :inverse-video t :box (:line-width -1 :color "red" :style released-button)))))
 '(header-line ((t (:inherit mode-line :box (:line-width -1 :color "red")))))
 ;; highlight
 '(highlight ((t (:background "sea green"))))
 '(link ((t (:foreground "SteelBlue2" :underline t))))
 '(hl-line ((t (:background "grey25"))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(region ((t (:background "SeaGreen4"))))
 '(secondary-selection ((t (:background "#333366" :foreground "#f6f3e8"))))
 '(error ((t (:foreground "deep pink" :weight bold))))
 ;; `(escape-glyph ((t (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
 ;; `(homoglyph ((t (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
 ;; which-func
 `(which-func ((t (:foreground "DeepSkyBlue1"))))
 ;; compilation
 `(next-error ((t (:inherit region :background "SkyBlue"))))
 ;; icomplete
 '(icomplete-first-match ((t (:foreground "deep sky blue" :weight bold))))
 ;; ido
 '(ido-first-match ((t (:foreground "turquoise" :weight bold))))
 '(ido-only-match ((t (:foreground "medium spring green" :weight bold))))
 '(ido-subdir ((t (:inherit dired-directory :weight normal))))
 '(menu ((t (:background "gray30" :foreground "gray70"))))
 ;; org
 '(org-agenda-date ((t (:inherit org-agenda-structure))) t)
 '(org-agenda-date-today ((t (:inherit org-agenda-date :underline t))) t)
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "green"))) t)
 '(org-agenda-done ((t (:foreground "#269926"))))
 '(org-agenda-restriction-lock ((t (:background "#FFB273"))))
 '(org-agenda-structure ((t (:foreground "#4671D5" :weight bold))))
 '(org-date ((t (:foreground "medium sea green" :underline t))))
 '(org-done ((t (:foreground "#008500" :weight bold))))
 '(org-drawer ((t (:foreground "#2A4480"))))
 '(org-ellipsis ((t (:foreground "#FF7400" :underline t))))
 '(org-footnote ((t (:foreground "#1240AB" :underline t))))
 '(org-hide ((t (:foreground "gray20"))))
 '(org-level-1 ((t (:inherit outline-1 :box nil))))
 '(org-level-2 ((t (:inherit outline-2 :box nil))))
 '(org-level-3 ((t (:inherit outline-3 :box nil))))
 '(org-level-4 ((t (:inherit outline-4 :box nil))))
 '(org-level-5 ((t (:inherit outline-5 :box nil))))
 '(org-level-6 ((t (:inherit outline-6 :box nil))))
 '(org-level-7 ((t (:inherit outline-7 :box nil))))
 '(org-level-8 ((t (:inherit outline-8 :box nil))))
 '(org-scheduled-previously ((t (:foreground "#FF7400"))))
 '(org-tag ((t (:weight bold))))
 '(org-todo ((t (:foreground "#FF6961" :weight bold))))
 ;; outline
 '(outline-1 ((t (:foreground "cyan1" :weight bold))))
 '(outline-2 ((t (:foreground "SeaGreen1" :weight bold))))
 '(outline-3 ((t (:foreground "cyan3" :weight bold))))
 '(outline-4 ((t (:foreground "SeaGreen3" :weight bold))))
 '(outline-5 ((t (:foreground "LightGoldenrod1" :weight bold))))
 '(outline-6 ((t (:foreground "light salmon" :weight bold))))
 '(outline-7 ((t (:foreground "pale goldenrod" :weight bold))))
 '(outline-8 ((t (:foreground "OliveDrab1" :weight bold))))
 ;; rcirc
 '(rcirc-my-nick ((t (:foreground "SpringGreen1" :weight bold))) t)
 '(rcirc-other-nick ((t (:foreground "dodger blue"))) t)
 '(rcirc-track-keyword ((t (:foreground "DodgerBlue" :weight bold))) t)
 '(rcirc-track-nick ((t (:background "yellow" :foreground "DodgerBlue" :weight bold))) t))

(provide-theme 'dwale)

;;; dwale-theme.el ends here
