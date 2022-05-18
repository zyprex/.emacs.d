;;; i-bar.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;   private customized statusbar (Emacs mode line)
;;; Code:

(defun copy-file-path ()
  (interactive)
  (let ((f (buffer-file-name)))
    (if f
        (progn
          (kill-new f)
          (message "Copied: '%s'" f)))))

(defun file-directory ()
  "Check whether the default directory is not in current file's
directory, if not, return file's directory"
  (if (buffer-file-name)
    (let* ((dir (file-name-directory (buffer-file-name))))
      (if (string-equal (expand-file-name default-directory)
                        (expand-file-name dir))
          nil
        dir))
    nil))

(defun file-modified-p ()
  "True file changed"
  (and (buffer-file-name) (buffer-modified-p)))

(defun cd-file-directory ()
  "Change directory use `file-directory'true"
  (interactive)
  (if (file-directory)
      (cd (file-directory))))

(defun shorten-string-prefix (str len)
  (if (> (string-width str) len)
      (concat (substring str 0 len) "…")
  str))

(defun shorten-default-directory (len)
  (let ((d (abbreviate-file-name default-directory)))
    (if (> (string-width d) len)
        (concat
         "…"
         (substring d (- (string-width str) len) (string-width d)))
      d)))

(defun shrink-replace (short long)
  (if (or (< (window-width) (frame-width))
          (< (window-pixel-width) (display-pixel-width)))
      short
    long))

(defun str-displayable-fallback (ustr str)
  (if (char-displayable-p (string-to-char ustr)) ustr str))

(defun shorten-major-mode-name ()
  (cond
   ((eq major-mode 'inferior-emacs-lisp-mode) "IEL")
   ((eq major-mode 'lisp-interaction-mode) "LI")
   ((eq major-mode 'emacs-lisp-mode) "EL")
   ((eq major-mode 'c-mode) "C")
   ((eq major-mode 'c++-mode) "Cpp")
   ((eq major-mode 'markdown-mode) "Md")
   ((eq major-mode 'text-mode) "Txt")
   ((eq major-mode 'fundamental-mode) "?")
   ((eq major-mode 'package-menu-mode) "Pkg")
   ((eq major-mode 'messages-buffer-mode) "Msg")
   ((eq major-mode 'bookmark-bmenu-mode) "Bookmark")
   (t (capitalize (string-trim (symbol-name major-mode) nil "-mode")))))


(defgroup i-bar nil "i-bar group for customized" :group 'faces)
(defmacro i-bar-defface (name body docstring)
  `(defface ,(intern (format "i-bar/%s-face" name))
     ,body
     ,(format "Face used for show %s in mode line" docstring)
     :group 'i-bar))

(i-bar-defface "dir-true"
               '((t (:inherit 'font-lock-string-face)))
               "true directory path")
(i-bar-defface "dir-untrue"
               '((t (:inherit 'i-bar/dir-true-face :inverse-video t)))
               "untrue directory path")
(i-bar-defface "dir-prj"
               '((t (:inherit 'font-lock-constant-face :weight bold)))
               "current project type")
(i-bar-defface "dir-prj-path"
               '((t (:inherit 'font-lock-constant-face)))
               "current project path")
(i-bar-defface "mo"
               '((t (:foreground "Red" :weight bold)))
               "modified state")
(i-bar-defface "ro"
               '((t (:foreground "goldenrod" :weight bold)))
               "read-only")
(i-bar-defface "bname-mo"
               '((t (:inherit 'mode-line-buffer-id :inverse-video t)))
               "modified buffer name")
(i-bar-defface "ime"
               '((t (:foreground "chartreuse1")))
               "IME")
(i-bar-defface "major-mode"
               '((t (:inherit 'font-lock-type-face :weight bold)))
               "major-mode")
(i-bar-defface "minor-mode"
               '((t (:inherit 'font-lock-comment-face :weight bold)))
               "minor-mode")
(i-bar-defface "pos"
               '((nil ()))
               "position")
(i-bar-defface "pos-region"
               '((t (:inherit 'mode-line-emphasis :inverse-video t)))
               "position of region")


(defvar i-bar/dir-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'cd-file-directory)
    (define-key map [mode-line mouse-2] 'copy-file-path)
    (define-key map [mode-line mouse-3] 'cd)
      map))

(defun i-bar/dir-prj (cface)
  (let* ((root (cdr (project-current)))
         (type (symbol-name (car (project-current))))
         (path (string-replace root ""
                               (abbreviate-file-name default-directory))))
  (list (propertize type
                    'face 'i-bar/dir-prj-face
                    'help-echo root)
        " "
        (propertize (shrink-replace
                     (str-displayable-fallback "○" "^") root)
                    'face 'i-bar/dir-prj-path-face
                    'help-echo
                    (purecopy (concat "Default directory:" default-directory
"\nmouse-1: cd buffer directory\n\
nouse-2: copy buffer file path\n\
mouse-3: cd other directory"))
                    'mouse-face 'mode-line-highlight
                    'local-map i-bar/dir-keymap)
        (propertize (shrink-replace "" path)
                    'face cface))))

(defun i-bar/dir-norm (cface)
  (propertize (shrink-replace
               (str-displayable-fallback "○" "^")
               dir)
              'face cface
              'help-echo
              (purecopy (concat "Default directory:" default-directory
"\nmouse-1: cd buffer directory\n\
nouse-2: copy buffer file path\n\
mouse-3: cd other directory"))
              'mouse-face 'mode-line-highlight
              'local-map i-bar/dir-keymap))

(defvar i-bar/dir
  '(:eval
    (list
     (let ((dir (shorten-default-directory 60))
           (cface (if (buffer-file-name)
                      (progn
                        (if (file-directory)
                            'i-bar/dir-untrue-face
                          'i-bar/dir-true-face))
                    nil)))
       (if (project-current)
           (i-bar/dir-prj cface)
         (i-bar/dir-norm cface)))))
  "Show directory in current buffer")

(defvar i-bar/bname
  '(:eval
    (list
     (let ((cface (if (file-modified-p)
                      'i-bar/bname-mo-face
                    'mode-line-buffer-id)))
     (propertize " %b "
                 'face cface
                 'help-echo
                 (purecopy "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer")
                 'mouse-face 'mode-line-highlight
                 'local-map mode-line-buffer-identification-keymap)))))

(defvar i-bar/ro
  '(:eval
    (list
     (if buffer-read-only
         (propertize
          (str-displayable-fallback "•" "R")
          'face 'i-bar/ro-face)
       " "))))

(defvar i-bar/mo
  '(:eval
    (list
     (if (file-modified-p)
         (propertize
          (str-displayable-fallback "◊" "*")
          'face 'i-bar/mo-face)
       " "))))

(defvar i-bar/size
  '(:eval
    (list
     (propertize
      (shrink-replace "" " %I")
      'face 'font-lock-comment-face))))

(defvar i-bar/ime
  '(:eval
    (list
     (propertize
      (if current-input-method-title
          current-input-method-title "")
      'face 'i-bar/ime-face
      'help-echo current-input-method
      'mouse-face 'mode-line-highlight
      'local-map mode-line-input-method-map))) )


(defvar i-bar/major-mode
  '(:eval
    (list
     (propertize (shorten-major-mode-name)
                 'face 'i-bar/major-mode-face
                 'help-echo
                  (concat "Major mode: " (symbol-name major-mode)
"\nmouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes")
                 'mouse-face 'mode-line-highlight
                 'local-map mode-line-major-mode-keymap)
      '("" mode-line-process))))

(defvar i-bar/minor-mode
    (list `(:propertize ("" minor-mode-alist)
                        face i-bar/minor-mode-face
                        mouse-face mode-line-highlight
                        help-echo "Minor-mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                        local-map ,mode-line-minor-mode-keymap)
          (propertize "%n" 'help-echo "mouse-1: Remove narrowing from buffer"
                      'mouse-face 'mode-line-highlight
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1 #'mode-line-widen))))

(defvar i-bar/pos
  '(:eval
    (list
     (propertize
      (concat
       (format " %.0f"
               (* 100
                  (/(* 1.0 (line-number-at-pos))
                    (+ 1 (count-lines (point-min) (point-max))))))
       "%% "))
     (if (region-active-p)
         (progn
           (run-with-timer 0.2 nil 'force-mode-line-update)
           (propertize
            (format " %d C %d L "
                    (- (region-end) (region-beginning))
                    (count-lines (region-beginning) (region-end)))
            'face 'region))
       " %4l:%2c "))))

(defvar i-bar/enc
  '(:eval
    (list
     " "
     (propertize
      ;; "%z"
      (let* ((enc (coding-system-plist buffer-file-coding-system))
             (enc-name (symbol-name (plist-get enc :name))))
        (if (not (string= enc-name "undecided")) (upcase enc-name) ""))
      'face nil
      'mouse-face 'mode-line-highlight
      'help-echo 'mode-line-mule-info-help-echo
      'local-map mode-line-coding-system-map)
     (propertize
      (mode-line-eol-desc)
      'face `(:height 80 :weight bold))
     )))

;; Default value of the top-level `mode-line-format' variable:
;; (list "%e"
;;   'mode-line-front-space
;;   'mode-line-mule-info
;;   'mode-line-client
;;   'mode-line-modified
;;   'mode-line-remote
;;   'mode-line-frame-identification
;;   'mode-line-buffer-identification
;;   "   "
;;   'mode-line-position
;;   'mode-line-modes
;;   '(vc-mode vc-mode)
;;   'mode-line-misc-info
;;   'mode-line-end-spaces)
(defvar i-bar-format
  (list "%e"
        'mode-line-front-space
        'i-bar/dir
        'i-bar/bname
        'i-bar/mo
        'i-bar/ro
        'i-bar/size
        'i-bar/pos
        'i-bar/major-mode
        'i-bar/minor-mode
        'i-bar/enc
        'vc-mode
        'flymake-mode-line-format
        'i-bar/ime
        'mode-line-misc-info
        'mode-line-end-spaces)
  "My i-bar mode-line")
;; mode line EOL format
(setq eol-mnemonic-dos " dos"
      eol-mnemonic-mac " mac"
      eol-mnemonic-unix " unix"
      eol-mnemonic-undecided " _")
;; put all symbol in i-bar-format to risky local variable
(mapc #'(lambda (x)
          (if (eq (type-of x) 'symbol)
              (put x 'risky-local-variable t)))
      (cdr i-bar-format))
(setq-default mode-line-format i-bar-format)

(with-eval-after-load "flymake"
  (setq flymake-mode-line-format
        '(:eval
          (when flymake-mode
            '(" [" flymake-mode-line-error-counter
              flymake-mode-line-warning-counter
              flymake-mode-line-note-counter "]")))))

(provide 'i-bar)

;;; i-bar.el ends here
