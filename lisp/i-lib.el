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
  (let ((pkg))
  (catch 'e
    (dolist (pkg package-list)
      (when (not (require 'pkg nil 'noerror))
        (throw 'e (package-get)))))))

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

;; (defun ido-recentf-open ()
;;   ;; because of `ido-use-virtual-buffers', this function becomes useless
;;   "Use `ido-completing-read' to find a recent file"
;;   (interactive)
;;   (when recentf-list
;;     (find-file-existing (ido-completing-read "Find recentf: " recentf-list))))

(defun read-n-char (n)
  "Read n chars"
  (let ((str "") (ch "") (i 0))
    (while (< i n)
      (setq ch (read-char
                (concat
                 (propertize (format "Read char(%d/%d): " i n)
                             'face '(minibuffer-prompt default))
                 str)))
      (setq str (concat str (char-to-string ch)))
      (setq i (1+ i)))
      str))

(defun search-n-char-forward (arg)
  "Search n chars forward"
  (interactive "P")
  (let ((search-str
         (read-n-char
          (if current-prefix-arg
              (if (integer-or-marker-p current-prefix-arg)
                  current-prefix-arg
                (car current-prefix-arg))
            1))))
    (setq-local search-n-string search-str)
    (search-forward search-str)))

(defun search-n-char-backward (arg)
  "Search n chars backward"
  (interactive "P")
  (let ((search-str
         (read-n-char
          (if current-prefix-arg
              (if (integer-or-marker-p current-prefix-arg)
                  current-prefix-arg
                (car current-prefix-arg))
            1))))
    (setq-local search-n-string search-str)
    (search-backward search-str)))

(defun search-n-char-forward-repeat ()
  "Repeat last `search-n-char-forward'"
  (interactive)
  (search-forward search-n-string))

(defun search-n-char-backward-repeat ()
  "Repeat last `search-n-char-backward'"
  (interactive)
  (search-backward search-n-string))

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

(provide 'i-lib)

;; https://www.emacswiki.org/emacs/IncrementNumber
(defun change-number-at-point (change increment)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number increment)))
        (goto-char point)))))
(defun increment-number-at-point (&optional increment)
  "Increment number at point like vim's C-a"
  (interactive "p")
  (change-number-at-point '+ (or increment 1)))
(defun decrement-number-at-point (&optional increment)
  "Decrement number at point like vim's C-x"
  (interactive "p")
  (change-number-at-point '- (or increment 1)))

;; duplicate line
(defun duplicate-line (&optional n)
  "Duplicate Current Line, make more than 1 copy if given a numeric argument"
  (interactive "p")
  (message "%s" n)
  (move-beginning-of-line 1)
  (dotimes (i (abs (or n 1)))
    (insert (thing-at-point 'line))))

(defun middle-column (&optional arg)
  "Go to middle of column"
  (interactive "P")
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle))
  (forward-char arg))

(defun isearch-window-forward ()
  ;; Origin from https://stackoverflow.com/questions/11569635/isearch-occur-visible-area-in-emacs/11569806#11569806
  "Interactive search, limited to the visible portion of the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region (window-start) (window-end))
    (isearch-forward)))

(defun isearch-window-backward ()
  (interactive)
  (save-restriction
    (narrow-to-region (window-start) (window-end))
    (isearch-backward)))

(defun rabbit-jump-forward ()
  (interactive)
  (setq-local rabbit-jump-char (rabbit-jump--read-char "forward"))
  (rabbit-jump rabbit-jump-char (point) (window-end))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-forward))

(defun rabbit-jump-backward ()
  (interactive)
  (setq-local rabbit-jump-char (rabbit-jump--read-char "backward"))
  (rabbit-jump rabbit-jump-char (point) (window-start))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-backward))

(defun rabbit-jump-repeat-forward ()
  (interactive)
  (rabbit-jump rabbit-jump-char (point) (window-end))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-forward))

(defun rabbit-jump-repeat-backward ()
  (interactive)
  (rabbit-jump rabbit-jump-char (point) (window-start))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-backward))

(defun rabbit-jump-top ()
  (interactive)
  (setq-local rabbit-jump-char (rabbit-jump--read-char "top"))
  (rabbit-jump rabbit-jump-char (window-start) (window-end))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-forward))

(defun rabbit-jump-bot ()
  (interactive)
  (setq-local rabbit-jump-char (rabbit-jump--read-char "bottom"))
  (rabbit-jump rabbit-jump-char (window-end) (window-start))
  (rabbit-jump--transient-map 'rabbit-jump-repeat-backward))

(defmacro rabbit-jump--transient-map (fn)
  `(set-transient-map
   (let ((kmap (make-sparse-keymap)))
     (define-key kmap (kbd (char-to-string rabbit-jump-char)) ,fn)
     kmap)))

(defun alphabet-p (char-num)
  "check ascii char is a-z or A-Z"
  (or (and (> char-num 96) (< char-num 123))
      (and (> char-num 64) (< char-num 91))))

(defun rabbit-jump--read-char (prompt-str)
  (read-char
   (concat (propertize (format "rabbit jump %s" prompt-str)
                       'face '(minibuffer-prompt default)))))

(defun rabbit-jump (ch start-position end-position)
  "jump to line, in first CHAR between two position"
  (let ((start start-position)
        (end end-position)
        (table (make-vector
                (* 2 (count-screen-lines (window-start) (window-end))) 0))
        (idx 0)
        (cur 0)
        (lend 1))
    (if (> start end)
        (setq start end-position end start-position rev 1)
      (setq rev nil))
    (setq end (1- end))
    (while (and (< start end) (char-after start))
      (progn
        (setq cur (char-after start))
        ;; a char in alphabet, not in current line had over line end
        (if (and (alphabet-p cur) (not (= start (point))) (= lend 1))
            (progn
              (aset table idx cur)
              (aset table (1+ idx) start)
              (setq lend 0)
              (setq idx (+ 2 idx))))
        ;; over line end
        (if (= cur 10) (setq lend 1))
        (setq start (1+ start))))
    (dotimes (v (/ (length table) 2))
      ;; c - current char code, i - the char's position
      (let ((c 0) (i 0))
        (if (eq rev 1)
            (setq c (aref table (* 2 v))
                  i (1+ (* 2 v)))
          (setq c (aref table (- (length table) (* 2 v) 2))
                i (1+ (- (length table) (* 2 v) 2))))
        (if (eq c ch)
            (goto-char (aref table i)))
        ))
    ))

;;; i-lib.el ends here
