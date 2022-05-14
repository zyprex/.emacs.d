;;; i-lvf.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Live View Filter, it suppose to replacement the ioccur
;;
;;; Code:
(require 'imenu)
(require 'recentf)
(require 'thingatpt)

;;;
;;;
;;; lvf core
;;;
(defconst lvf-buffer-name "*lvf*" "The buffer name for lvf's temp buffer")

(defvar lvf-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-j") 'lvf-end)
    (define-key map (kbd "C-m") 'lvf-end)
    (define-key map (kbd "C-g") 'lvf-interrupt)
    (define-key map (kbd "<escape>") 'lvf-interrupt)
    (define-key map (kbd "C-n") 'lvf-next)
    (define-key map (kbd "C-p") 'lvf-prev)
    (define-key map (kbd "C-v") 'lvf-pgdn)
    (define-key map (kbd "M-v") 'lvf-pgup)
    (define-key map (kbd "C-w") 'lvf-insert-word)
    (define-key map (kbd "C-l") 'lvf-insert-line)
    (define-key map (kbd "M-<") 'lvf-top)
    (define-key map (kbd "M->") 'lvf-bot)
    map)
  "Keymap used by `lvf' in the minibuffer")

;; Status var of lvf
(defvar lvf-last-input nil "Latest input in lvf minibuffer")
(defvar lvf-run-timer nil "Monitor the user input")
(defvar lvf-result "" "Result line returned")
(defvar lvf-history nil "history input of lvf")

;; Those var may have to be set after each build source
(defvar lvf-leave-early nil "If t close lvf-buffer before exit minibuffer")
(defvar lvf-prefix-len 0 "Prefix length in result")
(defvar lvf-cache nil "Cache for lvf first build source list")

;; Those var will have different value when lvf run different type
(defvar lvf-build-source nil "lvf function to build source")
(defvar lvf-run-fn nil "lvf function run when run when user do input")
(defvar lvf-act-fn nil "lvf function run when user confirm input")

;; Buffer context var of lvf
(defvar lvf-buffer nil "lvf buffer object")
(defvar lvf-prev-buffer nil "lvf previous buffer object")

;; Render lvf-buffer
(defvar lvf-overlay nil "lvf highlight overlay")
(defface lvf-highlight-face
  '((t (:inherit 'match :underline t)))
  "Face used for highlight select line"
  :group 'lvf)

(defmacro with-lvf-buffer-window (&rest body)
  "Ensure to manipulating in lvf-buffer window"
  `(let ((w (get-buffer-window lvf-buffer)))
     (if (window-live-p w)
         (with-selected-window w ,@body))))

(defmacro with-lvf-prev-buffer-window (&rest body)
  "Ensure to manipulating in lvf-prev-buffer window"
  `(let ((w (get-buffer-window lvf-prev-buffer)))
     (if (window-live-p w)
         (with-selected-window w ,@body))))

(defmacro lvf-define-simple-motion (name &rest body)
  "Define simple motion for lvf buffer"
  `(defun ,(intern (concat "lvf-" name)) ()
     (interactive)
     (with-lvf-buffer-window
      ,@body
      (lvf-overlay-put))))

(lvf-define-simple-motion "next" (forward-line))
(lvf-define-simple-motion "prev" (forward-line -1))
(lvf-define-simple-motion "pgdn" (scroll-up))
(lvf-define-simple-motion "pgup" (scroll-down))
(lvf-define-simple-motion "top" (goto-char (point-min)))
(lvf-define-simple-motion "bot" (goto-char (point-max)))

(defun lvf-insert-word ()
  (interactive)
  (let ((word ""))
    (with-lvf-prev-buffer-window
     (setq word (thing-at-point 'word t)))
     (insert word)))

(defun lvf-insert-line ()
  (interactive)
  (let ((line ""))
    (with-lvf-prev-buffer-window
     (setq line (thing-at-point 'line t)))
     (insert line)))

(defun lvf-local-buffer-keymap ()
  (with-lvf-buffer-window
   (cancel-timer lvf-run-timer)
   (read-only-mode 1)
   (local-set-key (kbd "C-j")
                  (lambda ()
                    (interactive)
                    (lvf-buf-act-fn)
                    (forward-line)))
   (local-set-key (kbd "C-m") 'lvf-buf-act-fn)
   (local-set-key (kbd "<return>") 'lvf-buf-act-fn)))

(defun lvf-buf-act-fn()
  (interactive)
  (save-selected-window
    (setq lvf-result (lvf-get-cursor-line))
    (lvf-act-fn)))

(defun lvf-interrupt ()
  (interactive)
  (setq lvf-result "")
  (lvf-local-buffer-keymap)
  (abort-recursive-edit)
  (lvf-leave-minibuffer))

(defun lvf-end ()
  "The execute end of lvf"
  (interactive)
  (setq lvf-result (lvf-get-cursor-line))
  (lvf-local-buffer-keymap)
  (if lvf-leave-early
      (progn
        (lvf-leave-window)
        (lvf-leave-minibuffer)
        (lvf-act-fn))
    (lvf-act-fn)
    (lvf-leave-window)
    (lvf-leave-minibuffer))
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
   (if buffer-read-only
       (read-only-mode 0))
   (erase-buffer)
   (insert str)
   (goto-char (point-min))
   (lvf-overlay-put)))

(defun lvf-overlay-put ()
  (with-lvf-buffer-window
   (if lvf-overlay
       (delete-overlay lvf-overlay))
   (setq lvf-overlay (make-overlay
                      (+ lvf-prefix-len (line-beginning-position))
                      (line-end-position)))
   (overlay-put lvf-overlay 'face 'lvf-highlight-face)))

;;;###autoload
(defun lvf-run ()
  "If call this command directly, run lvf with last type."
  (interactive)
  (when (and (functionp 'lvf-build-source)
             (functionp 'lvf-run-fn)
             (functionp 'lvf-act-fn))
    (setq lvf-buffer (get-buffer-create lvf-buffer-name))
    (catch 'e
      (if (eq (current-buffer) lvf-buffer)
          (throw 'e (message "USE LVF IN LVF BUFFER")))
      (setq lvf-prev-buffer (current-buffer))
      (if (not (window-live-p (get-buffer-window lvf-buffer)))
          (switch-to-buffer-other-window lvf-buffer t)))
    ;; (if cursor-in-non-selected-windows
    ;;     (setq-local cursor-in-non-selected-windows t))
    (lvf-build-source)
    (setq lvf-run-timer (run-with-idle-timer 0.7 300 'lvf-run-fn))
    (add-to-history
     'lvf-history
     (read-from-minibuffer ">" nil lvf-minibuffer-map nil 'lvf-history))
    (lvf-end)))

(defmacro lvf-define-type (name)
  "Define command called `lvf-NAME`"
  `(defun ,(intern (format "lvf-%s" name)) ()
     ,(format "Use lvf query result, type (%s)." name)
     (interactive)
     (defalias 'lvf-build-source ',(intern (format "lvf-%s--build-source" name)))
     (defalias 'lvf-run-fn ',(intern (format "lvf-%s--run-fn" name)))
     (defalias 'lvf-act-fn ',(intern (format "lvf-%s--act-fn" name)))
     (lvf-run)))

(defmacro lvf-define-common-run-fn (str-list)
  "Define common lvf run function. Note the STR-LIST should alwauys be a string
list"
  `(let ((input (minibuffer-contents-no-properties)))
     (if (and (not (string= lvf-last-input input))
              (minibufferp (current-buffer)))
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
  "Filter elements on STR-LIST whose matched REGEX, and match from START. If no
  matches, return '(\"\")"
  (let* ((out-list
          (mapcar
           (lambda (x)
             (if (string-match-p regex x start) x nil))
           str-list))
         (out-list (delete nil out-list)))
    (if out-list out-list '(""))))

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

(defmacro str-list--add-prefix (prefix str-list)
  "Add PREFIX to each STR-LIST's element"
  `(if ,str-list
       (setq ,str-list
             (mapcar (lambda (x) (concat ,prefix x)) ,str-list))))



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
      (setq lvf-leave-early nil)
      (setq lvf-prefix-len (1+ (string-match "|" (car out-list))))
      (setq lvf-cache out-list))))

(defun lvf-line--run-fn ()
  (lvf-define-common-run-fn lvf-cache))

(defun lvf-line--act-fn ()
  (let ((line-num (string-to-number (car (split-string lvf-result "|")))))
    (with-lvf-prev-buffer-window
     (goto-char (point-min))
     (forward-line (1- line-num))
     (recenter))))

;;;###autoload
(lvf-define-type "line")


;;;
;;; lvf imenu
;;;
(defun setcar-prefix-str-fmt (s pad v)
  "Add prefix S to a cons's car V (the car's type must be the string),
PAD is padding space length"
  (cons (format (concat "%" (int-to-string pad) "s %s") s (car v)) (cdr v)))

(defun lvf-imenu--build-source ()
  (with-lvf-prev-buffer-window
   (let ((min-pad 1)
         (out-alist '())
         (imenu-alist (imenu--make-index-alist)))
     ;; find min padding length
     (dolist (v imenu-alist)
       (if (listp (cdr v))
           (setq min-pad (max (length (car v)) min-pad))))
     ;; append items with added prefix
     (dolist (v imenu-alist)
       ;; -- function
       (if (or (markerp (cdr v))
               (and (numberp (cdr v))
                    (not (= (cdr v) -99)))) ;; remove *Rescan*
           (setq out-alist
                 (append out-alist
                         (list (setcar-prefix-str-fmt "*" min-pad v)))))
       ;; -- other type
       (if (listp (cdr v))
           (dolist (i (cdr v))
             (setq out-alist
                   (append out-alist
                           (list (setcar-prefix-str-fmt (car v) min-pad i)))))))
     ;; (message "%s" out-alist)
     ;; (dolist (v out-alist) (message "%s" (car v)))
     (setq lvf-leave-early nil)
     (setq lvf-prefix-len (string-match "[^ ] " (car (car out-alist))))
     (setq lvf-cache out-alist))))

(defun lvf-imenu--run-fn ()
  (lvf-define-common-run-fn (mapcar (lambda (x) (car x)) lvf-cache)))

(defun lvf-imenu--act-fn ()
  (with-lvf-prev-buffer-window
   (let ((m (cdr (assoc lvf-result lvf-cache))))
     (cond
      ((markerp m)
       (switch-to-buffer (marker-buffer m))
       (goto-char (marker-position m)))
      ((numberp m) (goto-char m)))
     (recenter))))

;;;###autoload
(lvf-define-type "imenu")


;;;
;;; lvf recentf
;;;

(defun lvf-recentf--build-source ()
  (with-lvf-prev-buffer-window
   (let ((pbuf  (list (buffer-name (car (buffer-list)))))
         (buf-list '())
         (tbuf-list '())
         (f-list '())
         (d-list '()))
     (dolist (b (cdr (buffer-list)))
       (if (buffer-file-name b)
           (setq buf-list
                 (append buf-list (list (buffer-name b))))
         (if (not (string-match-p " \\*" (buffer-name b)))
             (setq tbuf-list
                   (append tbuf-list (list (buffer-name b)))))))
     (dolist (f recentf-list)
       (if (not (member (get-file-buffer f) (buffer-list)))
           (setq f-list
                 (append f-list (list (abbreviate-file-name f)))))
       (if (not (member (file-name-directory f) d-list))
           (setq d-list
                 (append d-list (list (abbreviate-file-name
                                       (file-name-directory f)))))))
     (dolist (f file-name-history)
       (if (and (file-exists-p f)
                (not (member (get-file-buffer f) (buffer-list)))
                (not (member f f-list)))
           (setq f-list
                 (append f-list (list f))))
       (if (and (file-exists-p (file-name-directory f))
                (not (member (file-name-directory f) d-list)))
           (setq d-list
                 (append d-list (list (file-name-directory f))))))
     (setq tbuf-list (delete lvf-buffer-name tbuf-list))
     (if (buffer-file-name (get-buffer (car pbuf)))
         (setq buf-list (append buf-list pbuf))
       (setq tbuf-list (append tbuf-list pbuf)))
     (str-list--add-prefix "B|" buf-list)
     (str-list--add-prefix "B|" tbuf-list)
     (str-list--add-prefix "F|" f-list)
     (str-list--add-prefix "D|" d-list)
     (setq lvf-cache (append buf-list f-list d-list tbuf-list))
     (setq lvf-leave-early nil)
     (setq lvf-prefix-len 2))))

(defun lvf-recentf--run-fn ()
  (lvf-define-common-run-fn lvf-cache))

(defun lvf-recentf--act-fn ()
  (let* ((type (substring lvf-result 0 1))
         (name (substring lvf-result 2)))
    (cond
     ((string= type "B") (pop-to-buffer name))
     ((string= type "F") (find-file-other-window name))
     ((string= type "D")
      (with-lvf-prev-buffer-window (cd name))))))

;;;###autoload
(lvf-define-type "recentf")


;;;
;;; lvf rg
;;;
(defun lvf-rg--build-source ()
  (setq lvf-leave-early nil)
  (setq lvf-prefix-len 0)
  (setq lvf-cache ""))

(defun lvf-rg--run-fn ()
  (when (minibufferp (current-buffer))
    (let ((input (minibuffer-contents-no-properties)))
      (if (not (string= lvf-last-input input))
          (progn
            (lvf-display
             (shell-command-to-string (concat "rg --vimgrep " input)))
            (setq lvf-last-input input))))))

(defun lvf-rg--act-fn ()
  (let* ((result-list (split-string lvf-result ":"))
         (f (nth 0 result-list))
         (l (1- (string-to-number (nth 1 result-list))))
         (c (1- (string-to-number (nth 2 result-list)))))
    (find-file-other-window f)
    (goto-char (point-min))
    (forward-line l)
    (move-to-column c)
    (recenter)))

;;;###autoload
(lvf-define-type "rg")


;;;
;;; lvf fd
;;;
(defun lvf-fd--build-source ()
  (setq lvf-leave-early nil)
  (setq lvf-prefix-len 0)
  (setq lvf-cache ""))

(defun lvf-fd--run-fn ()
  (when (minibufferp (current-buffer))
    (let ((input (minibuffer-contents-no-properties)))
      (if (not (string= lvf-last-input input))
          (progn
            (lvf-display
             (shell-command-to-string (concat "fd -t f " input " .")))
            (setq lvf-last-input input))))))

(defun lvf-fd--act-fn ()
  (find-file-other-window lvf-result))

;;;###autoload
(lvf-define-type "fd")


;;;
;;; lvf loadhist
;;;

;; command-history
;; h_L lvf-history
;; r_K kill-ring
;; r_S search-ring
;; M-x extended-command-history <- remove duplicate ?
;; M-: read-expression-history
;; M-! shell-command-history
;; EWW eww-prompt-history

(defun str-to-line-list (list-var)
  "Convert string list to one line string list, substitute any \\n to ' '"
  (mapcar (lambda (x) (string-replace "\n" " " (substring-no-properties x)))
          list-var))

(defun lvf-loadhist--build-source ()
  (let* ((lvf-input-hist lvf-history)
         (kill-ring-hist (str-to-line-list kill-ring))
         (search-ring-hist (str-to-line-list search-ring))
         (extended-command-hist extended-command-history)
         (read-expression-hist read-expression-history)
         (shell-command-hist shell-command-history)
         (eww-prompt-hist eww-prompt-history))
    (str-list--add-prefix "h_L " lvf-input-hist)
    (str-list--add-prefix "r_K " kill-ring-hist)
    (str-list--add-prefix "r_S " search-ring-hist)
    (str-list--add-prefix "EWW " eww-prompt-hist)
    (str-list--add-prefix "M-x " extended-command-hist)
    (str-list--add-prefix "M-: " read-expression-hist)
    (str-list--add-prefix "M-! " shell-command-hist)
    (setq lvf-cache (append
                     shell-command-hist
                     read-expression-hist
                     extended-command-hist
                     eww-prompt-hist
                     search-ring-hist
                     kill-ring-hist
                     lvf-input-hist))
    (setq lvf-leave-early t)
    (setq lvf-prefix-len 4)))

(defun lvf-loadhist--run-fn ()
  (lvf-define-common-run-fn lvf-cache))

(defun lvf-loadhist--act-fn ()
  (let* ((type (substring lvf-result 0 (1- lvf-prefix-len)))
         (val (substring lvf-result lvf-prefix-len))
         (val (string-replace " " "\n" val)))
    (cond
     ((string= "h_L" type) (kill-new val))
     ((string= "r_K" type) (kill-new val))
     ((string= "r_S" type) (kill-new val))
     ((string= "M-x" type)
      (let ((cmd (intern val)))
        (if (commandp cmd)
             (command-execute cmd))))
     ((string= "M-:" type)
      (eval-expression (read--expression "Eval: " val)))
     ((string= "M-!" type) (shell-command val))
     ((string= "EWW" type) (eww val))
     (t nil))))

;;;###autoload
(lvf-define-type "loadhist")

(provide 'i-lvf)
;;; i-lvf.el ends here
