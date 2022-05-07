;;; i-jmp.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    jump to a char
;;
;;; Code:

(defun alpha-p (char-num)
  "Check ascii char is a-z or A-Z"
  (or (and (> char-num 96) (< char-num 123))
      (and (> char-num 64) (< char-num 91))))

(defun non-blank-char-p (char-num)
  (and (not (member char-num '(9 32 10))) (< char-num 256)))

(defun rabbit-jump (ch table direction)
  "Jump to char CH recorded in TABLE, when DIRECTION < 0 enable reverse find"
  (let ((idx 0) (c 0) (p 0) (brk nil) (tb-len (length table)))
    (while (and (not brk) (< idx tb-len))
      (if (> direction -1)
          (progn
            (setq c (aref table idx))
            (setq p (aref table (1+ idx))))
        (setq c (aref table (- (1- tb-len) idx 1)))
        (setq p (aref table (1+ (- (1- tb-len) idx 1)))))
      (if (= c ch)
          (progn
            (goto-char p)
            (setq brk t)))
      (setq idx (+ idx 2)))))

(defun rabbit-jump-build-table (start-pos end-pos)
  "Build a jump list vector range from START-POS to END-POS,
the data means [char-code1 char-code1-position char-code2 char-code2-position
...]"
  (let* ((start start-pos)
         (end end-pos)
         (tb-len (* 2 (count-screen-lines start end)))
         (tb (make-vector tb-len 0))
         (ts (make-vector tb-len 0))
         (cur (char-after start))
         (idx 0)
         (b-end 1)
         (s-end 1))
    (while (< start end)
      ;; collect first alphabet char
      (when (and (alpha-p cur) (= b-end 1))
        (aset tb idx cur)
        (aset tb (1+ idx) start)
        (setq b-end 0))
      ;; collect first non-blank char
      (when (and (non-blank-char-p cur) (= s-end 1))
        (aset ts idx cur)
        (aset ts (1+ idx) start)
        (setq s-end 0))
      ;; over line end
      (when (= cur 10)
        (setq idx (+ idx 2))
        (setq b-end 1)
        (setq s-end 1))
      (setq start (1+ start))
      (setq cur (char-after start)))
    ;; merge tb and ts
    (dolist (n (number-sequence 0 (1- tb-len) 2))
      (let ((tb-ch (aref tb n))
            (ts-ch (aref ts n)))
        (when (and (= tb-ch 0) (not (= ts-ch 0)))
          (aset tb n ts-ch)
          (aset tb (1+ n) (aref ts (1+ n))))))
    tb))

(defvar rabbit-jump-char 0 "local var for rabbit-jump")

(defun rabbit-jump-read-char (hint-string)
  (make-local-variable 'rabbit-jump-char)
  (setq-local rabbit-jump-char
              (read-char
               (propertize
                (concat "rabbit-jump _ " hint-string) 'face 'minibuffer-prompt))))

;;;###autoload
(defun rabbit-jump-forward ()
  (interactive)
  (rabbit-jump (rabbit-jump-read-char "forward")
            (rabbit-jump-build-table (line-end-position) (window-end)) 1)
  (rabbit-jump--transient-map 'rabbit-jump-forward-repeat))

;;;###autoload
(defun rabbit-jump-backward ()
  (interactive)
  (rabbit-jump (rabbit-jump-read-char "backward")
            (rabbit-jump-build-table (window-start) (line-beginning-position)) -1)
  (rabbit-jump--transient-map 'rabbit-jump-backward-repeat))

(defun rabbit-jump-forward-repeat ()
  (interactive)
  (rabbit-jump rabbit-jump-char
            (rabbit-jump-build-table (line-end-position) (window-end)) 1)
  (rabbit-jump--transient-map 'rabbit-jump-forward-repeat))

(defun rabbit-jump-backward-repeat ()
  (interactive)
  (rabbit-jump rabbit-jump-char
            (rabbit-jump-build-table (window-start) (line-beginning-position)) -1)
  (rabbit-jump--transient-map 'rabbit-jump-backward-repeat))

(defmacro rabbit-jump--transient-map (fn)
  `(set-transient-map
    (let ((kmap (make-sparse-keymap)))
      (define-key kmap (kbd (char-to-string rabbit-jump-char)) ,fn)
      kmap)))

;;;###autoload
(defun rabbit-jump-top ()
  (interactive)
  (rabbit-jump (rabbit-jump-read-char "top")
            (rabbit-jump-build-table (window-start) (window-end)) 1)
  (rabbit-jump--transient-map 'rabbit-jump-forward-repeat))

;;;###autoload
(defun rabbit-jump-bot ()
  (interactive)
  (rabbit-jump (rabbit-jump-read-char "bottom")
            (rabbit-jump-build-table (window-start) (window-end)) -1)
  (rabbit-jump--transient-map 'rabbit-jump-backward-repeat))

(defmacro zoom-to-char--transient-map ()
  `(set-transient-map
    (let ((kmap (make-sparse-keymap)))
      (define-key kmap (kbd (char-to-string zoom-current-char))
        (lambda ()
          (interactive)
          (zoom-to-char-next zoom-current-char)))
      kmap)))

;;;###autoload
(defun zoom-to-char ()
  (interactive)
  (make-local-variable 'zoom-current-char)
  (setq-local zoom-current-char (read-char))
  (zoom-to-char-next zoom-current-char)
  (zoom-to-char--transient-map))

(defun zoom-to-char-next (char)
  (let ((ch char)
        (cur (1+ (point))))
    (while (and (not (eq ch (char-after cur)))
                (< cur (window-end)))
      (setq cur (1+ cur)))
    (if (eq ch (char-after cur))
        (goto-char cur)))
  (zoom-to-char--transient-map))



(provide 'i-jmp)
