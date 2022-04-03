;;; i-abb.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    A Basic Boilerplate elisp implement
;;; Code:
(defvar i-abb/last-end-point nil "Last success end of abb expand")
(defvar i-abb/table '() "Contain many code boilerplate.
The item structure is (([mode1] [mode2] ...) . ([id] [pre] [post]))")
(setq i-abb/table
      '(
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("fu" "(defun " "()\n)"))
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("if" "(if " "\n(progn\n))"))
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("m" "(message " ")"))
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("s" "(setq " ")"))
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("k" "(kbd \"" "\")"))
        ((lisp-interaction-mode emacs-lisp-mode inferior-emacs-lisp-mode)
         . ("i" "(interactive)\n"))
        ((org-mode) . ("link" "[[" "][description]]"))
        ((org-mode) . ("img" "#+caption: " "\n[[image/path]]"))
        ((org-mode) . ("co" "#+BEGIN_SRC: " "\n#+END_SRC"))
        ((org-mode) . ("ex" "#+BEGIN_EXAMPLE\n" "\n#+END_EXAMPLE"))
        ((org-mode) . ("meta" "#+TITLE: " "\n#+AUTHOR:\n#+DATE:"))
        ((markdown-mode) . ("link" "[" "]()"))
        ((markdown-mode) . ("img" "![alt_text](" ")"))
        ((markdown-mode) . ("tb" "|" "    |    |\n|:---|:---|\n|    |    |"))
        ((c-mode cc-mode) . ("pr" "printf(\"" "\");"))
        ((c-mode cc-mode) . ("ca" "case " ":\nbreak;"))
        ((c-mode cc-mode) . ("de" "#define " ""))
        ((c-mode cc-mode) . ("ty" "typedef " ""))
        ((c-mode cc-mode) . ("in" "#include <" ">"))
        ((c-mode cc-mode) . ("inc" "#include \"" "\""))
        ((c-mode cc-mode) . ("ma" "int main(int argc, char* argv[]) {\n" "\nreturn 0;\n}"))
        ((c-mode cc-mode) . ("fu" "" "() {\n}"))
        ((c-mode cc-mode) . ("so" "sizeof(" ")"))
        ((c-mode cc-mode js-mode web-mode) . ("do" "do {\n" "\n} while(0);"))
        ((c-mode cc-mode js-mode web-mode) . ("wh" "while (" ") {\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("fo" "for (" ") {\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("if" "if (" ") {\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("ef" "else if (" ") {\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("el" "else {\n" "\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("sw" "switch (" ") {\n}"))
        ((c-mode cc-mode js-mode web-mode) . ("ca" "case " ":\nbreak;"))
        ((c-mode cc-mode js-mode web-mode) . ("?" "(" ") ? : ;"))
        ((c-mode cc-mode js-mode web-mode) . ("re" "return" ";"))
        ((js-mode web-mode) . ("co" "console.log(" ");"))
        ((js-mode web-mode) . ("fu" "function " "() {\n}"))
        ((js-mode web-mode) . ("af" "(" ") => {\n}"))
        ))

(defun i-abb/get-body (str)
  (catch 'e
    (dolist (tb i-abb/table)
      (let* ((mode (car tb))
             (body (cdr tb))
             (id (car body)))
        (if (and (member major-mode mode)
                 (string-equal str id))
            (throw 'e (cdr body)))))))

(defun i-abb/insert-date (prefix)
  ;; https://www.emacswiki.org/emacs/InsertDate
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format
         (cond
          ((not prefix) "%c")
          ((eq prefix 1) "%Y-%m-%d")
          ((eq prefix 2) "%A, %d. %B %Y"))))
    (insert (format-time-string format))))

;;;###autoload
(defun i-abb/expand ()
  (interactive)
  (let* ((cword (thing-at-point 'word))
         (bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds))
         (s (i-abb/get-body cword)))
    (when s
      (kill-region beg end)
      (insert (nth 0 s) (nth 1 s))
      (setq i-abb/last-end-point (point))
      (goto-char (+ beg (length (nth 0 s))))
      (indent-region beg i-abb/last-end-point))
    ))


;; (defun i-abb/insert-date ()
;;   (interactive)
;;   (insert (format-time-string "%Y-%m-%d %a")))

(defun i-abb/map-ctrl-e ()
  "Use Ctrl+E expand abb table"
  (interactive)
  (if (i-abb/get-body (thing-at-point 'word))
      (i-abb/expand)
    (move-end-of-line 1)))

(global-set-key (kbd "C-e") 'i-abb/map-ctrl-e)
(with-eval-after-load "evil"
  (evil-define-key 'insert 'global (kbd "C-e") 'i-abb/map-ctrl-e))

(provide 'i-abb)

;;; i-abb.el ends here
