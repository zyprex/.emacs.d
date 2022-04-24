;;; webcut.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Function:
;;    1. Extract webpage as pure text and show in temp buffer
;;    2. Use company and web apis to give english suggestion
;;
;;; Code:
;;; TODO: https://www.etymonline.com/api/etymology/fuzzy?key=
(require 'company)
(require 'json)

;;;
;;; webcut
;;;
(defun get-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word)))

(defun get-html-string-from-url (url &optional frontline backline)
  "return html string, delete FRONTLINE ahead, trim BACKLINE backward"
  (with-temp-buffer
    (url-insert-file-contents url)
    (if (eq (type-of frontline) 'integer)
        (progn
          (goto-char (point-min))
          (re-search-forward "\n" nil t frontline)
          (delete-region (point-min) (point))))
    (if (eq (type-of backline) 'integer)
        (progn
          (goto-char (point-max))
          (re-search-backward "\n" nil t backline)
          (delete-region (point) (point-max))))
    (if (eq (type-of frontline) 'string)
        (progn
          (goto-char (point-min))
          (re-search-forward frontline nil t)
          (delete-region (point-min) (point))))
    (if (eq (type-of backline) 'string)
        (progn
          (goto-char (point-max))
          (re-search-backward backline nil t)
          (delete-region (point) (point-max))))
    ;; concat html string in one line remove \t space
    (mapconcat (lambda (x)
                 (string-replace  "\t" "" (string-trim x)))
               (split-string (buffer-string) "\n") "")))

(defun html-convert-newline (str)
  "Convert block element's newline to \n in string"
  (setq case-fold-search t)
  (replace-regexp-in-string
   (concat "<\\("
           (mapconcat
            (lambda (x) x)
            '("/h[1-6]" "/p" "/li" "/div" "br ?/?") "\\|")
           "\\)>")
   "
" str)
  (setq case-fold-search (default-value 'case-fold-search)))

(defun html-remove-inline-js (str)
  (replace-regexp-in-string "<script.*>.*</script>" "" str))

(defun html-remove-tag (str)
  (replace-regexp-in-string "\\(<[^<>]*>\\)+" "" str))

(defun html-convert-entities-to-char (str)
    (let ((retval str)
        (pair-list
         '(("&nbsp;" . " ")
           ("&amp;" . "&")
           ("&hellip;" . "…")
           ("&quot;" . "\"")
           ("&lt;" . "<")
           ("&gt;" . ">")
           ("&#[0-9]*;" .
            (lambda (match)
              (format "%c" (string-to-number (substring match 2 -1))))))))
    (dolist (elt pair-list retval)
      (setq retval (replace-regexp-in-string (car elt) (cdr elt) retval)))))

(defun temp-scratch-buffer (name content)
  (with-current-buffer (get-buffer-create name)
    (if buffer-read-only (read-only-mode 0))
    (erase-buffer)
    (insert content)
    (goto-char (point-min))
    (read-only-mode 1)
    (display-buffer name)
    (local-set-key (kbd "q") 'delete-window)))

(defmacro webcut--out (name url &optional arg1 arg2)
  `(temp-scratch-buffer
    ,(concat "*webcut-" name "*")
    (html-convert-entities-to-char
     (html-remove-tag
      (html-convert-newline
       (html-remove-inline-js
        (get-html-string-from-url ,url ,arg1 ,arg2)))))))

(defun webcut-wordnet ()
  (interactive)
  (webcut--out
   "wordnet"
   (string-replace "%s" (get-word) "http://wordnetweb.princeton.edu/perl/webwn?s=%s&sub=Search+WordNet&o2=&o0=1&o8=1&o1=1&o7=&o5=&o9=&o6=&o3=&o4=&h=")
   73))
;; http://wordnetweb.princeton.edu/perl/webwn?s=pear&sub=Search+WordNet&o2=&o0=1&o8=1&o1=1&o7=&o5=&o9=&o6=&o3=&o4=&h=

(defun webcut-ichacha ()
  (interactive)
  (webcut--out
   "ichacha"
   (string-replace "%s" (get-word) "http://www.ichacha.net/m/%s.html")
   330 278))
;; http://www.ichacha.net/m/reputable.html

(defun webcut-haici ()
  (interactive)
  (webcut--out
   "haici"
   (string-replace "%s" (get-word) "https://dict.cn/search?q=%s")
   "<div class=\"word\">"
   "<div id=\"dshared\">"))
;; https://dict.cn/search?q=honorable

(defun webcut-ahd ()
  (interactive)
  (webcut--out
   "AHD"
   (string-replace "%s" (get-word) "https://ahdictionary.com/word/search.html?q=%s")
   "<div class=\"results\">"
   "<span class=\"copyright\">"))
;; https://ahdictionary.com/word/search.html?q=latency


;;;
;;; webcut + company
;;;
(defun company-toggle-backends (arg)
  (interactive
   (list
    (ido-completing-read "company toggle backend:"
                         '("word-sug"
                           "iciba"
                           "youdao"
                           "bing-hover"
                           "etym"))))
  (setq backend (intern (concat "company-" arg)))
  (make-local-variable 'company-backends)
  (if (member backend company-backends)
      (progn
        (setq company-backends (remove backend company-backends))
        (message (format "Remove: company-%s" arg)))
    (add-to-list 'company-backends backend)
    (message (format "Added: company-%s" arg))))

(defun company-etym-find (word)
  (let ((str
         (with-temp-buffer
           (url-insert-file-contents
            (concat "https://www.etymonline.com/api/etymology/fuzzy?key=" word))
           (buffer-string))))
    (split-string (substring str 2 (1- (1- (length str)))) "\",\"")))

(defun company-etym (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-etym))
    (prefix (company-grab-word))
    (candidates (company-etym-find arg))
    (meta (bing-hover arg))))

(defun bing-hover (word)
  (string-replace
   word
   (concat word " ") ;; add space to seperate explain
   (html-convert-entities-to-char
    (html-remove-tag
     (with-temp-buffer
       (url-insert-file-contents
        (concat "https://cn.bing.com/dict/SerpHoverTrans?q=" (url-hexify-string word)))
       (buffer-string))))))

(defun company-bing-hover (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bing-hover))
    (prefix (company-grab-word))
    (candidates
     (let ((res (bing-hover arg)))
       (if (string= res "")
           (list arg)
         (list res))))
    (meta (format "%s" arg))))

(defun fetch-json-from-url (url)
  "return json as plist"
  (with-temp-buffer
    (url-insert-file-contents url)
    (json-parse-buffer :object-type 'plist)))
(defun company-iciba-find (word)
  (let* ((pl
          (fetch-json-from-url
           (concat "http://dict.iciba.com/dictionary/word/suggestion?word=" word)))
         (pl-arr (plist-get pl :message))
         (i 0) (sl '()))
    (while (< i (length pl-arr))
      (let* ((item (aref pl-arr i))
             (candidate (concat (plist-get item :key) " " (plist-get item :paraphrase))))
        (setq sl (append sl (list candidate))))
      (setq i (1+ i)))
    sl))

(defun company-iciba (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-iciba))
    (prefix (company-grab-word))
    (candidates (company-iciba-find arg))
    (meta (format "%s" arg))))

(defun company-youdao-find (word)
  (let* ((pl
          (fetch-json-from-url
           (concat "https://dict.youdao.com/suggest?doctype=json&num=30&q=" word)))
         (pl-arr (plist-get (plist-get pl :data) :entries))
         (i 0) (sl '()))
    (while (< i (length pl-arr))
      (let* ((item (aref pl-arr i))
             (candidate (concat (plist-get item :entry) " " (plist-get item :explain))))
        (setq sl (append sl (list candidate))))
      (setq i (1+ i)))
    sl))

(defun company-youdao (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-youdao))
    (prefix (company-grab-word))
    (candidates (company-youdao-find arg))
    (meta (format "%s" arg))))

(defun company-word-sug-find (word)
  (let ((pl
         (fetch-json-from-url
          (concat "https://api.datamuse.com/sug?s=" word)))
        (i 0) (sl '()))
    (while (< i (length pl))
      (setq sl (append sl (list (plist-get (aref pl i) :word))))
      (setq i (1+ i)))
    sl))

(defun company-word-sug (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-word-sug))
    (prefix (company-grab-word))
    (candidates (company-word-sug-find arg))
    (meta (bing-hover arg))))

(provide 'webcut)
