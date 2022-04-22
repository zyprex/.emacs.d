;;; company-translator.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Use company and web apis to translator text
;;
;;; Code:
(defun company-toggle-backends (arg)
  (interactive
   (list
    (ido-completing-read "company toggle backend:"
                         '("word-sug"
                           "iciba"
                           "youdao"
                           "bing-hover"))))
  (setq backend (intern (concat "company-" arg)))
  (if (member backend company-backends)
      (progn
        (setq company-backends (remove backend company-backends))
        (message (format "Remove: company-%s" arg)))
    (add-to-list 'company-backends backend)
    (message (format "Added: company-%s" arg))))

(defun bing-hover (word)
  (extract-content-from-html
   (fetch-html-string-from-url
    (concat "https://cn.bing.com/dict/SerpHoverTrans?q=" word))))

(defun company-bing-hover (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bing-hover))
    (prefix (company-grab-word))
    (candidates (list (bing-hover arg)))
    (meta (format "%s" arg))))

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

(defun convert-html-entities-to-char (str)
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

(defun extract-content-from-html (html-string)
  (string-trim
   (convert-html-entities-to-char
    (replace-regexp-in-string
     ;; "\\(</?[- a-zA-Z1-6\"=;_]+>\\)+"
     "\\(<[^<>]*>\\)+" "" html-string))))

(defun fetch-json-from-url (url)
  "return json as plist"
  (with-temp-buffer
    (url-insert-file-contents url)
    (json-parse-buffer :object-type 'plist)))

(defun fetch-html-string-from-url (url)
  "return html string"
  (with-temp-buffer
    (url-insert-file-contents url)
    ;; TODO: may delete by line?
    ;; (goto-char (point-min))
    ;; (re-search-forward "<\\(body\\|BODY\\).*>")
    ;; (delete-region (point) (point-min))
    ;; (goto-char (point-max))
    ;; (re-search-backward "</\\(body\\|BODY\\)>")
    ;; (delete-region (point) (point-max))
    (buffer-string)))

(provide 'company-translator)
