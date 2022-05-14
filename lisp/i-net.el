;;; i-net.el -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;    Functions:
;;    1. Extract webpage as pure text and show in temp buffer
;;    2. Use company and web apis to give english suggestion
;;
;;; Code:
(require 'company)
(require 'json)
(require 'button)

;;;
;;; webcut
;;;

;; (defun parse-html-dom-from-string (str)
;;   (with-temp-buffer
;;     (inset str)
;;     (libxml-parse-html-region (point-min) (point-max))))

(defun get-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun prepare-query-url (url)
  (string-replace "%s" (get-word) url))

(defun get-html-string-from-url (url &optional frontline backline subst-pair)
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
    (if (eq (type-of subst-pair) 'cons)
        (let ((bstr (buffer-string)))
          (erase-buffer)
          (insert (subst-each bstr subst-pair))))
    ;; concat html string in one line remove \t space
    (mapconcat (lambda (x)
                 (string-replace  "\t" "" (string-trim x)))
               (split-string (buffer-string) "\n") "")))

(defun html-convert-newline (str)
  "Convert block element's newline to \\n in string"
  (let ((case-fold-search t))
    (replace-regexp-in-string
     (concat "<\\("
             (mapconcat
              (lambda (x) x)
              '("/h[1-6]" "/p" "/li" "/div" "/d[tdl]" "br[ ]*/?") "\\|")
             "\\)>")
     "
" str)))
;; (setq case-fold-search (default-value 'case-fold-search)))

(defun html-remove-inline-js (str)
  (replace-regexp-in-string "<script.*>.*</script>" "" str))

(defun html-remove-tag (str)
  (replace-regexp-in-string "\\(<[^<>]*>\\)+" "" str))

(defun html-convert-entities-to-char (str)
  (let ((retval str)
        (pair-list
         '(("&nbsp;" . " ")  ("&amp;" . "&")
           ("&lt;" . "<")    ("&gt;" . ">")
           ("&bull;" . "•")  ("&middot;" . "·")
           ("&quot;" . "\"") ("&apos;" . "'")
           ("&lsquo;" . "‘") ("&rsquo;" . "’")
           ("&ldquo;" . "“") ("&rdquo;" . "”")
           ("&laquo;" . "«") ("&raquo;" . "»")
           ("&mdash;" . "—") ( "&ndash;" . "–")
           ("&hellip;" . "…")
           ("&#[0-9]*;" .
            (lambda (match)
              (format "%c" (string-to-number (substring match 2 -1))))))))
    (subst-each str pair-list)))

(defun subst-each (str pair-list)
  "Replace each pair's car to cdr in STR"
  (let ((retval str))
    (dolist (elt pair-list retval)
      (setq retval (replace-regexp-in-string (car elt) (cdr elt) retval)))))

(defun temp-scratch-buffer (name content)
  (with-current-buffer (get-buffer-create name)
    (if buffer-read-only (read-only-mode 0))
    (erase-buffer)
    (insert content)
    (goto-char (point-min))
    (read-only-mode 1)
    (display-buffer name)))
;; (local-set-key (kbd "q") 'delete-window)))

(defmacro webcut--out (name url &optional arg1 arg2 subst-pair)
  `(temp-scratch-buffer
    ,(concat "*webcut-" name "*")
    (html-convert-entities-to-char
     (html-remove-tag
      (html-convert-newline
       (html-remove-inline-js
        (get-html-string-from-url ,url ,arg1 ,arg2 ,subst-pair)))))))

(defmacro webcut-define--out (name url &optional arg1 arg2 subst-pair)
  `(defun ,(intern (concat "webcut-" name)) ()
     (interactive)
     (let ((url (prepare-query-url ,url))
           (bufname (concat "*webcut-" ,name "*")))
       (webcut--out
        ,name url ,arg1 ,arg2 ,subst-pair)
       (webcut-insert-origin-link bufname url))))

(defun webcut-insert-origin-link (bufname link)
  (with-selected-window (get-buffer-window bufname)
    (read-only-mode  0)
    (goto-char (point-max))
    (insert "\n")
    (insert-button
     link
     'action (lambda (_) (eww link))
     'follow-link t
     'face 'link)
    (read-only-mode 1)
    (goto-char (point-min))))

;;;###autoload
(webcut-define--out
 "wordnet"
 "http://wordnetweb.princeton.edu/perl/webwn?s=%s&sub=Search+WordNet&o2=&o0=1&o8=1&o1=1&o7=&o5=&o9=&o6=&o3=&o4=&h="
 73)
;; http://wordnetweb.princeton.edu/perl/webwn?s=pear&sub=Search+WordNet&o2=&o0=1&o8=1&o1=1&o7=&o5=&o9=&o6=&o3=&o4=&h=

;;;###autoload
(webcut-define--out "ichacha" "http://www.ichacha.net/m/%s.html" 332 266)
;; http://www.ichacha.net/m/reputable.html

;;;###autoload
(webcut-define--out
 "haici"
 "https://dict.cn/search?q=%s"
 "<div class=\"word\">"
 "<div id=\"dshared\">")
;; https://dict.cn/search?q=honorable

;;;###autoload
(webcut-define--out
 "ahd"
 "https://ahdictionary.com/word/search.html?q=%s"
 "<div class=\"results\">"
 "<span class=\"copyright\">")
;; https://ahdictionary.com/word/search.html?q=latency

;;;###autoload
(webcut-define--out
 "quword"
 "https://www.quword.com/w/%s"
 "<div class=\"col-sm-8\" id=\"yd-content\">"
 "<div class=\"col-sm-4 hidden-xs yd-sidebars\">"
 '(("<span class=\"glyphicon glyphicon-star\"></span>" . "★")))
;; https://www.quword.com/w/perspective

(defmacro plist-get-string (p-list attr)
  `(let ((ret (plist-get ,p-list ,attr)))
     (if ret ret "")))

;;;###autoload
(defun webcut-moedict ()
  (interactive)
  (let* ((url (prepare-query-url "https://www.moedict.tw/a/%s.json"))
         (pl (fetch-json-from-url url))
         (title-str (plist-get pl :t))
         (def (aref (plist-get pl :h) 0))
         (pinyin-str (plist-get def :p))
         (explain-list (mapcar (lambda (x)
                                 (format "\n<%s> %s\n%s\n"
                                         (plist-get-string x :type)
                                         (plist-get-string x :f)
                                         (plist-get-string x :e)))
                               (plist-get def :d)))
         (explain-str (mapconcat (lambda (x) x) explain-list ""))
         (translation (plist-get pl :translation))
         (translation-str (format "\n英: %S\n德: %S\n法: %S\n"
                                  (plist-get-string translation :English)
                                  (plist-get-string translation :Deutsch)
                                  (plist-get-string translation :francais)))
         (str-out (concat title-str "(" pinyin-str ")" explain-str
                          translation-str)))
    (temp-scratch-buffer
     "*webcut-moedict*"
     (subst-each str-out '(("~`" . "") ("`" . "") ("~" . ""))))
    (webcut-insert-origin-link "*webcut-moedict*" url)))
;; https://www.moedict.tw/a/%E8%90%8C.json


;;;
;;; company word suggestion
;;;

;;;###autoload
(defun company-toggle-backends (arg)
  (interactive
   (list
    (completing-read "company toggle backend:"
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
  "Return json as plist"
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

(provide 'i-net)
