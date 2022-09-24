;;; ts-docstr-key.el --- Support keys to add document string  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Support key to add document string.
;;
;; This module help user add document string in a much nicer/nature way.
;;

;;; Code:

(require 'ts-docstr)

;;
;; (@* "Customization" )
;;

(defcustom ts-docstr-key-alist
  `(("RET" . ts-docstr-key-doxygen-like-return)
    ("/"   . ts-docstr-key-csharp)
    ("/"   . ts-docstr-key-go)
    ("\""  . ts-docstr-key-python)
    ("#"   . ts-docstr-key-ruby)
    ("/"   . ts-docstr-key-rust)
    ("/"   . ts-docstr-key-swift))
  "List of trigger to each `major-mode'."
  :type 'hook
  :group 'ts-docstr)

(defcustom ts-docstr-key-support nil
  "If non-nil, use key support to fulfill document triggerations' conditions."
  :type 'boolean
  :group 'ts-docstr)

;;
;; (@* "Util" )
;;

(defun ts-docstr--looking-back (regexp &optional limit greedy)
  "Wrapper for function `looking-back'.
See function `looking-back' description for arguments REGEXP, LIMIT,
and GREEDY."
  (ignore-errors (looking-back regexp limit greedy)))

(defun ts-docstr--line-is (string)
  "Check if STRING is equal to current line (trimmed)."
  (string= (string-trim (thing-at-point 'line)) string))

(defun ts-docstr--line-relative (&optional n trim)
  "Return string of N line relatively.

If optional argument TRIM is non-nil; then trim the return string.

See function `forward-line' for argument N."
  (save-excursion
    (when n (forward-line n))
    (if trim (string-trim (thing-at-point 'line)) (thing-at-point 'line))))

(defun ts-docstr--current-line-empty-p ()
  "Current line empty, but accept spaces/tabs in there."
  (save-excursion (beginning-of-line) (looking-at "[[:space:]\t]*$")))

(defun ts-docstr--last-char-in-line-p (&optional pt)
  "Return non-nil if there is nothing behind of the right from the PT."
  (save-excursion
    (when pt (goto-char pt))
    (null (re-search-forward "[^ \t]" (line-end-position) t))))

(defun ts-docstr--comment-p (&optional pos)
  "Return non-nil when inside comment block."
  (save-excursion (goto-char (or pos (point))) (nth 4 (syntax-ppss))))

(defun ts-docstr--goto-start-comment ()
  "Go to the start of the comment."
  (while (ts-docstr--comment-p)
    (re-search-backward comment-start-skip nil t)))

(defun ts-docstr--goto-end-comment ()
  "Go to the end of the comment."
  (when (ts-docstr--comment-p)
    (forward-char 1)
    (ts-docstr--goto-end-comment)))

(defun ts-docstr--comment-start-point ()
  "Return comment start point."
  (save-excursion (ts-docstr--goto-start-comment) (point)))

(defun ts-docstr--comment-end-point ()
  "Return comment end point."
  (save-excursion (ts-docstr--goto-end-comment) (point)))

(defun ts-docstr--kill-comment-at-point ()
  "Kill comment at point."
  (interactive)
  (delete-region (ts-docstr--comment-start-point) (ts-docstr--comment-end-point)))

(defun ts-docstr--start-comment-symbol (&optional pt)
  "Return the starting comment symbol form the given PT."
  (when (ts-docstr--comment-p)
    (let (start-pt)
      (save-excursion
        (when pt (goto-char pt))
        (ts-docstr--goto-start-comment)
        (progn  ; Make sure to go outside of symbol
          (re-search-backward "[ \t\r\n]" nil t)
          (when (= (point) (line-end-position)) (forward-char 1)))
        (setq start-pt (point))
        (re-search-forward comment-start-skip (1+ (line-end-position)) t)
        (if (= start-pt (point)) nil
          (string-trim (buffer-substring start-pt (point))))))))

(defun ts-docstr--comment-line-symbol (&optional n)
  "Forward N line and return starting comment symbol."
  (save-excursion
    (when n (forward-line n)) (end-of-line)
    (ts-docstr--start-comment-symbol)))

(defun ts-docstr--multiline-comment-p ()
  "Return non-nil, if current point inside multi-line comment block."
  (ignore-errors (string-match-p "/[*]" (ts-docstr--start-comment-symbol))))

(defun ts-docstr-key--advice-add (key where fnc)
  "Safe add advice KEY with FNC at WHERE."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-add key-fnc where fnc))))

(defun ts-docstr-key--advice-remove (key fnc)
  "Safe remove advice KEY with FNC."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-remove key-fnc fnc))))

(defmacro ts-docstr-key--with-major-modes (mode-list &rest body)
  "Execute BODY only when major-mode is defined in MODE-LIST."
  (declare (indent 1))
  `(when (memq major-mode ,mode-list) ,@body))

(defmacro ts-docstr-key--with-env (mode-list &rest body)
  "Set up docstring insertion environment."
  (declare (indent 1))
  `(ts-docstr-key--with-major-modes ,mode-list
     (when (ts-docstr-key--valid-p) ,@body)))

(defun ts-docstr-key--valid-p ()
  "Return t when we are able to add document string."
  (and ts-docstr-mode (ts-docstr--comment-p)))

;;
;; (@* "Entry" )
;;

(defun ts-docstr-key--active (act)
  "Enable/Disable trigger base on boolean ACT."
  (dolist (tri ts-docstr-key-alist)
    (let ((key (car tri)) (fnc (cdr tri)))
      (if act (ts-docstr-key--advice-add key :after fnc)
        (ts-docstr-key--advice-remove key fnc)))))

;;;###autoload
(defun ts-docstr-key-enable ()
  "Enable key support."
  (when ts-docstr-key-support  ; Be infront, in order to take effect
    (ts-docstr-key--advice-add "*" :around #'ts-docstr-key--doxygen-asterik)
    (ts-docstr-key--advice-add "RET" :around #'ts-docstr-key--c-like-return)
    (ts-docstr-key--advice-add "RET" :around #'ts-docstr-key--sharp-return))
  (ts-docstr-key--active t))

;;;###autoload
(defun ts-docstr-key-disable ()
  "Disable key support."
  (ts-docstr-key--advice-remove "*" #'ts-docstr-key--doxygen-asterik)
  (ts-docstr-key--advice-remove "RET" #'ts-docstr-key--c-like-return)
  (ts-docstr-key--advice-remove "RET" #'ts-docstr-key--sharp-return)
  (ts-docstr-key--active nil))

;;
;; (@* "Insertion" )
;;

(defun ts-docstr--min-str (str1 str2)
  "Return minimum string by comparing the lenght of STR1 and STR2."
  (cond ((and (null str1) (null str2)) "")
        ((null str1) str2)
        ((null str2) str1)
        (t (if (< (length str1) (length str2)) str1 str2))))

(defun ts-docstr--string-match-mut-p (str1 str2)
  "Mutual way to check STR1 and STR2 with function `string-match-p'."
  (and (stringp str1) (stringp str2)
       (or (string-match-p str1 str2) (string-match-p str2 str1))))

(defcustom ts-docstr-key-inhibit-prefix
  '("//" "--" "#")
  "List of document symbol that are inhibit to insert for prefix."
  :type 'list
  :group 'ts-docstr)

(defun ts-docstr-key--single-line-prefix-insertion ()
  "Insertion for single line comment."
  (when (ts-docstr--current-line-empty-p)  ; Ensure on newline
    (let* ((prev-line-text (save-excursion (forward-line -1) (thing-at-point 'line)))
           (prev-line-doc-symbol (ts-docstr--comment-line-symbol -1))
           (current-line-doc-symbol (ts-docstr--comment-line-symbol))
           (next-line-doc-symbol (ts-docstr--comment-line-symbol 1))
           (prev-line-content (string-trim (s-replace prev-line-doc-symbol "" prev-line-text))))
      (when (or (ts-docstr--string-match-mut-p prev-line-doc-symbol next-line-doc-symbol)
                (and (not (string-empty-p prev-line-content))
                     (not (member prev-line-doc-symbol ts-docstr-key-inhibit-prefix))
                     (string= current-line-doc-symbol next-line-doc-symbol)))
        (insert (concat (ts-docstr--min-str prev-line-doc-symbol next-line-doc-symbol) " "))
        (indent-for-tab-command)))))

;;
;; (@* "Doxygen" )
;;

(defcustom ts-docstr-key-doxygen-like-modes
  '( c-mode c++-mode objc-mode csharp-mode swift-mode
     java-mode groovy-mode processing-mode
     javascript-mode js-mode js2-mode js3-mode json-mode rjsx-mode
     web-mode php-mode
     actionscript-mode typescript-mode
     go-mode scala-mode
     rust-mode rustic-mode
     css-mode ssass-mode scss-mode)
  "List of `major-mode' that can be use Doxygen style."
  :type 'list
  :group 'ts-docstr)

(defun ts-docstr-key--doxygen-like-p ()
  "Return non-nil if current `major-mode' use Doxygen style."
  (memq major-mode ts-docstr-key-doxygen-like-modes))

(defun ts-docstr-key--doxygen-asterik (fnc &rest args)
  "Asterik key for Doxygen like document string.  This fulfill condition,
/* with */ into a pair."
  (apply fnc args)
  (when (ts-docstr-key--doxygen-like-p)
    (save-excursion
      (when (and (ts-docstr--last-char-in-line-p)
                 (ts-docstr--looking-back "/[*]" 2))
        (insert "*/")))))

(defun ts-docstr--between-pair-p (p1 p2)
  "Return non-nil if pair P1 and P2 on the same line."
  (and (save-excursion (search-backward p1 (line-beginning-position) t))
       (save-excursion (search-forward p2 (line-end-position) t))))

(defun ts-docstr-key--insert-prefix ()
  "Insert prefix."
  (ts-docstr--setup-style
    (ignore c-start) (ignore c-end)
    (when c-prefix (insert c-prefix) (indent-for-tab-command))))

(defun ts-docstr-key--c-like-return (fnc &rest args)
  "Return key for C like programming languages.

This function will help insert the corresponding prefix on every line of the
document string."
  (if (or (not (ts-docstr-key--doxygen-like-p)) (not (ts-docstr--comment-p)))
      (apply fnc args)
    (let ((new-doc-p (ts-docstr--between-pair-p "/*" "*/")))
      (apply fnc args)
      (if (ts-docstr--multiline-comment-p)
          (ts-docstr-key--insert-prefix)
        (ts-docstr-key--single-line-prefix-insertion))
      (when (and new-doc-p
                 ;; Make sure end symbol */ still at the back
                 (not (ts-docstr--current-line-empty-p)))
        ;; We can't use `newline-and-indent' here, or else the space will be gone.
        (progn (insert "\n") (indent-for-tab-command))
        (forward-line -1))
      (end-of-line))))

;;
;; (@* "Sharp" )
;;

(defcustom ts-docstr-key-sharp-doc-modes
  '(python-mode ruby-mode sh-mode)
  "List of `major-mode' that use # as document string prefix."
  :type 'list
  :group 'ts-docstr)

(defun ts-docstr-key--sharp-return (fnc &rest args)
  "Return key for programming languages that can use # as document."
  (cond ((and (memq major-mode ts-docstr-key-sharp-doc-modes) (ts-docstr--comment-p))
         (let ((start-comment (ts-docstr--start-comment-symbol)))
           (apply fnc args)
           (when (string-match-p "#" start-comment)
             (ts-docstr-key--single-line-prefix-insertion))))
        (t (apply fnc args))))

;;
;; (@* "Implementation" )
;;

(defun ts-docstr-key-doxygen-like-return (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env ts-docstr-key-doxygen-like-modes
    (let ((ln-prev (ts-docstr--line-relative -1 t))
          (ln-next (ts-docstr--line-relative 1 t)))
      (when (and (string-prefix-p "/*" ln-prev) (string-suffix-p "*/" ln-next)
                 (save-excursion (forward-line 1) (ts-docstr-activatable-p)))
        (ts-docstr--kill-comment-at-point)
        (ts-docstr-at-point)))))

(defun ts-docstr-key-csharp (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(csharp-mode)
    (when (and (ts-docstr--line-is "///")
               (ts-docstr--looking-back "///" 3)
               (ts-docstr-activatable-p))
      (backward-delete-char 3)
      (ts-docstr-at-point))))

(defun ts-docstr-key-go (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(go-mode)
    (when (and (ts-docstr--line-is "//")
               (ts-docstr--looking-back "//" 2)
               (ts-docstr-activatable-p))
      (backward-delete-char 2)
      (ts-docstr-at-point))))

(defun ts-docstr-key-python (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(python-mode)
    (when (and (ts-docstr--line-is "\"\"\"")
               (ts-docstr--looking-back "\"\"\"" 3)
               (ts-docstr-activatable-p))
      (backward-delete-char 3)
      (ts-docstr-at-point))))

(defun ts-docstr-key-ruby (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(ruby-mode)
    (when (and (ts-docstr--line-is "##")
               (ts-docstr--looking-back "##" 2)
               (ts-docstr-activatable-p))
      (backward-delete-char 2)
      (ts-docstr-at-point))))

(defun ts-docstr-key-rust (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(rust-mode)
    (when (and (ts-docstr--line-is "///")
               (ts-docstr--looking-back "///" 3)
               (ts-docstr-activatable-p))
      (backward-delete-char 3)
      (ts-docstr-at-point))))

(defun ts-docstr-key-swift (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-env '(swift-mode)
    (when (and (ts-docstr--line-is "///")
               (ts-docstr--looking-back "///" 3)
               (ts-docstr-activatable-p))
      (backward-delete-char 3)
      (ts-docstr-at-point))))

(provide 'ts-docstr-key)
;;; ts-docstr-key.el ends here
