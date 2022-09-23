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

(defcustom ts-docstr-key-alist
  `(("RET" . ts-docstr-key-c-like-return)
    ("/"   . ts-docstr-key-csharp)
    ("/"   . ts-docstr-key-golang)
    ("\""  . ts-docstr-key-python)
    ("#"   . ts-docstr-key-ruby)
    ("/"   . ts-docstr-key-rust)
    ("/"   . ts-docstr-key-swift))
  "List of trigger to each `major-mode'."
  :type 'hook
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

(defun ts-docstr-comment-p ()
  "Return non-nil when inside comment block."
  (nth 4 (syntax-ppss)))

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

(defun ts-docstr-key--valid-p ()
  "Return t when we are able to add document string."
  (and ts-docstr-mode (ts-docstr-comment-p)))

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
  (ts-docstr-key--active t))

;;;###autoload
(defun ts-docstr-key-disable ()
  "Disable key support."
  (ts-docstr-key--active nil))

;;
;; (@* "Implementation" )
;;

(defun ts-docstr-key-c-like-return (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes
      (list c-mode c++-mode csharp-mode java-mode
            javascript-mode js-mode js2-mode js3-mode)
    ;; TODO: ..
    ))

(defun ts-docstr-key-csharp (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(csharp-mode)
    (when (ts-docstr-key--valid-p)
      (when (and (ts-docstr--line-is "///")
                 (ts-docstr--looking-back "///" 3))
        (backward-delete-char 3)
        (ts-docstr-at-point)))))

(defun ts-docstr-key-golang (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(go-mode)
    ;; TODO: ..
    ))

(defun ts-docstr-key-python (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(python-mode)
    ;; TODO: ..
    ))

(defun ts-docstr-key-ruby (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(ruby-mode)
    ;; TODO: ..
    ))

(defun ts-docstr-key-rust (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(rust-mode)
    ;; TODO: ..
    ))

(defun ts-docstr-key-swift (&rest _)
  "Insert docstring with key."
  (ts-docstr-key--with-major-modes '(swift-mode)
    ;; TODO: ..
    ))

(provide 'ts-docstr-key)
;;; ts-docstr-key.el ends here
