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
  `(("RET" . ts-docstr-trigger-c-like-return)
    ("/"   . ts-docstr-trigger-csharp)
    ("/"   . ts-docstr-trigger-golang)
    ("-"   . ts-docstr-trigger-lua)
    ("RET" . ts-docstr-trigger-lua-return)
    ("\""  . ts-docstr-trigger-python)
    ("#"   . ts-docstr-trigger-ruby)
    ("/"   . ts-docstr-trigger-rust)
    ("/"   . ts-docstr-trigger-swift))
  "List of trigger to each `major-mode'."
  :type 'hook
  :group 'ts-docstr)

;;
;; (@* "Util" )
;;

(defun ts-docstr-key--advice-add (key where fnc)
  "Safe add advice KEY with FNC at WHERE."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-add key-fnc where fnc))))

(defun ts-docstr-key--advice-remove (key fnc)
  "Safe remove advice KEY with FNC."
  (let ((key-fnc (key-binding (kbd key))))
    (when (symbolp key-fnc) (advice-remove key-fnc fnc))))

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

(provide 'ts-docstr-key)
;;; ts-docstr-key.el ends here
