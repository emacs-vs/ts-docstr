;;; ts-docstr.el --- A document string minor mode using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/ts-docstr
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.9.0"))
;; Keywords: convenience

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
;; Add document string using tree-sitter.
;;

;;; Code:

(require 'subr-x)

(require 's)
(require 'tree-sitter)

(defgroup ts-docstr nil
  "A document string minor mode using tree-sitter."
  :group 'tree-sitter
  :prefix "ts-docstr-")

;;
;; (@* "Entry" )
;;

(defun ts-docstr--enable ()
  "Enable `ts-docstr' in current buffer."
  )

(defun ts-docstr--disable ()
  "Disable `ts-docstr' in current buffer."
  )

;;;###autoload
(define-minor-mode ts-docstr-mode
  "Minor mode `ts-docstr-mode'."
  :lighter " DocStr"
  :group docstr
  (if ts-docstr-mode (ts-docstr--enable) (ts-docstr--disable)))

(defun ts-docstr--turn-on-docstr-mode ()
  "Turn on the `ts-docstr-mode'."
  (ts-docstr-mode 1))

;;;###autoload
(define-globalized-minor-mode global-ts-docstr-mode
  ts-docstr-mode ts-docstr--turn-on-docstr-mode
  :require 'ts-docstr)

;;
;; (@* "Core" )
;;

(defcustom ts-docstr-parsers-alist
  `((c-mode . ts-docstr-c))
  ""
  :type '(alist key-type symbol))

(defun ts-docstr--require-module ()
  "Require LANG module."
  (when-let ((module (asoc-get ts-docstr-parsers-alist major-mode)))
    (require module nil t)))

;;;###autoload
(defun ts-docstr-at-point ()
  "Add document string at point."
  (interactive)
  (if (ts-docstr--require-module))
  )

(provide 'ts-docstr)
;;; ts-docstr.el ends here
