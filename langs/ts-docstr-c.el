;;; ts-docstr-c.el --- Document string for C  -*- lexical-binding: t; -*-

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
;; Document string for C.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-c-style nil
  "Style specification for document string in C."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

;;;###autoload
(defun ts-docstr-c-activate ()
  ""
  (interactive)
  (let* ((pos (point))
         (root (tsc-root-node tree-sitter-tree))
         ;;(node (tsc-get-descendant-for-point-range root pos pos))
         )
    (message "node: %s" (tsc-node-text root))
    ))

;;;###autoload
(defun ts-docstr-c-parse ()
  ""
  (ts-docstr--parser-data nil nil))

;;;###autoload
(defun ts-docstr-c-insert ()
  "")

(provide 'ts-docstr-c)
;;; ts-docstr-c.el ends here
