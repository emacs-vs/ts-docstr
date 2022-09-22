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

(defmacro ts-docstr-c--narrow-region (&rest body)
  "Narrow region to class/struct/function declaration."
  (declare (indent 0))
  `(save-restriction
     (narrow-to-region (line-beginning-position) (line-end-position 2))
     ,@body))

;;;###autoload
(defun ts-docstr-c-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c--narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(function_declarator)))
           (len-nodes (length nodes)))
      (cond ((zerop len-nodes) (user-error "No declaration found"))
            ((<= 2 len-nodes) (user-error "Multiple declarations are invalid"))
            (t t)))))

;;;###autoload
(defun ts-docstr-c-parse ()
  ""
  (ts-docstr--parser-data nil nil))

;;;###autoload
(defun ts-docstr-c-insert ()
  "")

(provide 'ts-docstr-c)
;;; ts-docstr-c.el ends here
