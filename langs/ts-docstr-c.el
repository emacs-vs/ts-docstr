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

(defcustom ts-docstr-c-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-c-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(struct_specifier
                                                   enum_specifier
                                                   function_declarator))))
      (cond ((zerop (length nodes))
             (user-error "No declaration found"))
            ((<= 2 (length nodes))
             (user-error "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;;;###autoload
(defun ts-docstr-c-parse ()
  "Parse declaration for C."
  (ts-docstr-c++-parse))

(defun ts-docstr-c-config ()
  "Configure style according to variable `ts-docstr-c-style'."
  (cl-case ts-docstr-c-style
    (t (list :start "/**"
             :prefix "* "
             :end "*/"
             :summary ts-docstr-c-format-summary
             :param ts-docstr-c-format-param
             :return ts-docstr-c-format-return))))

;;;###autoload
(defun ts-docstr-c-insert (_node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c-like-narrow-region
    (ts-docstr-inserting
     (when-let* ((types (plist-get data :type))
                 (variables (plist-get data :variable))
                 (len (length types)))
       (insert c-start "\n")
       (setq restore-point (point))
       (insert c-prefix (ts-docstr-format 'summary) "\n")
       (dotimes (index len)
         (insert c-prefix (ts-docstr-format 'param :variable (nth index variables)) "\n"))
       (when (plist-get data :return)
         (insert c-prefix (ts-docstr-format 'return) "\n"))
       (insert c-end)))))

(provide 'ts-docstr-c)
;;; ts-docstr-c.el ends here
