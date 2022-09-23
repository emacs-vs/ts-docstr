;;; ts-docstr-go.el --- Document string for Go  -*- lexical-binding: t; -*-

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
;; Document string for Go.
;;

;;; Code:

(require 'ts-docstr-c++)

(defcustom ts-docstr-go-style 'godoc
  "Style specification for document string in Go."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Document String in Golang" godoc))
  :group 'ts-docstr)


(defcustom ts-docstr-go-start "// "
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-go-prefix "// "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-go-end ""
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-go-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-go-format-param "@param {v} - {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-go-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-go-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(struct_type
                                                   field_identifier
                                                   parameter_list))))
      (cond ((zerop (length nodes))
             (user-error "No declaration found"))
            ((<= 2 (length nodes))
             (user-error "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-go--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-pl (ts-docstr-grab-nodes-in-range '(formal_parameters)))
         (node-pl (nth 0 nodes-pl))
         (parent (tsc-get-parent node-pl))
         (return t))
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-go-parse ()
  "Parse declaration for Go."
  (ts-docstr-c-like-narrow-region
    (when-let* ((params (ts-docstr-grab-nodes-in-range '(formal_parameters))))
      (let (types variables default-values)
        (dolist (param params)
          (tsc-traverse-mapc
           (lambda (node)
             (pcase (ts-docstr-2-str (tsc-node-type node))
               ((or "array_type" "integral_type" "floating_point_type"
                    "boolean_type" "scoped_type_identifier"
                    "generic_type" "type_identifier")
                (ts-docstr-push (tsc-node-text node) types))
               ("identifier"
                (ts-docstr-push (tsc-node-text node) variables))))
           param))
        (list :type types :variable variables
              :default-values default-values
              :return (ts-docstr-go--parse-return))))))

(defun ts-docstr-go-config ()
  "Configure style according to variable `ts-docstr-go-style'."
  (cl-case ts-docstr-go-style
    (godoc (list :start "// "
                 :prefix "// "
                 :end ""
                 :summary "{d}"
                 :param "@param {v} - {d}"
                 :return "@return {d}"))
    (t (list :start ts-docstr-go-start
             :prefix ts-docstr-go-prefix
             :end ts-docstr-go-end
             :summary ts-docstr-go-format-summary
             :param ts-docstr-go-format-param
             :return ts-docstr-go-format-return))))

;;;###autoload
(defun ts-docstr-go-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c++-insert node data))

(provide 'ts-docstr-go)
;;; ts-docstr-go.el ends here
