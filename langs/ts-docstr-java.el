;;; ts-docstr-java.el --- Document string for Java  -*- lexical-binding: t; -*-

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
;; Document string for Java.
;;

;;; Code:

(require 'ts-docstr-c++)

(defcustom ts-docstr-java-style 'javadoc
  "Style specification for document string in Java."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Javadoc Style" javadoc))
  :group 'ts-docstr)

(defcustom ts-docstr-java-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-java-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-java-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-java-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-java-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-java-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-java-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   interface_declaration
                                                   enum_declaration
                                                   formal_parameters))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-java--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-pl (ts-docstr-grab-nodes-in-range '(formal_parameters)))
         (node-pl (nth 0 nodes-pl))
         (parent (tsc-get-parent node-pl))
         (return t))
    ;; OKAY: We don't traverse like `JavaScript' does, since Java needs to declare
    ;; return type in the function declaration.
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-java-parse ()
  "Parse declaration for Java."
  (ts-docstr-c-like-narrow-region
    (when-let* ((params (ts-docstr-grab-nodes-in-range '(formal_parameters))))
      (let (types variables)
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
              :return (ts-docstr-java--parse-return))))))

(defun ts-docstr-java-config ()
  "Configure style according to variable `ts-docstr-java-style'."
  (cl-case ts-docstr-java-style
    (javadoc (list :start "/**"
                   :prefix "* "
                   :end "*/"
                   :summary "{d}"
                   :param "@param {v} {d}"
                   :return "@return {d}"))
    (t (list :start ts-docstr-java-start
             :prefix ts-docstr-java-prefix
             :end ts-docstr-java-end
             :summary ts-docstr-java-format-summary
             :param ts-docstr-java-format-param
             :return ts-docstr-java-format-return))))

;;;###autoload
(defun ts-docstr-java-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c++-insert node data))

(provide 'ts-docstr-java)
;;; ts-docstr-java.el ends here
