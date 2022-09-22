;;; ts-docstr-c++.el --- Document string for C++  -*- lexical-binding: t; -*-

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
;; Document string for C++.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-c++-style 'javadoc
  "Style specification for document string in C++."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Javadoc Style" javadoc)
                 (const :tag "Qt Style" qt))
  :group 'ts-docstr)

(defcustom ts-docstr-c++-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c++-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c++-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-c++-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_specifier
                                                   struct_specifier
                                                   enum_specifier
                                                   function_declarator))))
      (cond ((zerop (length nodes))
             (user-error "No declaration found"))
            ((<= 2 (length nodes))
             (user-error "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-c++--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-fd (ts-docstr-grab-nodes-in-range '(function_declarator)))
         (node-fd (nth 0 nodes-fd))
         (parent (tsc-get-parent node-fd))
         (return t))
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-c++-parse ()
  "Parse declaration for C++."
  (ts-docstr-c-like-narrow-region
    (when-let* ((params (ts-docstr-grab-nodes-in-range '(parameter_declaration))))
      (let (types variables)
        (dolist (param params)
          (tsc-traverse-mapc
           (lambda (node)
             (when (ts-docstr-leaf-p node)
               (pcase (ts-docstr-2-str (tsc-node-type node))
                 ((or "primitive_type" "type_identifier")
                  (ts-docstr-push (tsc-node-text node) types))
                 ("identifier"
                  (ts-docstr-push (tsc-node-text node) variables))
                 ((or "*" "&" "[" "]")
                  (if (null types)
                      (ts-docstr-push (tsc-node-text node) types)
                    (let ((last (1- (length types))))
                      (setf (nth last types)
                            (concat (nth last types) (tsc-node-text node)))))))))
           param))
        `(:type ,types :variable ,variables :return ,(ts-docstr-c++--parse-return))))))

(defun ts-docstr-c++-config ()
  "Configure style according to variable `ts-docstr-c++-style'."
  (cl-case ts-docstr-c++-style
    (javadoc (list :start "/**"
                   :prefix "* "
                   :end "*/"
                   :summary "{d}"
                   :param "@param {v} {d}"
                   :return "@return {d}"))
    (qt (list :start "/*!"
              :prefix "    "
              :end "*/"
              :summary "{d}"
              :param "\\\\param {v} {d}"
              :return "\\\\return {d}"))
    (t (list :start "/**"
             :prefix "* "
             :end "*/"
             :summary ts-docstr-c++-format-summary
             :param ts-docstr-c++-format-param
             :return ts-docstr-c++-format-return))))

;;;###autoload
(defun ts-docstr-c++-insert (_node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c-like-narrow-region
    (ts-docstr-inserting
     (when-let* ((types (plist-get data :type))
                 (variables (plist-get data :variable))
                 (len (length types)))
       (insert c-start "\n")
       (insert c-prefix (ts-docstr-format 'summary) "\n")
       (setq restore-point (1- (point)))
       (dotimes (index len)
         (insert c-prefix (ts-docstr-format 'param :variable (nth index variables)) "\n"))
       (when (plist-get data :return)
         (insert c-prefix (ts-docstr-format 'return) "\n"))
       (insert c-end)))))

(provide 'ts-docstr-c++)
;;; ts-docstr-c++.el ends here
