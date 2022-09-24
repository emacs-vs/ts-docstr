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

(defcustom ts-docstr-c++-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c++-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-c++-end "*/"
  "Docstring end line."
  :type 'string
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
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(preproc_def
                                                   class_specifier
                                                   struct_specifier
                                                   enum_specifier
                                                   function_declarator))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-c++--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "type_identifier")
                         (ts-docstr-find-children node "identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-c++--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-fd (ts-docstr-grab-nodes-in-range '(function_declarator)))
         (node-fd (nth 0 nodes-fd))
         (parent (tsc-get-parent node-fd))
         (return t))
    ;; OKAY: We don't traverse like `JavaScript' does, since C/C++ needs to declare
    ;; return type in the function declaration.
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-c++-parse (node)
  "Parse declaration for C++."
  (ts-docstr-c-like-narrow-region
    ;; OKAY: We find parameters directly from the captured node, this is much
    ;; faster than the previous capture method (previously, we were capturing
    ;; it from the whole buffer).
    (if-let ((params (ts-docstr-find-children node "parameter_list")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter_declaration)  ; Enters `parameter_declaration' node
                 (dotimes (index (tsc-count-children node))
                   (let ((child (tsc-get-nth-child node index)))  ; access `parameter_declaration' child
                     (pcase (ts-docstr-2-str (tsc-node-type child))
                       ((or "primitive_type" "type_identifier")
                        (ts-docstr-push (tsc-node-text child) types))
                       ((or "identifier"
                            "array_declarator"
                            "pointer_declarator"
                            "reference_declarator")
                        (ts-docstr-push (s-replace " " "" (tsc-node-text child)) variables)))))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-c++--parse-return)
                :name (ts-docstr-c++--get-name node)))
      (list :name (ts-docstr-c++--get-name node)))))

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
    (t (list :start ts-docstr-c++-start
             :prefix ts-docstr-c++-prefix
             :end ts-docstr-c++-end
             :summary ts-docstr-c++-format-summary
             :param ts-docstr-c++-format-param
             :return ts-docstr-c++-format-return))))

;;;###autoload
(defun ts-docstr-c++-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c-like-narrow-region
    (ts-docstr-inserting
      (cl-case (tsc-node-type node)
        (function_declarator  ; For function
         (when-let* ((types (plist-get data :type))
                     (variables (plist-get data :variable))
                     (len (length variables)))
           (ts-docstr-insert c-start "\n")
           (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
           (setq restore-point (1- (point)))
           (dotimes (index len)
             (ts-docstr-insert c-prefix
                               (ts-docstr-format 'param
                                                 :typename (nth index types)
                                                 :variable (nth index variables))
                               "\n"))
           (when (plist-get data :return)
             (ts-docstr-insert c-prefix (ts-docstr-format 'return) "\n"))
           (ts-docstr-insert c-end)))
        (t  ; For the rest of the type, class/struct/enum
         (ts-docstr-insert c-start "\n")
         (ts-docstr-insert c-prefix "\n")
         (setq restore-point (1- (point)))
         (ts-docstr-insert c-end))))))

(provide 'ts-docstr-c++)
;;; ts-docstr-c++.el ends here
