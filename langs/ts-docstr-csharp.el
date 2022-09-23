;;; ts-docstr-csharp.el --- Document string for C#  -*- lexical-binding: t; -*-

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
;; Document string for C#.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-csharp-style nil
  "Style specification for document string in C#."
  :type '(choice (const :tag "No specify" nil))
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-start "/// <summary>"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-prefix "/// "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-end "/// <summary>"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-format-param "<param name=\"{v}\">{d}</param>"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-csharp-format-return "<returns>{d}</returns>"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-csharp-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   struct_declaration
                                                   enum_declaration
                                                   method_declaration))))
      (cond ((zerop (length nodes))
             (user-error "No declaration found"))
            ((<= 2 (length nodes))
             (user-error "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-csharp--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "type_identifier")
                         (ts-docstr-find-children node "identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-csharp--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-pl (ts-docstr-grab-nodes-in-range '(parameter_list)))
         (node-pl (nth 0 nodes-pl))
         (parent (tsc-get-parent node-pl))
         (return t))
    ;; OKAY: We don't traverse like `JavaScript' does, since C# needs to declare
    ;; return type in the function declaration.
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-csharp-parse (node)
  "Parse declaration for C#."
  (ts-docstr-c-like-narrow-region
    ;; OKAY: We find parameters directly from the captured node, this is much
    ;; faster than the previous capture method.
    (if-let ((params (ts-docstr-find-children node "parameter_list")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter)  ; Enters `parameter' node
                 (dotimes (index (tsc-count-children node))
                   (let ((child (tsc-get-nth-child node index)))  ; access `parameter' child
                     (pcase (ts-docstr-2-str (tsc-node-type child))
                       ("predefined_type"
                        (ts-docstr-push (tsc-node-text child) types))
                       ("identifier"
                        (ts-docstr-push
                         (tsc-node-text child)
                         ;; If first child (index 0) is `identifier', then it
                         ;; could be a type. Otherwise, it's a variable name.
                         (if (zerop index) types variables)))
                       ("equals_value_clause"
                        ;; TODO: default-value?
                        ))))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-csharp--parse-return)
                :name (ts-docstr-csharp--get-name node)))
      (list :name (ts-docstr-csharp--get-name node)))))

(defun ts-docstr-csharp-config ()
  "Configure style according to variable `ts-docstr-csharp-style'."
  (cl-case ts-docstr-csharp-style
    (t (list :start ts-docstr-csharp-start
             :prefix ts-docstr-csharp-prefix
             :end ts-docstr-csharp-end
             :summary ts-docstr-csharp-format-summary
             :param ts-docstr-csharp-format-param
             :return ts-docstr-csharp-format-return))))

;;;###autoload
(defun ts-docstr-csharp-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-c-like-narrow-region
    (ts-docstr-inserting
      (cl-case (tsc-node-type node)
        (method_declaration
         (when-let* ((types (plist-get data :type))
                     (variables (plist-get data :variable))
                     (len (length types)))
           (insert c-start "\n")
           (insert c-prefix (ts-docstr-format 'summary) "\n")
           (setq restore-point (1- (point)))
           (insert c-end "\n")
           (dotimes (index len)
             (insert c-prefix (ts-docstr-format 'param :variable (nth index variables)) "\n"))
           (when (plist-get data :return)
             (insert c-prefix (ts-docstr-format 'return)))))
        (t
         (insert c-start "\n")
         (insert c-prefix (ts-docstr-format 'summary) "\n")
         (setq restore-point (1- (point)))
         (insert c-end))))))

(provide 'ts-docstr-csharp)
;;; ts-docstr-csharp.el ends here
