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

(require 'ts-docstr)

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
                                                   method_declaration))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-java--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (ts-docstr-find-children node "identifier"))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-java--parse-return (nodes-pl)
  "Return t if function does have return value."
  (let* ((node-pl (nth 0 nodes-pl))
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
(defun ts-docstr-java-parse (node)
  "Parse declaration for Java."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "formal_parameters")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'formal_parameter)
                 (tsc-mapc-children
                  (lambda (child)
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      ("identifier"
                       (ts-docstr-push (tsc-node-text child) variables))
                      (_
                       (ts-docstr-push (tsc-node-text child) types))))
                  node)))
             param))
          (list :type types :variable variables
                :return (ts-docstr-java--parse-return params)
                :name (ts-docstr-java--get-name node)))
      (list :name (ts-docstr-java--get-name node)))))

(defun ts-docstr-java-config ()
  "Configure style according to variable `ts-docstr-java-style'."
  (ts-docstr-with-style-case
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
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (method_declaration  ; For function
       (when-let* ((types (plist-get data :type))
                   (variables (plist-get data :variable))
                   (len (length variables)))
         (ts-docstr-with-style-case
           (javadoc
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
            (ts-docstr-insert c-end))
           (t
            (ts-docstr-custom-insertion node data)))))
      ;; For the rest of the type, class/struct/enum
      (t
       (ts-docstr-with-style-case
         (javadoc
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-java)
;;; ts-docstr-java.el ends here
