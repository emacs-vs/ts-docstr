;;; ts-docstr-typescript.el --- Document string for TypeScript  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Shen, Jen-Chieh

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
;; Document string for TypeScript.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-typescript-style 'typedoc
  "Style specification for document string in TypeScript."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "TypeDoc" typedoc)
                 (const :tag "TsDoc" tsdoc))
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-format-param "@param {{t}} {v} - {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-typescript-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-typescript-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   interface_declaration
                                                   enum_declaration
                                                   function_declaration
                                                   method_definition))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-typescript--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "type_identifier")
                         (ts-docstr-find-children node "property_identifier")
                         (ts-docstr-find-children node "identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-typescript--parse-return (nodes-fp is-method)
  "Return t if function does have return value."
  (when-let*
      ((node-fp (nth 0 nodes-fp))
       (node-sb (ts-docstr-get-next-sibling node-fp "statement_block"))
       (node-sb-prev (tsc-get-prev-sibling node-sb)))
    (or (and is-method
             ;; In TypesSript's method (not JavaScript's function), if node
             ;; before statement_block isn't parameter list then it does
             ;; return something!
             (not (equal (tsc-node-type node-sb-prev) 'formal_parameters)))
        ;; OKAY: This is probably the best solution!
        ;;
        ;; We traverse the entire tree nad look for `return', if it does return
        ;; with something else, we simply return true!
        (cl-some (lambda (return-node) (<= 3 (tsc-count-children return-node)))
                 (ts-docstr-find-children-traverse node-sb "return_statement")))))

;;;###autoload
(defun ts-docstr-typescript-parse (node)
  "Parse declaration for TypeScript."
  (ts-docstr-c-like-narrow-region
    (if-let* ((params (ts-docstr-find-children node "formal_parameters"))
              (parent (tsc-get-parent (nth 0 params))))
        (let* ((node-method-or-function (tsc-node-type parent))
               (is-method (eq node-method-or-function 'method_definition))
               types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'required_parameter)
                 (tsc-traverse-mapc
                  (lambda (child)  ; access `required_parameter' child
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      ((or "type_identifier" "predefined_type")
                       (ts-docstr-push (tsc-node-text child) types))
                      ("identifier"
                       (unless is-method
                         (ts-docstr-push ts-docstr-default-typename types))
                       (ts-docstr-push (tsc-node-text child) variables))))
                  node))
               ;; Make sure the typenames and variables have the same length
               (while (not (= (length types) (length variables)))
                 ;; Add until they have the same length
                 (if (< (length types) (length variables))
                     (ts-docstr-push ts-docstr-default-typename types)
                   (ts-docstr-push ts-docstr-default-variable variables))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-typescript--parse-return params is-method)
                :name (ts-docstr-typescript--get-name node)))
      (list :name (ts-docstr-typescript--get-name node)))))

(defun ts-docstr-typescript-config ()
  "Configure style according to variable `ts-docstr-typescript-style'."
  (ts-docstr-with-style-case
    (typedoc (list :start "/**"
                   :prefix "* "
                   :end "*/"
                   :summary "{d}"
                   :param "@param {v} {d}"
                   :return "@returns {d}"))
    (tsdoc (list :start "/**"
                 :prefix "* "
                 :end "*/"
                 :summary "{d}"
                 :param "@param {v} - {d}"
                 :return "@returns {d}"))
    (t (list :start ts-docstr-typescript-start
             :prefix ts-docstr-typescript-prefix
             :end ts-docstr-typescript-end
             :summary ts-docstr-typescript-format-summary
             :param ts-docstr-typescript-format-param
             :return ts-docstr-typescript-format-return))))

;;;###autoload
(defun ts-docstr-typescript-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      ((or function_declaration method_definition)  ; For function
       (let* ((types (plist-get data :type))
              (variables (plist-get data :variable))
              (len (length variables)))
         (ts-docstr-with-style-case
           ((or typedoc tsdoc jsdoc google)
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
         ((or typedoc tsdoc jsdoc google)
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-typescript)
;;; ts-docstr-typescript.el ends here
