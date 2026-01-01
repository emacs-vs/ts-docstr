;;; ts-docstr-php.el --- Document string for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Shen, Jen-Chieh

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
;; Document string for PHP.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-php-style 'phpdoc
  "Style specification for document string in PHP."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "PHPDoc Style" phpdoc))
  :group 'ts-docstr)

(defcustom ts-docstr-php-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-php-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-php-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-php-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-php-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-php-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-php-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   enum_declaration
                                                   function_definition))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-php--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (ts-docstr-find-children node "name"))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-php--parse-return (nodes-fp)
  "Return t if function does have return value."
  (let* ((node-fp (nth 0 nodes-fp))
         (node-cs (ts-docstr-get-next-sibling node-fp "compound_statement")))
    ;; OKAY: This is probably the best solution!
    ;;
    ;; We traverse the entire tree nad look for `return', if it does return
    ;; with something else, we simply return true!
    (cl-some (lambda (return-node) (<= 3 (tsc-count-children return-node)))
             (ts-docstr-find-children-traverse node-cs "return_statement"))))

;;;###autoload
(defun ts-docstr-php-parse (node)
  "Parse declaration for PHP."
  (ts-docstr-c-like-narrow-region
    (if-let* ((params (ts-docstr-find-children node "formal_parameters")))
        (let (types variables)
          (dolist (param params)  ; loop through each parameter declaration
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'simple_parameter)
                 (tsc-mapc-children
                  (lambda (child)
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      ("type_list"
                       (ts-docstr-push (tsc-node-text child) types))
                      ("variable_name"
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
                :return (ts-docstr-php--parse-return params)))
      (list :name (ts-docstr-php--get-name node)))))

(defun ts-docstr-php-config ()
  "Configure style according to variable `ts-docstr-php-style'."
  (ts-docstr-with-style-case
    (phpdoc (list :start "/**"
                  :prefix "* "
                  :end "*/"
                  :summary "{d}"
                  :param "@param {v} {d}"
                  :return "@return {d}"))
    (t (list :start ts-docstr-php-start
             :prefix ts-docstr-php-prefix
             :end ts-docstr-php-end
             :summary ts-docstr-php-format-summary
             :param ts-docstr-php-format-param
             :return ts-docstr-php-format-return))))

;;;###autoload
(defun ts-docstr-php-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (function_definition
       (let* ((types (plist-get data :type))
              (variables (plist-get data :variable))
              (len (length variables)))
         (ts-docstr-with-style-case
           (phpdoc
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
         (phpdoc
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-php)
;;; ts-docstr-php.el ends here
