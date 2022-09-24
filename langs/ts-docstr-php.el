;;; ts-docstr-php.el --- Document string for PHP  -*- lexical-binding: t; -*-

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
                                                   formal_parameters))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-php--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-fp (ts-docstr-grab-nodes-in-range '(formal_parameters)))
         (node-fp (nth 0 nodes-fp))
         (node-cs (ts-docstr-get-next-sibling node-fp "compound_statement")))
    ;; OKAY: This is probably the best solution!
    ;;
    ;; We traverse the entire tree nad look for `return', if it does return
    ;; with something else, we simply return true!
    (cl-some (lambda (return-node) (<= 3 (tsc-count-children return-node)))
             (ts-docstr-find-children-traverse node-cs "return_statement"))))

;;;###autoload
(defun ts-docstr-php-parse ()
  "Parse declaration for PHP."
  (ts-docstr-c-like-narrow-region
    (when-let* ((params (ts-docstr-grab-nodes-in-range '(simple_parameter))))
      (let (types variables)
        (dolist (param params)  ; loop through each parameter declaration
          (tsc-mapc-children
           (lambda (node)
             (pcase (ts-docstr-2-str (tsc-node-type node))
               ("type_list"
                (ts-docstr-push (tsc-node-text node) types))
               ("variable_name"
                (ts-docstr-push (tsc-node-text node) variables))))
           param)
          ;; Make sure the typenames and variables have the same length
          (while (not (= (length types) (length variables)))
            ;; Add until they have the same length
            (if (< (length types) (length variables))
                (ts-docstr-push ts-docstr-default-typename types)
              (ts-docstr-push ts-docstr-default-variable variables))))
        (list :type types :variable variables
              :return (ts-docstr-php--parse-return))))))

(defun ts-docstr-php-config ()
  "Configure style according to variable `ts-docstr-php-style'."
  (cl-case ts-docstr-php-style
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
  (ts-docstr-c-like-narrow-region
    (ts-docstr-inserting
      (let ((types (plist-get data :type)))
        (when-let* ((variables (plist-get data :variable))
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
          (ts-docstr-insert c-end))))))

(provide 'ts-docstr-php)
;;; ts-docstr-php.el ends here
