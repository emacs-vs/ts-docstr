;;; ts-docstr-go.el --- Document string for Go  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh

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
                 (const :tag "Official Golang documentation generator" godoc)
                 (const :tag "Swag RESTful API" swag))
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
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(type_declaration
                                                   function_declaration))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-go--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children-traverse node "type_identifier")
                         (ts-docstr-find-children node "identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-go--parse-return (nodes-pl)
  "Return t if function does have return value."
  (when-let*
      ((node-pl (nth 0 nodes-pl))
       (node-sb (ts-docstr-get-next-sibling node-pl "block"))
       (node-sb-prev (tsc-get-prev-sibling node-sb)))
    ;; If node before `block' node is not `parameter_list', it means the user
    ;; does not define a return type.
    (or (not (equal (tsc-node-type node-sb-prev) 'parameter_list))
        ;; OKAY: This is probably the best solution!
        ;;
        ;; We traverse the entire tree nad look for `return', if it does return
        ;; with something else, we simply return true!
        (cl-some (lambda (return-node) (<= 2 (tsc-count-children return-node)))
                 (ts-docstr-find-children-traverse node-sb "return_statement")))))

;;;###autoload
(defun ts-docstr-go-parse (node)
  "Parse declaration for Go."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "parameter_list")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter_declaration)
                 (tsc-mapc-children
                  (lambda (child)
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      ("identifier"
                       (ts-docstr-push (tsc-node-text child) variables))
                      (_
                       (ts-docstr-push (tsc-node-text child) types))))
                  node))
               ;; TODO: There is no way to parse typenames in order. The parsed
               ;; tree looks something like this:
               ;;
               ;; @ Parameters
               ;;   (a int, b, c float)
               ;;
               ;; @ Tree
               ;;   ...
               ;;   parameter_declaration:
               ;;     identifier:                       ; a
               ;;     type_identifier:                  ; int
               ;;   parameter_declaration:
               ;;     identifier:                       ; b
               ;;     identifier:                       ; c
               ;;     type_identifier:                  ; float
               ;;
               ;; There might be a workaround but it mostly likely not going to
               ;; work well.
               ;;
               ;;                                            Date: 2022-09-24
               ;;
               ;; Make sure the typenames and variables have the same length
               (while (not (= (length types) (length variables)))
                 ;; Add until they have the same length
                 (if (< (length types) (length variables))
                     (ts-docstr-push ts-docstr-default-typename types)
                   (ts-docstr-push ts-docstr-default-variable variables))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-go--parse-return params)
                :name (ts-docstr-go--get-name node)))
      (list :name (ts-docstr-go--get-name node)))))

(defun ts-docstr-go-config ()
  "Configure style according to variable `ts-docstr-go-style'."
  (ts-docstr-with-style-case
    (godoc (list :start ""
                 :prefix "// "
                 :end ""
                 :summary "{d}"
                 :param ""
                 :return ""))
    (swag (list :start ""
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
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (function_declaration
       (when-let* ((types (plist-get data :type))
                   (variables (plist-get data :variable))
                   (len (length types)))
         (ts-docstr-with-style-case
           (godoc (ts-docstr-insert c-prefix (ts-docstr-format 'summary)))
           (swag
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
              (ts-docstr-insert c-prefix (ts-docstr-format 'return)))
            (ts-docstr-insert c-end))
           (t
            (ts-docstr-custom-insertion node data)))))
      ;; For the rest of the type, class/struct/enum
      (t
       (ts-docstr-with-style-case
         (godoc (ts-docstr-insert c-prefix (ts-docstr-format 'summary)))
         (swag (ts-docstr-insert c-prefix (ts-docstr-format 'summary)))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-go)
;;; ts-docstr-go.el ends here
