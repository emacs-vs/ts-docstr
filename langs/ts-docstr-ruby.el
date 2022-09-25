;;; ts-docstr-ruby.el --- Document string for Ruby  -*- lexical-binding: t; -*-

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
;; Document string for Ruby.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-ruby-style 'rdoc
  "Style specification for document string in Ruby."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Ruby Documentation System" rdoc))
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-start "##"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-prefix "# "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-end ""
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-format-param "+{v}+ {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-ruby-format-return ""
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-ruby-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class
                                                   method))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-ruby--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (ts-docstr-find-children node "identifier"))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-ruby--parse-return (nodes-pl)
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
(defun ts-docstr-ruby-parse (node)
  "Parse declaration for Ruby."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "method_parameters")))
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
                      (t
                       (ts-docstr-push (tsc-node-text child) types))))
                  node)))
             param))
          (list :type types :variable variables
                :return (ts-docstr-ruby--parse-return params)
                :name (ts-docstr-ruby--get-name node)))
      (list :name (ts-docstr-ruby--get-name node)))))

(defun ts-docstr-ruby-config ()
  "Configure style according to variable `ts-docstr-ruby-style'."
  (cl-case ts-docstr-ruby-style
    (rdoc (list :start "##"
                :prefix "# "
                :end ""
                :summary "{d}"
                :param "+{v}+ {d}"
                :return ""))
    (t (list :start ts-docstr-ruby-start
             :prefix ts-docstr-ruby-prefix
             :end ts-docstr-ruby-end
             :summary ts-docstr-ruby-format-summary
             :param ts-docstr-ruby-format-param
             :return ts-docstr-ruby-format-return))))

;;;###autoload
(defun ts-docstr-ruby-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (method_declaration  ; For function
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
       (ts-docstr-insert c-end)))))

(provide 'ts-docstr-ruby)
;;; ts-docstr-ruby.el ends here
