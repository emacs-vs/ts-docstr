;;; ts-docstr-scala.el --- Document string for Scala  -*- lexical-binding: t; -*-

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
;; Document string for Scala.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-scala-style 'scaladoc
  "Style specification for document string in Scala."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Scaladoc" scaladoc))
  :group 'ts-docstr)

(defcustom ts-docstr-scala-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-scala-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-scala-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-scala-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-scala-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-scala-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-scala-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_definition
                                                   object_definition
                                                   trait_definition
                                                   val_definition
                                                   var_definition
                                                   type_definition
                                                   function_definition))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-scala--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (ts-docstr-find-children node "identifier"))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-scala--parse-return (nodes-p)
  "Return t if function does have return value."
  (let* ((node-p (nth 0 nodes-p))
         (node-block (or (ts-docstr-get-next-sibling node-p "block")
                         (ts-docstr-get-next-sibling node-p "call_expression")))
         (node-block-prev (tsc-get-prev-sibling node-block)))
    (or
     ;; If specified at the end.
     (not (equal (tsc-node-type node-block-prev) 'parameters))
     ;; OKAY: This is probably the best solution!
     ;;
     ;; We traverse the entire tree nad look for `return', if it does return
     ;; with something else, we simply return true!
     (cl-some (lambda (return-node)
                (and (equal "return" (tsc-node-text return-node))
                     (not (null (tsc-get-next-sibling return-node)))))
              (ts-docstr-find-children-traverse node-block "identifier")))))

;;;###autoload
(defun ts-docstr-scala-parse (node)
  "Parse declaration for scala."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "parameters")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter)
                 (tsc-mapc-children
                  (lambda (child)
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      (":" )  ; do nothing! skip it!
                      ("identifier"
                       (ts-docstr-push (tsc-node-text child) variables))
                      (t
                       (ts-docstr-push (tsc-node-text child) types))))
                  node)))
             param))
          (list :type types :variable variables
                :return (ts-docstr-scala--parse-return params)
                :name (ts-docstr-scala--get-name node)))
      (list :name (ts-docstr-scala--get-name node)))))

(defun ts-docstr-scala-config ()
  "Configure style according to variable `ts-docstr-scala-style'."
  (cl-case ts-docstr-scala-style
    (scaladoc (list :start "/**"
                    :prefix "* "
                    :end "*/"
                    :summary "{d}"
                    :param "@param {v} {d}"
                    :return "@return {d}"))
    (t (list :start ts-docstr-scala-start
             :prefix ts-docstr-scala-prefix
             :end ts-docstr-scala-end
             :summary ts-docstr-scala-format-summary
             :param ts-docstr-scala-format-param
             :return ts-docstr-scala-format-return))))

;;;###autoload
(defun ts-docstr-scala-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (function_definition  ; For function
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

(provide 'ts-docstr-scala)
;;; ts-docstr-scala.el ends here
