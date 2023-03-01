;;; ts-docstr-lua.el --- Document string for Lua  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

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
;; Document string for Lua.
;;

;;; Code:

(defcustom ts-docstr-lua-style 'luadoc
  "Style specification for document string in Lua."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "Kepler's specification" luadoc)
                 (const :tag "doxygen/Javadoc-like style" doxygen)
                 (const :tag "Lua based document generator to Markdown" scriptum))
  :group 'ts-docstr)

(defcustom ts-docstr-lua-start "--- "
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-prefix "-- "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-end ""
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-format-param "@param {{t}} {v} - {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-lua-splitter
  "-------------------------------------"
  "Document string splitter for Lua programming language."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-lua-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   function_declaration))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-lua--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (ts-docstr-find-children node "identifier"))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-lua--parse-return (nodes-fp)
  "Return t if function does have return value."
  (when-let*
      ((node-fp (nth 0 nodes-fp))
       (node-sb (ts-docstr-get-next-sibling node-fp "statement_block")))
    ;; OKAY: This is probably the best solution!
    ;;
    ;; We traverse the entire tree nad look for `return', if it does return
    ;; with something else, we simply return true!
    (cl-some (lambda (return-node) (<= 3 (tsc-count-children return-node)))
             (ts-docstr-find-children-traverse node-sb "return_statement"))))

;;;###autoload
(defun ts-docstr-lua-parse (node)
  "Parse declaration for Lua."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "formal_parameters")))
        (let (types variables)
          (dolist (param params)
            (tsc-traverse-mapc
             (lambda (node)
               (pcase (ts-docstr-2-str (tsc-node-type node))
                 ("identifier"
                  (ts-docstr-push ts-docstr-default-typename types)
                  (ts-docstr-push (tsc-node-text node) variables))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-lua--parse-return params)
                :name (ts-docstr-lua--get-name node)))
      (list :name (ts-docstr-lua--get-name node)))))

(defun ts-docstr-lua-config ()
  "Configure style according to variable `ts-docstr-lua-style'."
  (ts-docstr-with-style-case
    (luadoc (list :start "--- "
                  :prefix "-- "
                  :end ""
                  :summary "{d}"
                  :param "@param {{t}} {v} - {d}"
                  :return "@return {d}"))
    (doxygen (list :start "-- "
                   :prefix "-- "
                   :end ""
                   :summary "{d}"
                   :param "@param {{t}} {v} {d}"
                   :return "@return {d}"))
    (scriptum (list :start "--[["
                    :prefix ""
                    :end "]]"
                    :summary "{d}"
                    :param "@param {{t}} {v} {d}"
                    :return "@return {d}"))
    (t (list :start ts-docstr-lua-start
             :prefix ts-docstr-lua-prefix
             :end ts-docstr-lua-end
             :summary ts-docstr-lua-format-summary
             :param ts-docstr-lua-format-param
             :return ts-docstr-lua-format-return))))

;;;###autoload
(defun ts-docstr-lua-insert (node data)
  "Insert document string upon NODE and DATA."
  )

(provide 'ts-docstr-lua)
;;; ts-docstr-lua.el ends here
