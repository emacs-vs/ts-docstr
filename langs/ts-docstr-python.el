;;; ts-docstr-python.el --- Document string for Python  -*- lexical-binding: t; -*-

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
;; Document string for Python.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-python-style 'pep-257
  "Style specification for document string in Python."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "PEP 257 convention" pep-257)
                 (const :tag "Google Style" google)
                 (const :tag "NumPy Style" numpy))
  :group 'ts-docstr)

(defcustom ts-docstr-python-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-format-param "* `{v}` - {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-format-return ""
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-header-argument "Keyword arguments:"
  "Header for arguments."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-python-header-return ""
  "Header for returns."
  :type 'string
  :group 'ts-docstr)

(defmacro ts-docstr-python--narrow-region (&rest body)
  "Narrow region to class/struct/function declaration."
  (declare (indent 0))
  `(save-restriction
     (narrow-to-region (line-beginning-position) (line-end-position))
     ,@body))

;;;###autoload
(defun ts-docstr-python-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-python--narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_definition
                                                   decorated_definition
                                                   function_definition)
                                                 t)))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-find-closest-node nodes))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-python--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "identifier")
                         (ts-docstr-find-children node "type_identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-python--parse-return (nodes-p)
  "Return t if function does have return value."
  (let* ((node-p (nth 0 nodes-p))
         (node-block (ts-docstr-get-next-sibling node-p "block"))
         (node-block-prev-: (tsc-get-prev-sibling node-block))  ; skip colon :
         (node-block-prev (tsc-get-prev-sibling node-block-prev-:)))
    (or
     ;; If specified at the end.
     (not (equal (tsc-node-type node-block-prev) 'parameters))
     ;; OKAY: This is probably the best solution!
     ;;
     ;; We traverse the entire tree nad look for `return', if it does return
     ;; with something else, we simply return true!
     (cl-some (lambda (return-node) (<= 2 (tsc-count-children return-node)))
              (ts-docstr-find-children-traverse node-block "return_statement")))))

;;;###autoload
(defun ts-docstr-python-parse (node)
  "Parse declaration for Python."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "parameters")))
        (let (types variables)
          (dolist (param params)  ; loop through each parameter declaration
            (tsc-mapc-children
             (lambda (node)
               (cond ((memq (tsc-node-type node) '(typed_parameter))
                      (tsc-mapc-children
                       (lambda (child)
                         (pcase (ts-docstr-2-str (tsc-node-type child))
                           (":" )  ; do nothing! skip it!
                           ("identifier"
                            (ts-docstr-push (tsc-node-text child) variables))
                           (_
                            (ts-docstr-push (tsc-node-text child) types))))
                       node))
                     ((and (memq (tsc-node-type node) '(identifier))
                           (not (string= (tsc-node-text node) "self")))
                      (ts-docstr-push (tsc-node-text node) variables)))
               ;; Make sure the typenames and variables have the same length
               (while (not (= (length types) (length variables)))
                 ;; Add until they have the same length
                 (if (< (length types) (length variables))
                     (ts-docstr-push ts-docstr-default-typename types)
                   (ts-docstr-push ts-docstr-default-variable variables))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-python--parse-return params)
                :name (ts-docstr-python--get-name node)))
      (list :name (ts-docstr-python--get-name node)))))

(defun ts-docstr-python-config ()
  "Configure style according to variable `ts-docstr-python-style'."
  (ts-docstr-with-style-case
    (pep-257 (list :start "\"\"\""
                   :prefix ""
                   :end "\"\"\""
                   :summary "{d}"
                   :param "{v} -- {d}"
                   :return ""
                   :header-arg "Keyword arguments:"
                   :header-ret ""))
    (google (list :start "\"\"\""
                  :prefix "  "
                  :end "\"\"\""
                  :summary "{d}"
                  :param "{v} ({t}): {d}"
                  :return "{d}"
                  :header-arg "Args:"
                  :header-ret "Returns:"))
    (numpy (list :start "\"\"\""
                 :prefix ""
                 :end "\"\"\""
                 :summary "{d}"
                 :param "{v} : {t}"
                 :return "{d}"
                 :header-arg "Parameters"
                 :header-ret "Returns"))
    (t (list :start ts-docstr-python-start
             :prefix ts-docstr-python-prefix
             :end ts-docstr-python-end
             :summary ts-docstr-python-format-summary
             :param ts-docstr-python-format-param
             :return ts-docstr-python-format-return
             :header-arg ts-docstr-python-header-argument
             :header-ret ts-docstr-python-header-return))))

;;;###autoload
(defun ts-docstr-python-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent-hold
    (cl-case (tsc-node-type node)
      (function_definition  ; For function
       (let* ((types (plist-get data :type))
              (variables (plist-get data :variable))
              (len (length variables)))
         ;; XXX: Start insert the differences!
         (ts-docstr-with-style-case
           (pep-257
            (let (ts-docstr-indent-spaces)
              (ts-docstr-insert c-start (ts-docstr-format 'summary) "\n"))
            (setq restore-point (1- (point)))
            (ts-docstr-insert c-prefix " " "\n")
            (ts-docstr-insert c-prefix (plist-get config :header-arg) "\n")
            (ts-docstr-insert c-prefix "\n")
            (dotimes (index len)
              (ts-docstr-insert c-prefix
                                (ts-docstr-format 'param
                                                  :typename (nth index types)
                                                  :variable (nth index variables))
                                "\n"))
            (ts-docstr-insert c-end))
           (google
            (let (ts-docstr-indent-spaces)
              (ts-docstr-insert c-start (ts-docstr-format 'summary) "\n"))
            (setq restore-point (1- (point)))
            (ts-docstr-insert " " "\n")
            (ts-docstr-insert (plist-get config :header-arg) "\n")
            (dotimes (index len)
              (ts-docstr-insert c-prefix
                                (ts-docstr-format 'param
                                                  :typename (nth index types)
                                                  :variable (nth index variables))
                                "\n"))
            (when (plist-get data :return)
              (ts-docstr-insert " " "\n")
              (ts-docstr-insert (plist-get config :header-ret) "\n")
              (ts-docstr-insert c-prefix (ts-docstr-format 'return) "\n"))
            (ts-docstr-insert c-end))
           (numpy
            (let (ts-docstr-indent-spaces)
              (ts-docstr-insert c-start (ts-docstr-format 'summary) "\n"))
            (setq restore-point (1- (point)))
            (ts-docstr-insert c-prefix " " "\n")
            (ts-docstr-insert c-prefix (plist-get config :header-arg) "\n")
            (ts-docstr-insert "-------" "\n")
            (ts-docstr-insert c-prefix "\n")
            (dotimes (index len)
              (ts-docstr-insert c-prefix
                                (ts-docstr-format 'param
                                                  :typename (nth index types)
                                                  :variable (nth index variables))
                                "\n")
              (ts-docstr-insert "    " ts-docstr-desc-param "\n"))
            (when (plist-get data :return)
              (ts-docstr-insert " " "\n")
              (ts-docstr-insert (plist-get config :header-ret) "\n")
              (ts-docstr-insert "-------" "\n")
              (ts-docstr-insert c-prefix "    " (ts-docstr-format 'return) "\n"))
            (ts-docstr-insert c-end))
           (t
            (ts-docstr-custom-insertion node data)))))
      ;; For the rest of the type, class/struct/enum
      (t
       (ts-docstr-with-style-case
         ((or pep-257 google numpy)
          (let (ts-docstr-indent-spaces)
            (ts-docstr-insert c-start (ts-docstr-format 'summary) "\n"))
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-python)
;;; ts-docstr-python.el ends here
