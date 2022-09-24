;;; ts-docstr-rust.el --- Document string for Rust  -*- lexical-binding: t; -*-

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
;; Document string for Rust.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-rust-style 'rfc-430
  "Style specification for document string in Rust."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "RFC 430 documentation conventions" rfc-430))
  :group 'ts-docstr)

(defcustom ts-docstr-rust-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-format-param "* `{v}` - {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-format-return ""
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-rust-header-argument "# Arguments"
  "Header for arguments."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-rust-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(trait_item
                                                   impl_item
                                                   struct_item
                                                   function_item
                                                   function_signature_item))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-rust--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "identifier")
                         (ts-docstr-find-children node "type_identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-rust--parse-return (nodes-p)
  "Return t if function does have return value."
  (let* ((node-p (nth 0 nodes-p))
         (node-block (ts-docstr-get-next-sibling node-p "block"))
         (node-block-prev (tsc-get-prev-sibling node-block)))
    (or
     ;; If specified at the end.
     (not (equal (tsc-node-type node-block-prev) 'parameters))
     ;; OKAY: This is probably the best solution!
     ;;
     ;; We traverse the entire tree nad look for `return', if it does return
     ;; with something else, we simply return true!
     (cl-some (lambda (return-node) (<= 2 (tsc-count-children return-node)))
              (ts-docstr-find-children-traverse node-block "return_expression")))))

;;;###autoload
(defun ts-docstr-rust-parse (node)
  "Parse declaration for Rust."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "parameters")))
        (let (types variables)
          (dolist (param params)  ; loop through each parameter declaration
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter)
                 (tsc-mapc-children
                  (lambda (child)
                    (pcase (ts-docstr-2-str (tsc-node-type child))
                      (":" )  ; do nothing! skip it!
                      ("identifier"
                       (ts-docstr-push (tsc-node-text child) variables))
                      (_
                       (ts-docstr-push (tsc-node-text child) types))))
                  node)))
             param)
            ;; Make sure the typenames and variables have the same length
            (while (not (= (length types) (length variables)))
              ;; Add until they have the same length
              (if (< (length types) (length variables))
                  (ts-docstr-push ts-docstr-default-typename types)
                (ts-docstr-push ts-docstr-default-variable variables))))
          (list :type types :variable variables
                :return (ts-docstr-rust--parse-return params)
                :name (ts-docstr-rust--get-name node)))
      (list :name (ts-docstr-rust--get-name node)))))

(defun ts-docstr-rust-config ()
  "Configure style according to variable `ts-docstr-rust-style'."
  (cl-case ts-docstr-rust-style
    (rfc-430 (list :start ""
                   :prefix "/// "
                   :end ""
                   :summary "{d}"
                   :param "* `{v}` - {d}"
                   :return ""
                   :header-arg "# Arguments"))
    (t (list :start ts-docstr-rust-start
             :prefix ts-docstr-rust-prefix
             :end ts-docstr-rust-end
             :summary ts-docstr-rust-format-summary
             :param ts-docstr-rust-format-param
             :return ts-docstr-rust-format-return
             :header-arg ts-docstr-rust-header-argument))))

;;;###autoload
(defun ts-docstr-rust-insert (_node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-inserting
    (let ((types (plist-get data :type)))
      (when-let* ((variables (plist-get data :variable))
                  (len (length variables)))
        (ts-docstr-insert c-start "\n")
        (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
        (setq restore-point (1- (point)))
        (ts-docstr-insert c-prefix "\n")
        (ts-docstr-insert c-prefix (plist-get config :header-arg) "\n")
        (ts-docstr-insert c-prefix "\n")
        (dotimes (index len)
          (ts-docstr-insert c-prefix
                            (ts-docstr-format 'param
                                              :typename (nth index types)
                                              :variable (nth index variables))
                            (if (= index (1- len)) "" "\n")))
        (ts-docstr-insert c-end)))))

(provide 'ts-docstr-rust)
;;; ts-docstr-rust.el ends here
