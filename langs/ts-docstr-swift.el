;;; ts-docstr-swift.el --- Document string for Swift  -*- lexical-binding: t; -*-

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
;; Document string for Swift.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-swift-style 'header-doc
  "Style specification for document string in Swift."
  :type '(choice (const :tag "No specify" nil)
                 (const :tag "SwiftDoc" swift-doc)
                 (const :tag "HeaderDoc" header-doc))
  :group 'ts-docstr)

(defcustom ts-docstr-swift-start "/**"
  "Docstring start line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-swift-prefix "* "
  "Docstring prefix for each line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-swift-end "*/"
  "Docstring end line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-swift-format-summary "{d}"
  "Format for summary line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-swift-format-param "@param {v} {d}"
  "Format for parameter line."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-swift-format-return "@return {d}"
  "Format for return line."
  :type 'string
  :group 'ts-docstr)

;;;###autoload
(defun ts-docstr-swift-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c-like-narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_declaration
                                                   struct_declaration
                                                   function_declaration))))
      (cond ((zerop (length nodes))
             (ts-docstr-log "No declaration found"))
            ((<= 2 (length nodes))
             (ts-docstr-log "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is generally not necessary but kinda useful for user.
(defun ts-docstr-swift--get-name (node)
  "Return declaration name, class/struct/enum/function."
  (let* ((nodes-name (or (ts-docstr-find-children node "identifier")
                         (ts-docstr-find-children node "type_identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-swift--parse-return (nodes-p)
  "Return t if function does have return value."
  (let* ((node-p (nth 0 nodes-p))
         (node-block (ts-docstr-get-next-sibling node-p "block"))
         (node-block-prev (tsc-get-prev-sibling node-block)))
    (or
     ;; If specified at the end.
     (not (equal (tsc-node-type node-block-prev) 'parameter_list))
     ;; OKAY: This is probably the best solution!
     ;;
     ;; We traverse the entire tree nad look for `return', if it does return
     ;; with something else, we simply return true!
     (cl-some (lambda (return-node) (<= 2 (tsc-count-children return-node)))
              (ts-docstr-find-children-traverse node-block "return_statement")))))

;;;###autoload
(defun ts-docstr-swift-parse (node)
  "Parse declaration for Swift."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "parameter_list")))
        (let (types variables)
          (dolist (param params)  ; loop through each parameter declaration
            (tsc-mapc-children
             (lambda (node)
               (when (eq (tsc-node-type node) 'parameter_declaration)
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
                ;; TODO: There are bugs in upstream, the parsed tree appears
                ;; ERROR nodes; hence we couldn't continue and parse the return
                ;; node in the parsed tree.
                ;;
                ;;                                             Date: 2020-09-25
                ;;
                ;;:return (ts-docstr-swift--parse-return params)
                :name (ts-docstr-swift--get-name node)))
      (list :name (ts-docstr-swift--get-name node)))))

(defun ts-docstr-swift-config ()
  "Configure style according to variable `ts-docstr-swift-style'."
  (ts-docstr-with-style-case
    (swift-doc (list :start ""
                     :prefix "/// "
                     :end ""
                     :summary "{d}"
                     :param "- {v}: {d}"
                     :return ""))
    (header-doc (list :start "/**"
                      :prefix "* "
                      :end "*/"
                      :summary "{d}"
                      :param "@param {v} {d}"
                      :return "@return {d}"))
    (t (list :start ts-docstr-swift-start
             :prefix ts-docstr-swift-prefix
             :end ts-docstr-swift-end
             :summary ts-docstr-swift-format-summary
             :param ts-docstr-swift-format-param
             :return ts-docstr-swift-format-return))))

;;;###autoload
(defun ts-docstr-swift-insert (node data)
  "Insert document string upon NODE and DATA."
  (ts-docstr-with-insert-indent
    (cl-case (tsc-node-type node)
      (function_declaration
       (when-let* ((types (plist-get data :type))
                   (variables (plist-get data :variable))
                   (len (length variables)))
         (ts-docstr-with-style-case
           (swift-doc
            (ts-docstr-insert c-start "\n")
            (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
            (setq restore-point (1- (point)))
            (ts-docstr-insert c-prefix "\n")
            (dotimes (index len)
              (ts-docstr-insert c-prefix
                                (ts-docstr-format 'param
                                                  :typename (nth index types)
                                                  :variable (nth index variables))
                                (if (= index (1- len)) "" "\n")))
            (when (plist-get data :return)
              (ts-docstr-insert c-prefix (ts-docstr-format 'return) "\n"))
            (ts-docstr-insert c-end))
           (header-doc
            (ts-docstr-insert c-start "\n")
            (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
            (setq restore-point (1- (point)))
            (ts-docstr-insert c-prefix "\n")
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
         (swift-doc
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary)))
         (header-doc
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary) "\n")
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-swift)
;;; ts-docstr-swift.el ends here
