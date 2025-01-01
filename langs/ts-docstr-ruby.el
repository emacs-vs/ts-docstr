;;; ts-docstr-ruby.el --- Document string for Ruby  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Shen, Jen-Chieh

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
  (let* ((nodes-name (or (ts-docstr-find-children node "constant")
                         (ts-docstr-find-children node "identifier")))
         (node-name (nth 0 nodes-name)))
    (tsc-node-text node-name)))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-ruby--parse-return (nodes-fp)
  "Return t if function does have return value."
  (when-let*
      ((node-fp (nth 0 nodes-fp))
       (parent (tsc-get-parent node-fp)))
    ;; OKAY: This is probably the best solution!
    ;;
    ;; We traverse the entire tree nad look for `return', if it does return
    ;; with something else, we simply return true!
    (cl-some (lambda (return-node) (<= 2 (tsc-count-children return-node)))
             (ts-docstr-find-children-traverse parent "return"))))

;;;###autoload
(defun ts-docstr-ruby-parse (node)
  "Parse declaration for Ruby."
  (ts-docstr-c-like-narrow-region
    (if-let ((params (ts-docstr-find-children node "method_parameters")))
        (let (types variables)
          (dolist (param params)
            (tsc-mapc-children
             (lambda (node)
               (pcase (ts-docstr-2-str (tsc-node-type node))
                 ("," )  ; do nothing! skip it!
                 ("identifier"
                  (ts-docstr-push (tsc-node-text node) variables))
                 (_
                  (ts-docstr-push (tsc-node-text node) types)))
               ;; Make sure the typenames and variables have the same length
               (while (not (= (length types) (length variables)))
                 ;; Add until they have the same length
                 (if (< (length types) (length variables))
                     (ts-docstr-push ts-docstr-default-typename types)
                   (ts-docstr-push ts-docstr-default-variable variables))))
             param))
          (list :type types :variable variables
                :return (ts-docstr-ruby--parse-return params)
                :name (ts-docstr-ruby--get-name node)))
      (list :name (ts-docstr-ruby--get-name node)))))

(defun ts-docstr-ruby-config ()
  "Configure style according to variable `ts-docstr-ruby-style'."
  (ts-docstr-with-style-case
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
      (method  ; For function
       (let* ((types (plist-get data :type))
              (variables (plist-get data :variable))
              (len (length variables)))
         (ts-docstr-with-style-case
           (rdoc
            (ts-docstr-insert c-start "\n")
            (ts-docstr-insert c-prefix (ts-docstr-format 'summary))
            (setq restore-point (1- (point)))
            (when (or (not (zerop len))
                      (plist-get data :return))
              (insert "\n")
              (ts-docstr-insert c-prefix "\n"))
            (dotimes (index len)
              (ts-docstr-insert c-prefix
                                (ts-docstr-format 'param
                                                  :typename (nth index types)
                                                  :variable (nth index variables))
                                (if (= index (1- len)) "" "\n")))
            (when (plist-get data :return)
              (ts-docstr-insert c-prefix (ts-docstr-format 'return) "\n"))
            (ts-docstr-insert c-end))
           (t
            (ts-docstr-custom-insertion node data)))))
      ;; For the rest of the type, class/struct/enum
      (t
       (ts-docstr-with-style-case
         (rdoc
          (ts-docstr-insert c-start "\n")
          (ts-docstr-insert c-prefix (ts-docstr-format 'summary) (if (string-empty-p c-end) "" "\n"))
          (setq restore-point (1- (point)))
          (ts-docstr-insert c-end))
         (t
          (ts-docstr-custom-insertion node data)))))))

(provide 'ts-docstr-ruby)
;;; ts-docstr-ruby.el ends here
