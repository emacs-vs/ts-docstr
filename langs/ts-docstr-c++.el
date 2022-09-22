;;; ts-docstr-c++.el --- Document string for C++  -*- lexical-binding: t; -*-

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
;; Document string for C++.
;;

;;; Code:

(require 'ts-docstr)

(defcustom ts-docstr-c++-style nil
  "Style specification for document string in C."
  :type '(choice (const :tag "No specify" nil))
  :group 'docstr)

(defcustom ts-docstr-c++-format "{v} {d}"
  ""
  :type 'string
  :group 'docstr)

(defmacro ts-docstr-c++--narrow-region (&rest body)
  "Narrow region to class/struct/function declaration."
  (declare (indent 0))
  `(save-restriction
     (narrow-to-region (line-beginning-position) (line-end-position 2))
     ,@body))

;;;###autoload
(defun ts-docstr-c++-activate ()
  "Return t if we are able to add document string at this point."
  (ts-docstr-c++--narrow-region
    (let* ((nodes (ts-docstr-grab-nodes-in-range '(class_specifier
                                                   struct_specifier
                                                   enum_specifier
                                                   function_declarator))))
      (cond ((zerop (length nodes))
             (user-error "No declaration found"))
            ((<= 2 (length nodes))
             (user-error "Multiple declarations are invalid, %s" (length nodes)))
            (t (nth 0 nodes))))))

;; NOTE: This is only used in function declaration!
(defun ts-docstr-c++--parse-return ()
  "Return t if function does have return value."
  (let* ((nodes-fd (ts-docstr-grab-nodes-in-range '(function_declarator)))
         (node-fd (nth 0 nodes-fd))
         (parent (tsc-get-parent node-fd))
         (return t))
    (tsc-mapc-children
     (lambda (node)
       (when (ts-docstr-leaf-p node)
         (when (string= (tsc-node-text node) "void")
           (setq return nil))))
     parent)
    return))

;;;###autoload
(defun ts-docstr-c++-parse ()
  "Parse declaration for C++."
  (ts-docstr-c++--narrow-region
    (when-let* ((params (ts-docstr-grab-nodes-in-range '(parameter_list)))
                (param (nth 0 params)))
      (if (<= 2 (length params))
          (user-error "Found multiple parameter_list, %s" (length params))
        (let (types variables)
          (tsc-traverse-mapc
           (lambda (node)
             (when (ts-docstr-leaf-p node)
               (pcase (ts-docstr-2-str (tsc-node-type node))
                 ((or "primitive_type" "type_identifier")
                  (ts-docstr-push (tsc-node-text node) types))
                 ("identifier"
                  (ts-docstr-push (tsc-node-text node) variables))
                 ((or "*" "&" "[" "]")
                  (if (null types)
                      (ts-docstr-push (tsc-node-text node) types)
                    (let ((last (1- (length types))))
                      (setf (nth last types)
                            (concat (nth last types) (tsc-node-text node)))))))))
           param)
          `(:type ,types :variables ,variables :return ,(ts-docstr-c++--parse-return)))))))

;;;###autoload
(defun ts-docstr-c++-insert (node data)
  "Insert document string upon NODE and DATA."
  (message "node; %s" node)
  (message "data; %s" data)
  )

(provide 'ts-docstr-c++)
;;; ts-docstr-c++.el ends here
