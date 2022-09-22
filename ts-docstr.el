;;; ts-docstr.el --- A document string minor mode using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/ts-docstr
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (tree-sitter "0.15.1") (s "1.9.0") (list-utils "0.4.6"))
;; Keywords: convenience

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
;; Add document string using tree-sitter.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'list-utils)
(require 's)
(require 'tree-sitter)

(defgroup ts-docstr nil
  "A document string minor mode using tree-sitter."
  :group 'tree-sitter
  :prefix "ts-docstr-")

(defcustom ts-docstr-desc-summary "[summary]"
  "Placeholder string for summary description."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-desc-param "[description]"
  "Placeholder string for parameter description."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-desc-return "[description]"
  "Placeholder string for return description."
  :type 'string
  :group 'ts-docstr)

(defcustom ts-docstr-default-typename "[type]"
  "Placeholder string for unknown type description."
  :type 'string
  :group 'ts-docstr)

;;
;; (@* "Entry" )
;;

(defun ts-docstr--enable ()
  "Enable `ts-docstr' in current buffer."
  )

(defun ts-docstr--disable ()
  "Disable `ts-docstr' in current buffer."
  )

;;;###autoload
(define-minor-mode ts-docstr-mode
  "Minor mode `ts-docstr-mode'."
  :lighter " DocStr"
  :group ts-docstr
  (if ts-docstr-mode (ts-docstr--enable) (ts-docstr--disable)))

(defun ts-docstr--turn-on-docstr-mode ()
  "Turn on the `ts-docstr-mode'."
  (ts-docstr-mode 1))

;;;###autoload
(define-globalized-minor-mode global-ts-docstr-mode
  ts-docstr-mode ts-docstr--turn-on-docstr-mode
  :require 'ts-docstr)

;;
;; (@* "Util" )
;;

(defun ts-docstr-2-str (object)
  "Convert OBJECT to string."
  (format "%s" object))

(defmacro ts-docstr-push (newelt seq)
  "Push NEWELT to the ahead or back of SEQ."
  `(if (zerop (length ,seq))
       (push ,newelt ,seq)
     (list-utils-insert-after-pos ,seq (min (1- (length ,seq)) 0) ,newelt)))

(defmacro ts-docstr--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enabled in the current buffer")))

(defun ts-docstr-leaf (node)
  "Return t if NODE is leaf node."
  (zerop (tsc-count-children node)))

(defun ts-docstr-grab-nodes (nodes &optional node)
  "Grab a list NODES from current buffer.

You can pass in NODE to start the captures from that node; default will use the
node from the root."
  (when-let* ((node (or node (tsc-root-node tree-sitter-tree)))
              (patterns (seq-mapcat (lambda (type) `(,(list type) @name)) nodes 'vector))
              (query (ignore-errors
                       (tsc-make-query tree-sitter-language patterns)))
              (found-nodes (tsc-query-captures query node #'ignore)))
    (mapcar #'cdr found-nodes)))

(defun ts-docstr-grab-nodes-in-range (nodes &optional beg end)
  "Grab a list of NODES in range from BEG to END."
  (when-let ((beg (or beg (point-min))) (end (or end (point-max)))
             (nodes (ts-docstr-grab-nodes nodes)))
    (cl-remove-if-not (lambda (node)
                        (and (<= beg (tsc-node-start-position node))
                             (>= end (tsc-node-end-position node))))
                      nodes)))

;;
;; (@* "Core" )
;;

(defcustom ts-docstr-module-alist
  `((c-mode          . ts-docstr-c)
    (c++-mode        . ts-docstr-c++)
    (csharp-mode     . ts-docstr-csharp)
    (go-mode         . ts-docstr-go)
    (java-mode       . ts-docstr-java)
    (javascript-mode . ts-docstr-js)
    (js-mode         . ts-docstr-js)
    (js2-mode        . ts-docstr-js)
    (js3-mode        . ts-docstr-js)
    (php-mode        . ts-docstr-php)
    (python-mode     . ts-docstr-python)
    (ruby-mode       . ts-docstr-ruby)
    (rust-mode       . ts-docstr-rust)
    (scala-mode      . ts-docstr-scala)
    (swift-mode      . ts-docstr-swift)
    (typescript-mode . ts-docstr-typescript))
  "Parsers alist."
  :type '(alist key-type symbol)
  :group 'ts-docstr)

(defun ts-docstr--require-module ()
  "Try to require module."
  (when-let ((module (asoc-get ts-docstr-module-alist major-mode)))
    (require module nil t)))

;;;###autoload
(defun ts-docstr-at-point ()
  "Add document string at point."
  (interactive)
  (ts-docstr--ensure-ts

    ))

(provide 'ts-docstr)
;;; ts-docstr.el ends here
