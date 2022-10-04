;;; yuck-mode.el --- Major mode for the yuck configuration language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 mmcjimsey26
;;
;; Author: mmcjimsey26
;; Maintainer: mmcjimsey26
;; Created: June 14, 2022
;; Modified: July 3, 2022
;; Version: 1.0.1
;; Keywords: languages yuck eww widgets
;; Homepage: https://github.com/mmcjimsey26/yuck-mode
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; A simple major mode for editing files in the yuck configuration language, used for
;; configuring ElKowar's Wacky Widgets (eww), usually ending in `.yuck'.

;; This package provides the following features:
;;      * Syntax hilighting

;;; Installation:

;; This mode requires Emacs-25.1 or higher.
;; Put this file into `load-path' and the following into ~/.emacs
;;      (autoload 'yuck-mode "yuck-mode" nil t)

;;; Code:

(defgroup yuck nil
  "Yuck Configuration Major Mode."
  :group 'languages)

(defvar yuck-mode-hook nil "Yuck mode hook.")

;; Keywords and widget types for `yuck-mode'.
;; Byte-compile the code for optimized regexps
(eval-and-compile
  (defvar yuck-keywords-list
     '("defvar" "defpoll" "deflisten" "defwindow" "defwidget" "for" "include"))

  (defvar yuck-widgets-list
    '("combo-box-text"
      "expander" "revealer" "checkbox" "color-button" "color-chooser" "scale" "progress"
      "input" "button" "image" "box" "overlay" "centerbox" "scroll" "eventbox"
      "label" "literal" "calendar" "transform" "circular-progress" "graph" "geometry")))

(eval-and-compile
  (defun yuck-ppre (re)
    (format "\\_<\\(%s\\)\\>" (regexp-opt re))))

(defvar yuck-font-lock-keywords
  (list
   (cons (eval-when-compile
           (format "\:[a-z\-]*"))
         font-lock-builtin-face)
   (cons (eval-when-compile
           (yuck-ppre yuck-keywords-list))
         font-lock-keyword-face)
   (cons (eval-when-compile
           (yuck-ppre yuck-widgets-list))
         font-lock-type-face)))

(defconst yuck-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; "" : string delimiter
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

;;;###autoload
(define-derived-mode yuck-mode prog-mode "Yuck"
  "Major mode for editing yuck config files."
  :syntax-table yuck-mode-syntax-table
  (setq-local font-lock-defaults '(yuck-font-lock-keywords))
  (setq-local comment-start ";; ")
  (setq-local comment-end "")
  (setq-local comment-padding "")
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.yuck\\'" . yuck-mode))

(provide 'yuck-mode)
;;; yuck-mode.el ends here
