;; -*- lexical-binding: t; -*-
(with-eval-after-load 'package
  (progn
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize)))

(require 'use-package)
(setq use-package-always-ensure t)

;; diminish hides modes from modeline
;; requires for `:diminish` keyword in use-package
(use-package diminish) 
(provide 'aa/use-package-presets)
