;; -*- lexical-binding: t; -*-
(require 'aa/use-package-presets)
(require 'aa/leader)

;; colors
(use-package eterm-256color
  :hook
  (term-mode-hook . eterm-256color-mode)
  (vterm-mode-hook . eterm-256color-mode))

;; vterm
(use-package vterm
  :config
  (cond
   ;; osx-specific setup
   (t (setq vterm-shell "/usr/bin/bash")))
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t
        vterm-buffer-name-string "vterm %s"))

(aa/with-leader
  :states 'normal
  :keymaps 'override
  "t" '(:ignore t :which-key "terminal")
  "tt" #'vterm)

(provide 'aa/term-presets)
