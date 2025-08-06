;; -*- lexical-binding: t; -*-
(require 'aa/use-package-presets)

;; colors
(use-package eterm-256color
  :hook
  (term-mode-hook . eterm-256color-mode)
  (vterm-mode-hook . eterm-256color-mode))

;; vterm
(use-package vterm
  :commands vterm
  :config
  (cond
   ;; osx-specific setup
   (t (setq vterm-shell "bash")))
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))

(provide 'aa/term-presets)
