;; -*- lexical-binding: t; -*-
(require 'aa/use-package-presets)
(require 'aa/leader)

;; rainbow delims
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; slime
(if (file-exists-p "/usr/bin/sbcl")
    (use-package slime
      :config
      (setq inferior-lisp-program "/usr/bin/sbcl")))

;; racket
(use-package racket-mode)

;; smartparens
(use-package smartparens
  :hook ((lisp-data-mode racket-mode pollen-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  :custom
  (sp-base-key-bindings 'sp "set smartparens bindings"))

;; ElDoc support
;; (this shows fn arguments in echo)
(use-package eldoc)

;; dts-mode
(use-package dts-mode
  ;; setup for zmk keymaps
  :mode "\\.keymap\\'")

;; nix
;; $PATH
(setenv "PATH" (concat "~/.nix-profile/bin:/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
;; nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")

;; markdown-mode
(use-package markdown-mode)

;; zig-mode
(use-package zig-mode
  :mode "\\.zig\\'")

;; direnv-mode
(use-package direnv
  :config
  (direnv-mode))

(use-package systemd)

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))
;; requires M-x treesit-install-language-grammar for `vim`
;; taken from https://github.com/tree-sitter-grammars/tree-sitter-vim
(use-package vimscript-ts-mode
  :mode "\\vifmrc\\'")

(use-package pollen-mode)

(use-package kdl-mode)
                                        ; tree-sitter

;; install grammars to ~/.emacs.d/tree-sitter
;; install with `M-x treesit-install-language-grammar`
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")))

;; TODO: when is this needed?
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

                                        ; eglot

;; the following assumes that language-servers are in $PATH
;; eglot is opt-in (M-x eglot or whatever)
(with-eval-after-load 'eglot
  (dolist (mode '((nix-mode . ("nixd"))
                  (zig-mode . ("zls"))))
    (add-to-list 'eglot-server-programs mode))
  ;; https://github.com/joaotavora/eglot/discussions/1393
  (setq eglot-ignored-server-capabilites
        (cons :documentOnTypeFormattingProvider
                    eglot-ignored-server-capabilites)))
;; let project.el recognize python project roots
(add-to-list 'project-vc-extra-root-markers "pyproject.toml")

(aa/with-leader
  :states 'normal
  :keymaps 'override
  "e" #'eglot)

(provide 'aa/coding-presets)
