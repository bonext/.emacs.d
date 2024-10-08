;; -*- lexical-binding: t; -*-

;; rainbow delims
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; slime
(straight-register-package 'slime)
(if (file-exists-p "/usr/bin/sbcl")
    (use-package slime
      :straight t
      :config
      (setq inferior-lisp-program "/usr/bin/sbcl")))

;; paredit
(use-package paredit
  :straight t
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; ElDoc support
;; (this shows fn arguments in echo)
(use-package eldoc
  :straight t)

;; dts-mode
(use-package dts-mode
  :straight t
  ;; setup for zmk keymaps
  :mode "\\.keymap\\'")

;; nix
;; $PATH
(setenv "PATH" (concat "~/.nix-profile/bin:/nix/var/nix/profiles/default/bin:" (getenv "PATH")))
;; nix-mode
(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

;; markdown-mode
(use-package markdown-mode
  :straight t)

;; zig-mode
(use-package zig-mode
  :straight t
  :mode "\\.zig\\'")

;; direnv-mode
(use-package direnv
  :straight t
  :config
  (direnv-mode))
                                        ; tree-sitter

;; install grammars to ~/.emacs.d/tree-sitter
;; install with `M-x treesit-install-language-grammar`
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")))

;; TODO: when is this needed?
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

                                        ; eglot

;; the following assumes that language-servers are in $PATH
;; eglot is opt-in (M-x eglot or whatever)
(with-eval-after-load 'eglot
  (dolist (mode '((nix-mode . ("nixd"))
                  (zig-mode . ("zls"))))
    (add-to-list 'eglot-server-programs mode)))
