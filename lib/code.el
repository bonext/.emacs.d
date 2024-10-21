;; -*- lexical-binding: t; -*-

;; rainbow delims
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; slime
(if (file-exists-p "/usr/bin/sbcl")
    (use-package slime
      :config
      (setq inferior-lisp-program "/usr/bin/sbcl")))

;; paredit
(use-package paredit
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

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

(aa/with-leader
  :states 'normal
  :keymaps 'override
  "e" #'eglot)
