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
  :config
  ;; setup for zmk keymaps
  (add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode)))

;; nix-mode
(use-package nix-ts-mode
  :straight t)

;; markdown-mode
(use-package markdown-mode
  :straight t)

                                        ; tree-sitter

;; install grammars to ~/.emacs.d/tree-sitter
;; install with `M-x treesit-install-language-grammar`
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (nix "https://github.com/nix-community/tree-sitter-nix")))

;; TODO: when is this needed?
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))