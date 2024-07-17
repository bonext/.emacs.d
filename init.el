                                        ; BUILT-INS
;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; disable tool bar (it causes glitches on wayland)
(tool-bar-mode -1)
;; disable menu
(menu-bar-mode -1)
;; disable scroll bars
(scroll-bar-mode -1)

;; show lines in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; startup to scratch
;; (setq inhibit-startup-screen t)

;; add russian as C-\ bind
(setq default-input-method "russian-computer")

;; customize to file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Dired
(with-eval-after-load 'dired
  (require 'dired-x))
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-x-hands-off-my-keys nil)
  
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)))
            
;; dired cd with 'a' to reuse buffer
(put 'dired-find-alternate-file 'disabled nil)

;; smoother scrolling
(pixel-scroll-precision-mode)

;; encryption
(require 'epa-file)
(setq epa-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)

;; tree-sitter
;; install grammars to ~/.emacs.d/tree-sitter
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (nix "https://github.com/nix-community/tree-sitter-nix")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

                                        ; PACKAGES
                                        ; auto packages
(require 'package)
;; MELPA
;; https://melpa.org/
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t) 
(package-initialize)

(load (concat user-emacs-directory "lib/colors.el"))
(load (concat user-emacs-directory "lib/org.el"))
(load (concat user-emacs-directory "lib/wayland.el"))

;; company
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 3)
  (add-hook 'after-init-hook 'global-company-mode))

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-default-state 'emacs)
  :config
  ;; disable evil-mode in some buffers (by their name, cf. C-xC-b)
  (add-to-list 'evil-buffer-regexps '("^\\*Geiser.*REPL\\*$"))
  (add-to-list 'evil-buffer-regexps '("^\\*slime-repl.*\\*$"))
  (evil-mode 1))
  

;; evil-org
(use-package evil-org
  :ensure t
  :after org
  :hook org-mode-hook
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

;; slime
(if (file-exists-p "/usr/bin/sbcl")
    (progn
      (use-package slime
        :ensure t
        :config
        (setq inferior-lisp-program "/usr/bin/sbcl"))))

;; paredit
(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; geiser
;;; racket
(if (file-exists-p "/usr/bin/racket")
    (use-package geiser-racket
      :ensure t
      :config
      (setq geiser-racket-binary "/usr/bin/racket")))

;; ElDoc support
;; (this shows fn arguments in echo)
(require 'eldoc)

;; dts-mode
(use-package dts-mode
  :ensure t
  :config
  ;; setup for zmk keymaps
  (add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode)))

;; nix-mode
(use-package nix-ts-mode
  :ensure t)

;; markdown-mode
(use-package markdown-mode
  :ensure t)

;; lua-mode
(use-package lua-mode
  :ensure t)

;; projectile
;; (use-package projectile
;;   :ensure t
;;   :config
;;   ;; Recommended keymap prefix on Windows/Linux
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))
