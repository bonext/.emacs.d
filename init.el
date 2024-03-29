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
(if (file-exists-p "~/.emacs.d/custom.el")
    (progn
      (setq custom-file "~/.emacs.d/custom.el")
      (load-file custom-file)))

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
      '((python "https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


                                        ; ORG-MODE

;;; enable scaling of inline images with attr_org width
(setq org-image-actual-width nil)
;;; show inline images by default
(setq org-startup-with-inline-images t)
;;; capture setup
(setq org-directory "~/Documents/Notes")
(setq org-default-notes-file (concat org-directory "/captured.org"))
;;; suggested shortcut keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

                                        ; PACKAGES

                                        ; manual
;; parinfer-rust-mode
(setq parinfer-rust-auto-download t)
(add-to-list 'load-path "~/.emacs.d/manual-packages/parinfer-rust-mode")
(autoload 'parinfer-rust-mode "parinfer-rust-mode" nil t)
(add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)

;; TODO: add these as parinfer hooks instead?
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

                                        ; auto packages
(require 'package)

;; MELPA
;; set up to stable for pinning
(add-to-list 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/")
 t)
 
(package-initialize)

;; colors
(use-package nord-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)
(use-package base16-theme
  :ensure t)

;; solarized-dark
;; cf. https://github.com/bbatsov/solarized-emacs
;; (load-theme 'solarized-dark t)
;; Don't change size of org-mode headlines (but keep other size-changes)
;; (setq solarized-scale-org-headlines nil

;; nord theme
;; (load-theme 'nord t)

;; modus theme
;; https://protesilaos.com/emacs/modus-themes
;; included in emacs
(load-theme 'modus-vivendi)

;; org-journal
(use-package org-journal
  :ensure t
  :init
 (setq org-journal-file-type 'daily)
 (setq org-journal-dir "~/Documents/journal")
 (setq org-journal-date-format "%Y-%m-%d, %A")
 (setq org-journal-file-format "%F.org") ; yyyy-mm-dd.org
 (setq org-journal-encrypt-journal t)
 :config
 (global-set-key (kbd "C-c j") 'org-journal-new-entry))
  
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
  :ensure t)

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
(use-package nix-mode
  :ensure t)

;; markdown-mode
(use-package markdown-mode
  :ensure t)

;; lua-mode
(use-package lua-mode
  :ensure t)

;; projectile
(use-package projectile
  :ensure t
  :config
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

                                        ; MISC

;; wayland clipboard support in terminal
;; TODO: make this conditional on wayland
;; credit: yorickvP on Github: https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)
