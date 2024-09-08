                                        ; BUILT-INS
;; UI
;; start to scratch
(setq inhibit-startup-message t)
;; disable tool bar (it causes glitches on wayland)
(tool-bar-mode -1)
;; disable menu
(menu-bar-mode -1)
;; disable scroll bars
(scroll-bar-mode -1)
;; TODO: research
(set-fringe-mode 10)
;; TODO: research
(tooltip-mode -1)
;; smoother scrolling
(pixel-scroll-precision-mode)
;; default font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)


;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; disable line numbers for certain modes
(dolist (mode '(eshell-mode-hook
                org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
;; NOTE: this kills existing dired buffer so current directory is lost in dired
(put 'dired-find-alternate-file 'disabled nil)

;; encryption
(require 'epa-file)
(setq epa-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)

;; tree-sitter
;; install grammars to ~/.emacs.d/tree-sitter
;; install with `M-x treesit-install-language-grammar`
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (nix "https://github.com/nix-community/tree-sitter-nix")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))

                                        ; PACKAGES
(require 'package)
;; MELPA
;; https://melpa.org/
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t) 
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; all-the-icons
;; NOTE: messes up with ~/.local/share/fonts
;; requires `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :if (display-graphic-p))

;; doom-modeline
;; doom-modeline relies on nerd-icons
;; NOTE: messes up with ~/.local/share/fonts
;; requires `M-x nerd-icons-install-fonts`
(use-package nerd-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; diminish hides modes from modeline
;; requires for `:diminish` keyword in use-package
(use-package diminish)

;; show keys and combinations
;; TODO: enable global and toggle bufffer
(use-package command-log-mode)

;; ivy completion engine
;; counsel also pulls in ivy and swiper
;; cf. https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/
(use-package ivy
  :diminish  ;; do not show in modeline
  :defer 0.1 ;; load after .1s idle emacs (otherwise waits)
  :bind (;; this causes ivy to delay loading (probably)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file))
  :config (counsel-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; rainbow delims
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
;; shows help on key prefix after `which-key-idle-delay` seconds
;; will be in emacs-30
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; helpful cfg directly out of emacs-from-scratch
;; TODO: research
(use-package helpful
  :diminish
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; TODO: research general.el
;; tldr: key definer for easy leader prefix
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#bindings-with-generalel

;; TODO: research hydra
;; tldr: transient keybindings
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra

(load (concat user-emacs-directory "lib/wayland.el"))
(load (concat user-emacs-directory "lib/colors.el"))
(load (concat user-emacs-directory "lib/evil.el"))
(load (concat user-emacs-directory "lib/org.el"))
(load (concat user-emacs-directory "lib/org-roam.el"))

;; company
(use-package company
  :config
  (setq company-minimum-prefix-length 3)
  (add-hook 'after-init-hook 'global-company-mode))

;; slime
(if (file-exists-p "/usr/bin/sbcl")
    (progn
      (use-package slime
        :config
        (setq inferior-lisp-program "/usr/bin/sbcl"))))

;; paredit
(use-package paredit
  :config
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; geiser
;;; racket
(if (file-exists-p "/usr/bin/racket")
    (use-package geiser-racket
      :config
      (setq geiser-racket-binary "/usr/bin/racket")))

;; ElDoc support
;; (this shows fn arguments in echo)
(require 'eldoc)

;; dts-mode
(use-package dts-mode
  :config
  ;; setup for zmk keymaps
  (add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode)))

;; nix-mode
(use-package nix-ts-mode)

;; markdown-mode
(use-package markdown-mode)

;; lua-mode
(use-package lua-mode)
