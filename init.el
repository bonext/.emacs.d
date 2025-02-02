; -*- lexical-binding: t; -*-
                                        ; host detection

(setq aa/host
      (cond
       ((and (eq system-type 'gnu/linux)
             (string-match "-[Mm]icrosoft" operating-system-release))
        'wsl)
       ((eq system-type 'darwin) 'work)
       (t 'home)))

                                        ; UI

;; wayland support (mostly cross-app clipboard)
(load (concat user-emacs-directory "lib/wayland.el"))

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
(cond
 ;; osx-specific setup
 (t (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100)))


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

;; minibuffers inside minibuffers
(setq enable-recursive-minibuffers t)

;; hide commands in M-x that do not apply in current mode
;; if no completion-predicate is specified for command then
;; predicate is true when command is applicable to major or any of minor modes
;; setopt is code way to set customized variables
;; cf. https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setopt
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; customize to file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; backup files
(setopt backup-by-copying t)
(unless backup-directory-alist
  (setopt backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))
                                        ; Dired
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

                                        ; encryption
(require 'epa-file)
(setq epa-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)


                                        ; PACKAGES
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

                                        ; keys

;; evil-mode
(load (concat user-emacs-directory "lib/evil.el"))

;; general
(use-package general)

;; show current key in the modeline
(use-package keycast
  :init (keycast-header-line-mode))

;; could not fit these into any of use-package keywords
(general-auto-unbind-keys)
(general-create-definer aa/with-leader
  :prefix "SPC")

(general-create-definer aa/with-insert-leader
  :prefix "C-c")

;; color scheme
;; hook to be called on theme reload
(defvar aa/after-load-theme-hook nil
  "Hook run after a color theme is loaded with `load-theme`.")
(defadvice load-theme (after aa/run-after-load-theme-hook activate)
  "Run `aa/after-load-theme-hook`."
  (run-hooks 'aa/after-load-theme-hook))
(load (concat user-emacs-directory "lib/colors.el"))

;; ;; all-the-icons
;; ;; NOTE: messes up with ~/.local/share/fonts
;; ;; requires `M-x all-the-icons-install-fonts`
;; (use-package all-the-icons
;;   :if (display-graphic-p))

;; ;; doom-modeline
;; ;; doom-modeline relies on nerd-icons
;; ;; NOTE: messes up with ~/.local/share/fonts
;; ;; requires `M-x nerd-icons-install-fonts`
;; (use-package nerd-icons)
;; (use-package doom-modeline)

;; diminish hides modes from modeline
;; requires for `:diminish` keyword in use-package
(use-package diminish) 

                                        ; COMPLETIONS

(load (concat user-emacs-directory "lib/completions.el"))


;; which-key
;; shows help on key prefix after `which-key-idle-delay` seconds
;; will be in emacs-30
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ;; helpful cfg directly out of emacs-from-scratch
;; ;; TODO: research
;; (use-package helpful
;;   :diminish
;;   :after counsel
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

;; ;; TODO: research hydra
;; ;; tldr: transient keybindings
;; ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra

                                        ; ORG

(load (concat user-emacs-directory "lib/org.el"))
(load (concat user-emacs-directory "lib/org-roam.el"))

                                        ; CODE

(load (concat user-emacs-directory "lib/code.el"))


                                        ; terminal emulation

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

                                        ; package management

(defun aa/recompile-all-packages nil
  (interactive)
  (message "recompiling elpa/ contents")
  (native-compile-async "~/.emacs.d/elpa/" t))

(aa/with-leader
  :states 'normal
  :keymaps 'override
  "p" '(:ignore t :which-key "packages")
  "pl" #'list-packages
  "pr" #'aa/recompile-all-packages)


                                        ; unsorted goodies

;; switch to other window even in a different frame
(aa/with-leader
  :states 'normal
  :keymaps 'override
  "w" '(:ignore t :which-key "window")
  "wo" #'next-multiframe-window)
