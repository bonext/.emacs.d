; -*- lexical-binding: t; -*-
(add-to-list 'load-path
             (file-name-concat user-emacs-directory "lib"))

(require 'aa/detect-host)
                                        ; UI

;; start to scratch
(setopt inhibit-startup-message t)
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
;; default font
(cond
 ;; osx-specific setup
 (t (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100)))
;; remove window titlebar
(add-to-list 'default-frame-alist '(undecorated . t))

;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setopt ring-bell-function 'ignore)
(setopt visible-bell t)

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
(setopt default-input-method "russian-computer")

;; minibuffers inside minibuffers
(setopt enable-recursive-minibuffers t)

;; hide commands in M-x that do not apply in current mode
;; if no completion-predicate is specified for command then
;; predicate is true when command is applicable to major or any of minor modes
;; setopt is code way to set customized variables
;; cf. https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setopt
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; customize to file
(setopt custom-file (concat user-emacs-directory "custom.el"))
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
(setopt epg-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)


                                        ; PACKAGES
(require 'aa/use-package-presets)

;; keybindings
(require 'aa/evil-presets)
(require 'aa/leader)
(require 'aa/wk-presets)

;; ui tweaks
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; highlight cursor
(use-package pulsar
  :config
  (pulsar-global-mode 1)
  :general
  (aa/with-leader
    :states 'normal
    :keymaps 'override
    "SPC" #'pulsar-pulse-line))


(aa/with-leader
  :states 'normal
  :keymaps 'override
  "e" #'eglot)

;; show current key in the modeline
(use-package keycast
  :init (keycast-header-line-mode))

(require 'aa/ui-colors)

(require 'aa/completion-presets)

;; richer help
(use-package helpful
  :diminish
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(require 'aa/org-presets)

(require 'aa/coding-presets)

(require 'aa/term-presets)

(require 'aa/shortcuts-unsorted)

;; TODO: research hydra
;; tldr: transient keybindings
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra

;; TODO: consider beam for extra project support
;; https://github.com/rpav/beam.el
