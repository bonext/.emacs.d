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
;; default fonts
(defun aa/set-default-fonts ()
  (cond
   ;; osx-specific setup
   (t (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100))))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f) (with-selected-frame f (aa/set-default-fonts))))
  (aa/set-default-fonts))

;; remove window titlebar
(if (aa/home-p)
    (add-to-list 'default-frame-alist '(undecorated . t)))

;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setopt ring-bell-function 'ignore)
(setopt visible-bell t)

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
;; recommemded with the above set up
(minibuffer-depth-indicate-mode)

;; hide commands in M-x that do not apply in current mode
;; if no completion-predicate is specified for command then
;; predicate is true when command is applicable to major or any of minor modes
;; setopt is code way to set customized variables
;; cf. https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html#index-setopt
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; customize to file
(setopt custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror 'nomessage)

;; backup files
(setopt backup-by-copying t)
(unless backup-directory-alist
  (setopt backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))

;; enable recent files list
(recentf-mode 1)

;; save cursor location on exit
(save-place-mode 1)

;; prompt y/n stuff in emacs rather than in dialogue boxes
(setopt use-dialog-box nil)

;; autorevert buffers for changed files
(global-auto-revert-mode 1)
(setopt global-auto-revert-non-file-buffers t)


                                        ; encryption
(require 'epa-file)
(setopt epg-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)


                                        ; PACKAGES
(require 'aa/use-package-presets)

;; vim keybindings
(require 'aa/evil-presets)

;; dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  :custom
  ;; NOTE: these require GNU ls
  (dired-listing-switches "-agho --group-directories-first"))

(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-x-hands-off-my-keys nil)
  ;; dired cd with 'a' to reuse buffer
  ;; NOTE: this kills existing dired buffer so current directory is lost in dired
  (put 'dired-find-alternate-file 'disabled nil))

;; ui tweaks
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; highlight cursor
(setq aa/pulsar-pulse-after
      '(evil-goto-line
        evil-window-left
        evil-window-right
        evil-window-up
        evil-window-down
        other-window
        next-multiframe-window))

(use-package pulsar
  ;; load after evil due to advice-add
  :after evil
  :commands pulsar-pulse-line
  :config
  (pulsar-global-mode 1)
  (dolist (f aa/pulsar-pulse-after)
    (advice-add f :after #'(lambda (&rest args) (pulsar-pulse-line)))))

;; show current key in the modeline
(use-package keycast
  :init (keycast-header-line-mode))

(require 'aa/ui-colors)

(require 'aa/completion-presets)

;; completing-read
(use-package consult
  :commands (consult-line
             consult-ripgrep
             consult-outline
             consult-buffer))

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

(use-package 0x0)

(use-package eev
  :init
  (require 'eev-load)
  (eev-mode 1))

;; TODO: consider beam for extra project support
;; https://github.com/rpav/beam.el

;; preload org-mode
(with-temp-buffer (org-mode))

;; setup all keymaps in one place
(require 'aa/keymaps)
