;; -*- lexical-binding: t; -*-

;; detect host system
(let ((aa/host (cond
                ((and (eq system-type 'gnu/linux)
                      (string-match "-[Mm]icrosoft" operating-system-release))
                 'wsl)
                ((eq system-type 'darwin) 'work)
                (t 'home))))
  (defun aa/home-p ()
    (eq aa/host 'home))
  (defun aa/work-p ()
    (eq aa/host 'work))
  (defun aa/wsl-p ()
    (eq aa/host 'wsl)))

                                        ;
                                        ; UI
                                        ;

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; remove window titlebar
(if (aa/home-p)
    (add-to-list 'default-frame-alist '(undecorated . t)))
;; allow more space around the sides
(when (fboundp 'set-fringe-mode)
   (set-fringe-mode 10))
;; move tooltips to echo area
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; show keybindings info
(which-key-mode)

;; start to scratch
(setopt inhibit-startup-message t)

;; default fonts
(defun aa/set-default-faces ()
  (cond
   ;; osx-specific setup
   (t (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f) (with-selected-frame f (aa/set-default-faces))))
  (aa/set-default-faces))

;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setopt ring-bell-function nil)
(setopt visible-bell t)

;; name buffers with same files with / paths
(setopt uniquify-buffer-name-style 'forward)



;; show line and column numbers in prog-mode
(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; highlight matching parens
(add-hook 'prog-mode-hook #'show-paren-mode)

;; add russian as C-\ bind
(setopt default-input-method "russian-computer")

;; minibuffers inside minibuffers
(setopt enable-recursive-minibuffers t)
;; recommemded with the above set up
(minibuffer-depth-indicate-mode)

;; hide commands in M-x that do not apply in current mode
;; if no completion-predicate is specified for command then
;; predicate is true when command is applicable to major or any of minor modes
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; ignore custom
(setopt custom-file (expand-file-name "ignored-custom.el" user-emacs-directory))

;; add final newline on save
(setopt require-final-newline t)

;; backup files
(unless backup-directory-alist
  (setopt backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))
(setopt backup-by-copying t)

;; enable recent files list
(recentf-mode)

;; save cursor location on exit
(save-place-mode)

;; save minibuffer history
(savehist-mode)

;; prompt y/n stuff in emacs rather than in dialogue boxes
(setopt use-dialog-box nil)

;; autorevert buffers for changed files
(global-auto-revert-mode)
;; auto-refresh dired too
(setopt global-auto-revert-non-file-buffers t)

;; vertical bar cursor in active frame/window/(mini)buffer
(setopt cursor-type 'bar)
(setopt cursor-in-non-selected-windows 'hollow)

;; (info "(emacs)Killing by Lines")
;; C-k acts as vim dd and kills newline too
(setopt kill-whole-line t)

;; repeat actions without modifiers for some commands
(repeat-mode)

;; undo/redo window configuration changes with C-c left/right
(winner-mode)

                                        ; encryption
(require 'epa-file)
(setopt epg-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)


                                        ; PACKAGES
(with-eval-after-load 'package
  (progn
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize)))

(require 'use-package)
(setq use-package-always-ensure t)

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
      '(other-window
        next-multiframe-window))

(use-package pulsar
  ;; load after evil due to advice-add
  :commands pulsar-pulse-line
  :config
  (pulsar-global-mode 1)
  (dolist (f aa/pulsar-pulse-after)
    (advice-add f :after #'(lambda (&rest args) (pulsar-pulse-line)))))

;; show current key in the modeline
(use-package keycast
  :init (keycast-header-line-mode))

;; ui colors
(defvar aa/before-load-theme-hook nil
  "Hooks to run before calling load-theme.")

(advice-add
 'load-theme
 :before
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa/before-load-theme-hook)))

(defvar aa/after-load-theme-hook nil
  "Hooks run after calling load-theme.")

(advice-add
 'load-theme
 :after
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa/after-load-theme-hook)))

(use-package ef-themes)
(use-package doric-themes)
(use-package solarized-theme)
(use-package base16-theme
  :config
  (setq base16-theme-distinct-fringe-background nil))

(setopt custom-safe-themes
        '(
          ;; solarized-dark
          "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
          ;; doric-marble
          "5dfbcedfeb1a3fab6d401677bf32bda4ef710ea16dfce285c7e87288a0694060"
          ;; doric-obsidian
          "bac67171cc8b7cf622cbe2f1ef494f27ff5fe530621201a1eea6e9457f17ab63"
          ;; doric-plum
          "cbd1c15fcbd258a6882220f807d99e7a8967dd040e687347e5467f54677267ef"
          ;; doric-water
          "b23a8982253227aae8e18365ec4683f1d05a4aa6b662845252b8f1654f8794a3"
          ;; base16-tokyodark
          "a40703f9d1adb7ee1500d3c33ac4d62144675505ae7fe98b18a5d9ff325ee369"
          default))

(cond
 ((aa/work-p) (setq aa/light-theme 'doric-marble
                    aa/dark-theme 'doric-marble))
 (t (setq aa/light-theme 'solarized-dark
                           aa/dark-theme 'base16-tokyodark)))

;; use https://git.sr.ht/~grtcdr/darkman.el
;; to integrate with https://darkman.whynothugo.nl/

(if (and (aa/home-p) (file-exists-p "/usr/bin/darkman"))
    (use-package darkman
      :config
      (setq darkman-themes `(:light ,aa/light-theme :dark ,aa/dark-theme))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 20) (load-theme aa/dark-theme t))
   (t (load-theme aa/light-theme t))))

;; reset themes when switching
(add-hook 'aa/before-load-theme-hook
          #'(lambda nil (mapc #'disable-theme custom-enabled-themes)))

                                        ;
                                        ; C O M P L E T I O N S
                                        ;

;; ;; TODO: replace corfu
;; ;; enable completion preview in prog-mode
;; ;; cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Symbol-Completion.html#Symbol-Completion
;; (add-hook 'prog-mode-hook #'completion-preview-mode)

                                        ; minibuffer

;; vertico (frontend / UI)
;; changes default completion buffer to vertical scrollable thing
(use-package vertico
  :init
  (vertico-mode))

;; marginalia provides marninalia info to completions in minibuffer
;; annotations are per-category
(use-package marginalia
  :init
  (marginalia-mode))

;; ignore case for file name completions
(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        ;; disable dictionary word completion in text modes
        text-mode-ispell-word-completion nil)


                                        ; in-buffer

;; corfu (frontend / UI)
;; completion-at-point (e.g. when writing code)
(use-package corfu
  :custom
  ;; enable auto completion
  (corfu-auto t)
  ;; close corfu buffer if no completion matches
  (corfu-quit-no-match t)
  ;; delay in seconds before completion
  (corfu-auto-delay 0.2)
  ;; prefix for completion
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode))

;; cape
;; suite of completion-at-point functions
;; use prefix map for now
(use-package cape
  :commands cape-prefix-map
  :init
  (add-hook 'completion-at-point-functions #'cape-file))

;; orderless (style)
;; decides how to match completion candidates
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;;

;; completing-read
(use-package consult
  :commands (consult-line
             consult-ripgrep
             consult-outline
             consult-buffer))

;; richer help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

                                        ;
                                        ; O R G
                                        ;

(cond
 ;; osx-specific stuff goes here
 (t (setq aa/org-font-height 140)))

(defun aa/org-setup-fonts ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; setup fonts
  ;; via https://yannesposito.com/posts/0020-cool-looking-org-mode/index.html
  ;; and https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (let* (
         ;;
         (font-height aa/org-font-height)
         ;; variable-width font setup
         (variable-tuple
          (cond
           ((x-list-fonts "PT Sans") `(:family "PT Sans" :height ,font-height))
           ((x-list-fonts "Source Sans Pro") `(:family "Source Sans Pro" :height ,font-height))
           ((x-list-fonts "IBM Plex Sans") `(:family "IBM Plex Sans" :height ,font-height))))
         ;; fixed-width font setup
         (fixed-tuple
          (cond
           ((x-list-fonts "Cascadia Code NF") '(:family "Cascadia Code NF"))
           ((x-list-fonts "Fira Code Retina") '(:family "Fira Code Retina"))
           ((x-list-fonts "Adobe Source Pro") '(:family "Adobe Source Pro"))
           ((x-list-fonts "IBM Plex Mono") '(:family "IBM Plex Mono"))
           ((x-list-fonts "PT Mono") '(:family "PT Mono"))))
         ;; store default font color to reset headlines
         (base-font-color (face-foreground 'default nil 'default))
         ;; store some settings for headlines
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))
    
    (custom-theme-set-faces
     ;; 'user is "current user custom settings"
     'user

     ;; variable and fixed pitch fonts
     `(variable-pitch     ((t ,@variable-tuple)))
     `(fixed-pitch        ((t ,@fixed-tuple)))

     ;; headings
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))

     ;; things that should be fixed-pitch
     '(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray60" :underline nil))))
     '(org-block            ((t (:inherit fixed-pitch))))
     '(org-block-begin-line ((t (:inherit fixed-pitch))))
     '(org-block-end-line   ((t (:inherit fixed-pitch))))
     '(org-src              ((t (:inherit fixed-pitch))))
     '(org-properties       ((t (:inherit fixed-pitch))))
     '(org-code             ((t (:inherit (shadow fixed-pitch)))))
     '(org-date             ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info    ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-drawer           ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent           ((t (:inherit (org-hide fixed-pitch)))))
     `(org-link             ((t (:inherit fixed-pitch :foreground ,base-font-color :underline t))))
     '(org-meta-line        ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value   ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword  ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table            ((t (:inherit fixed-pitch))))
     '(org-tag              ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim         ((t (:inherit (shadow fixed-pitch)))))))
  (message "fonts reset DONE"))

;; hook this function to load-theme to avoid reloading emacs on theme switch
(add-hook 'aa/after-load-theme-hook #'aa/org-setup-fonts)

(defun aa/org-common-hooks ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-agenda
             org-capture
             org-store-link)
  :hook (org-mode . aa/org-common-hooks)
  :config
  (setq org-directory "~/Documents/Notes")
  (setq org-agenda-files `(,org-directory))
  ;; TODO: make the following part of fonts
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  ;; enable scaling of inline images with attr_org width
  (setq org-image-actual-width nil)
  ;; show inline images by default
  (setq org-startup-with-inline-images t)
  ;; Make C-c a t reuse current window
  (setq org-agenda-window-setup 'current-window)
  ;; offer tags from all agenda files (not current buffer)
  (setq org-complete-tags-always-offer-all-agenda-tags t)
  ;; wrap text by default
  ;; TODO: do we need this?
  (setq org-startup-truncated nil)
  ;; show table of contents on load
  (setq org-startup-folded 'content)
  ;; log time when todos are done
  ;; cf. https://orgmode.org/guide/Progress-Logging.html
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(!)" "DONE(!)")))
  (setq org-refile-targets
        '((nil :maxlevel . 5)))
  ;; refile in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; show full paths for refiling
  (setq org-refile-use-outline-path t)
  ;; capture setup
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  (setq aa/capture-templates-dir (concat user-emacs-directory "org-capture-templates"))
  (setq org-capture-templates
        `(("b" "book" entry
           (file ,(concat org-directory "/finished-books.org"))
           (file ,(concat aa/capture-templates-dir "/book"))
           :kill-buffer t)
          ("d" "mind dump" entry
           (file ,(concat org-directory "/mind-dumps.org"))
           (file ,(concat aa/capture-templates-dir "/dump"))
           :prepend t :kill-buffer t)))
  (aa/org-setup-fonts))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun aa/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
       visual-fill-column-center-text t)
 (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . aa/org-mode-visual-fill))

;; org-journal
(defun aa/org-journal-setup ()
  ;; common steps
  (setq org-journal-dir "~/Documents/journal")
  ;; "2024-05-13, Monday"
  (setq org-journal-date-format "%Y-%m-%d, %A")
  ;; 2024-05-13.org
  (setq org-journal-file-format "%F.org")
  ;; system-specific
  (cond
   ;; osx-specific stuff
   (t (progn
        (setq org-journal-file-type 'daily)
        (setq org-journal-encrypt-journal t)))))
  
(use-package org-journal
  :commands org-journal-new-entry
  :after org
  :init
  (aa/org-journal-setup))

;; TODO: research
;; support for image paste
;; https://github.com/abo-abo/org-download

(use-package org-roam
  :if (aa/work-p)
  :commands (org-roam-node-find
             org-roam-capture
             org-roam-buffer-toggle
             org-roam-alias-add
             org-roam-dailies-capture-today
             org-roam-dailies-goto-date
             org-roam-dailies-goto-today
             org-roam-dailies-goto-previous-note
             org-roam-dailies-goto-next-note)
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  (org-roam-dailies-directory "daily/")
  :config
  (org-roam-setup))

                                        ;
                                        ; C O D I N G
                                        ;

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

(use-package lua-mode)
(use-package fennel-mode)
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
(use-package eglot
  :commands eglot
  :config
  (dolist (mode '((nix-mode . ("nixd"))
                  (zig-mode . ("zls"))))
    (add-to-list 'eglot-server-programs mode))
  ;; https://github.com/joaotavora/eglot/discussions/1393
  (setq eglot-ignored-server-capabilites
        (cons :documentOnTypeFormattingProvider
                    eglot-ignored-server-capabilites)))
;; let project.el recognize python project roots
(add-to-list 'project-vc-extra-root-markers "pyproject.toml")


                                        ;
                                        ; T E R M
                                        ;

;; colors
(use-package eterm-256color
  :hook
  (term-mode-hook . eterm-256color-mode)
  (vterm-mode-hook . eterm-256color-mode))

;; vterm
(use-package vterm
  :commands vterm
  :config
  (cond
   ;; osx-specific setup
   (t (setq vterm-shell "bash")))
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))


(use-package 0x0)

(use-package eev
  :init
  (require 'eev-load)
  (eev-mode 1))

;; TODO: consider beam for extra project support
;; https://github.com/rpav/beam.el

(use-package ace-window
  :commands ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; preload org-mode
(with-temp-buffer (org-mode))

                                        ;
                                        ; K E Y M A P S
                                        ;

;; bind C-SPC to activate leader map (note: this overrides mark set)
(defvar aa/leader-map (make-sparse-keymap) "Normal state leader map")
(keymap-global-set "C-c SPC" aa/leader-map)

                                        ; dired

(defvar aa/leader-map-dired (make-sparse-keymap) "SPC d: Dired")
(keymap-global-set "C-c d" aa/leader-map-dired)
(define-key aa/leader-map-dired (kbd "d") #'dired)
(define-key aa/leader-map-dired (kbd "j") #'dired-jump)
(which-key-add-key-based-replacements
  "C-c d" "dired")
(which-key-add-key-based-replacements
  "C-c d j" "dired-jump"
  "C-c d d" "open dired")

                                        ; pulsar

(keymap-global-set "C-c SPC" #'pulsar-pulse-line)

                                        ; org

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c n" `(lambda ()
                              (interactive)
                              (find-file ,(concat org-directory "/all.org"))))
(which-key-add-key-based-replacements
  "C-c a" "org-agenda"
  "C-c c" "org-capture"
  "C-c l" "org-store-link"
  "C-c n" "open notes")

                                        ; org-journal

(keymap-global-set "C-c j" #'org-journal-new-entry)
(which-key-add-key-based-replacements
  "C-c j" "journal")

                                        ; org-roam (work only)

(if (aa/work-p)
    (progn
      (defvar aa/leader-map-org-roam (make-sparse-keymap) "SPC r: org-roam")
      (keymap-global-set "C-c r" aa/leader-map-org-roam)
      (define-key aa/leader-map-org-roam (kbd "f") #'org-roam-node-find)
      (define-key aa/leader-map-org-roam (kbd "c") #'org-roam-capture)
      (define-key aa/leader-map-org-roam (kbd "l") #'org-roam-buffer-toggle)
      (define-key aa/leader-map-org-roam (kbd "a") #'org-roam-alias-add)
      ;; org-roam-dailies
      (defvar aa/leader-map-org-roam-dailies (make-sparse-keymap) "SPC r d: org-roam dailies")
      (define-key aa/leader-map-org-roam (kbd "d") aa/leader-map-org-roam-dailies)
      (define-key aa/leader-map-org-roam-dailies (kbd "c") #'org-roam-dailies-capture-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "d") #'org-roam-dailies-goto-date)
      (define-key aa/leader-map-org-roam-dailies (kbd "t") #'org-roam-dailies-goto-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "p") #'org-roam-dailies-goto-previous-note)
      (define-key aa/leader-map-org-roam-dailies (kbd "n") #'org-roam-dailies-goto-next-note)
      (keymap-set org-mode-map "C-c i" #'org-roam-node-insert)
      (which-key-add-key-based-replacements
        "C-c r" "org-roam"
        "C-c r f" "find node"
        "C-c r c" "capture"
        "C-c r l" "toggle backlinks"
        "C-c r a" "add node alias"
        "C-c r d" "dailies"
        "C-c r d c" "capture today"
        "C-c r d d" "date"
        "C-c r d t" "today"
        "C-c r d p" "previous note"
        "C-c r d n" "next note")))

                                        ; consult

(keymap-global-set "C-c f" #'consult-line)
(keymap-global-set "C-c g" #'consult-ripgrep)
(keymap-global-set "C-c o" #'consult-outline)
(keymap-global-set "C-c b" #'consult-buffer)
(which-key-add-key-based-replacements
  "C-c f" "search line"
  "C-c g" "ripgrep"
  "C-c o" "search outline"
  "C-c b" "select buffer")

                                        ; cape

(keymap-global-set "C-c p" #'cape-prefix-map)

                                        ; package management

(defun aa/recompile-all-packages nil
  (interactive)
  (message "recompiling elpa/ contents")
  (native-compile-async (file-name-concat user-emacs-directory "elpa") t))

                                        ; window management

(defvar aa/leader-map-windows (make-sparse-keymap) "SPC w: window management")
(keymap-global-set "C-c w" aa/leader-map-windows)
(which-key-add-key-based-replacements
  "C-c w" "window management")
;; switch to other window even in a different frame
(keymap-set aa/leader-map-windows "o" #'next-multiframe-window)
(which-key-add-key-based-replacements
  "C-c w o" "next window")
;; ace-window
(keymap-global-set "C-x o" #'ace-window)
(which-key-add-key-based-replacements
  "C-x o" "ace-window")

                                        ; terminal

(keymap-global-set "C-c s" #'eshell)
(which-key-add-key-based-replacements
  "C-c s" "eshell")

;; swap regex and standard search keymaps
(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)
(keymap-global-set "C-M-s" #'isearch-forward)
(keymap-global-set "C-M-r" #'isearch-backward)

;; search for things that are customizable by C-h u
(keymap-global-set "C-h u" #'apropos-user-option)
