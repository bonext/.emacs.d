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
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p "~/.emacs.d/custom.el")
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
      '((python "https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


                                        ; ORG-MODE
(setq org-directory "~/Documents/Notes")

;; populate org-agenda
;; TODO: make this a function and reload agenda by shortcut
(load-library "find-lisp")
(setq org-agenda-files
      (append
       (find-lisp-find-files org-directory "\.org$")))

;;; fonts and colors
;;; via https://yannesposito.com/posts/0020-cool-looking-org-mode/index.html
;;; and https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;;; make bold show bold and not *bold*
(setq org-hide-emphasis-markers t)
;;; variable pitch
(add-hook 'org-mode-hook 'variable-pitch-mode)
;;; set specific fonts for stuff
;;; use (font-family-list) and C-j in scratch to list fonts
(defun org-reset-my-fonts ()
  (let* ((variable-tuple
          (cond
           ((x-list-fonts "Source Sans Pro") '(:family "Source Sans Pro"))
           ((x-list-fonts "IBM Plex Sans") '(:family "IBM Plex Sans"))
           ((x-list-fonts "PT Sans") '(:family "PT Sans"))))
         (fixed-tuple
          (cond
           ((x-list-fonts "Adobe Source Pro") '(:family "Adobe Source Pro"))
           ((x-list-fonts "IBM Plex Mono") '(:family "IBM Plex Mono"))
           ((x-list-fonts "PT Mono") '(:family "PT Mono"))))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))
     `(variable-pitch     ((t ,@variable-tuple)))
     `(fixed-pitch        ((t ,@fixed-tuple)))

     '(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray40" :underline nil))))
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
     '(org-verbatim         ((t (:inherit (shadow fixed-pitch))))))))

;;; enable scaling of inline images with attr_org width
(setq org-image-actual-width nil)

;;; show inline images by default
(setq org-startup-with-inline-images t)

;;; capture setup
(setq org-default-notes-file (concat org-directory "/captured.org"))

;; Make C-c a t reuse current window
(setq org-agenda-window-setup 'current-window)

;; offer tags from all agenda files (not current buffer)
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; wrap text by default
(setq org-startup-truncated nil)

;; log time when todos are done
;; cf. https://orgmode.org/guide/Progress-Logging.html
(setq org-log-into-drawer "LOGBOOK")
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(!)" "DONE(!)")))

(setq org-refile-targets
      '((nil :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(setq org-capture-templates
 '(("b" "book" entry
    (file "~/Documents/Notes/finished-books.org")
    (file "~/.emacs.d/org-capture-templates/book")
    :kill-buffer t)
   ("d" "mind dump" entry
    (file "~/Documents/Notes/mind-dumps.org")
    (file "~/.emacs.d/org-capture-templates/dump")
    :prepend t :kill-buffer t)))

;;; suggested shortcut keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

                                        ; PACKAGES
                                        ; auto packages
(require 'package)

;; MELPA
;; https://melpa.org/#/
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             ;; '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
 
(package-initialize)

;; colors
(use-package nord-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

;; solarized-dark
;; cf. https://github.com/bbatsov/solarized-emacs
(cond
 ((> (decoded-time-hour (decode-time)) 21) (load-theme 'solarized-dark t))
 (t (load-theme 'solarized-selenized-white t)))

;; nord theme
;; (load-theme 'nord t)

;; modus theme
;; https://protesilaos.com/emacs/modus-themes
;; included in emacs
;; (load-theme 'modus-vivendi)

(org-reset-my-fonts)

;; org-bullets
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
;; TODO: add these as parinfer hooks instead?
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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
;; credit: yorickvP on Github: https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
;; (display-graphic-p): https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html#index-display_002dgraphic_002dp
;; $WAYLAND_DISPLAY: https://unix.stackexchange.com/a/559950
(if (and (display-graphic-p) (getenv "WAYLAND_DISPLAY"))
    (progn
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
      (setq interprogram-paste-function 'wl-paste)))
