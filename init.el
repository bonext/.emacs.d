; built-ins

;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; disable menu bar (it causes glitches on wayland)
(tool-bar-mode -1)

;; show lines in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; startup to scratch
;; (setq inhibit-startup-screen t)

;; add russian as C-\ bind
;;; TODO: move to C-^ (as in vim) instead
;;;       - that's impossible because it clashes
;;;         with some emacs defaults
(setq default-input-method "russian-computer")

;; org-mode
;;; suggested shortcut keys
(global-set-key (kbd "C-c l") 'org-store-link)
;;; enable scaling of inline images with attr_org width
(setq org-image-actual-width nil)
;;; show inline images by default
(setq org-startup-with-inline-images t)
;;; capture setup
(setq org-directory "~/Documents/Notes")
(setq org-default-notes-file (concat org-directory "/captured.org"))
(global-set-key (kbd "C-c c") 'org-capture)

; colors
;; via https://www.emacswiki.org/emacs/CustomThemes#h5o-3
;; (let ((basedir "~/.emacs.d/themes/"))
;;   (dolist (f (directory-files basedir))
;;     (if (and (not (or (equal f ".") (equal f "..")))
;;              (file-directory-p (concat basedir f)))
;;         (add-to-list 'custom-theme-load-path (concat basedir f)))))

; packages

;;; yuck-mode
;;; via https://github.com/bonext/.emacs.d/tree/master/manual-packages/yuck-mode#how-do-i-install-it
(add-to-list 'load-path "~/.emacs.d/manual-packages/yuck-mode")
(autoload 'yuck-mode "yuck-mode" nil t)

;; parinfer-rust-mode
(add-to-list 'load-path "~/.emacs.d/manual-packages/parinfer-rust-mode")
(autoload 'parinfer-rust-mode "parinfer-rust-mode" nil t)
(add-hook 'emacs-lisp-mode 'parinfer-rust-mode)
(setq parinfer-rust-auto-download t)

;; TODO: add these as parinfer hooks instead?
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; actual packages
(require 'package)

;; MELPA
;;; set up to bleeding-edge
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; solarized-dark
(load-theme 'solarized-dark t)

;; org-journal
(setq org-journal-file-type 'daily)
(setq org-journal-dir "~/Documents/journal")
(setq org-journal-date-format "%Y-%m-%d, %A")
(setq org-journal-file-format "%F.org") ; yyyy-mm-dd.org
(require 'org-journal)

;; company
(setq company-minimum-prefix-length 5)
(add-hook 'after-init-hook 'global-company-mode)

;; evil
;;; evil setup goes here
;;; (setq ...)
;; uncomment to go evil on demand
;; (setq evil-default-state 'emacs)
(require 'evil)
;;; disable evil-mode in some buffers (by their name, cf. C-xC-b)
;; (add-to-list 'evil-buffer-regexps '("^\\*info\\*$"))
(add-to-list 'evil-buffer-regexps '("^\\*Geiser.*REPL\\*$"))
(add-to-list 'evil-buffer-regexps '("^\\*slime-repl.*\\*$"))
(evil-mode 1)

;; evil-org
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; slime
(setq inferior-lisp-program "/usr/bin/sbcl")

;; geiser
;;; guile2
(setq geiser-guile-binary "/usr/local/bin/guile2.2")

;; ElDoc support
(require 'eldoc)

; save position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-firefox)
 '(package-selected-packages
   '(vimrc-mode markdown-mode slime evil-org org geiser geiser-racket racket-mode solarized-theme org-journal lua-mode paredit geiser-guile company evil))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

