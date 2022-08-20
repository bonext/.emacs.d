; built-ins
;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

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
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

; colors
;; solarized-dark
;; TODO: pull solarized to ~/.emacs.d as subtree and load from there
(add-to-list 'custom-theme-load-path "~/src/3rdparty/emacs-color-theme-solarized")
(load-theme 'solarized t)

; packages
(require 'package)

;; MELPA
;;; set up to bleeding-edge
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; evil
;;; evil setup goes here
;;; (setq ...)
;; (require 'evil)
;;; disable evil-mode in some buffers (by their name, cf. C-xC-b)
;; (add-to-list 'evil-buffer-regexps '("^\\*info\\*$"))
;; (add-to-list 'evil-buffer-regexps '("^\\*Geiser.*REPL\\*$"))
;; (evil-mode 1)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; geiser
;;; guile2
(setq geiser-guile-binary "/usr/local/bin/guile2.2")

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;; ElDoc support
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

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
 '(package-selected-packages '(paredit geiser-guile company slime evil)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

