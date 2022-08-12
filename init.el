; misc
;; solarized colorscheme
;;; TODO: pull solarized as subtree and load from there
(add-to-list 'custom-theme-load-path "~/src/3rdparty/emacs-color-theme-solarized")
(load-theme 'solarized t)

;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; startup to scratch
(setq inhibit-startup-screen t)

;; add russian as C-\ bind
;;; TODO: move to C-^ (as in vim) instead?
(setq current-input-method "russian-computer")


; packages
(require 'package)

;; MELPA
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; evil
;;; evil setup goes here
;;; (setq ...)
(require 'evil)
(evil-mode 1)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

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
 '(package-selected-packages '(slime evil)))
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

