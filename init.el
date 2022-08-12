; misc
;; colorscheme
(load-theme 'tango-dark)
;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)


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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
