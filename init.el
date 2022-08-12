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

; save position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
