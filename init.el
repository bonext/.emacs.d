; colors
(load-theme 'tango-dark)

; packages
(require 'package)

;; MELPA
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; evil
(require 'evil)
(evil-mode 1)

; save position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
