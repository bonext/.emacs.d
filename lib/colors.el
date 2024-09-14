;; -*- lexical-binding: t; -*-
                                        ; COLORS
(use-package ef-themes
  :straight t)

(use-package nord-theme
  :straight t)

(use-package solarized-theme
  :straight t)

(use-package gruvbox-theme
  :straight t)

(cond
 ;; osx-specific setup goes here
 (t (progn
      (setq aa/light-theme 'solarized-selenized-light)
      (setq aa/dark-theme 'gruvbox-dark-hard))))

;; use https://git.sr.ht/~grtcdr/darkman.el
;; to integrate with https://darkman.whynothugo.nl/

;; register darkman.el with straight
;; cf. https://github.com/radian-software/straight.el?tab=readme-ov-file#loading-packages-conditionaly
(straight-register-package 'darkman)

(if (file-exists-p "/usr/bin/darkman")
    (use-package darkman
      :straight t
      :config
      (setq darkman-themes `(:light ,aa/light-theme :dark ,aa/dark-theme))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 20) (load-theme aa/dark-theme t))
   (t (load-theme aa/light-theme t))))
