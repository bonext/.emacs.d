;; -*- lexical-binding: t; -*-
                                        ; COLORS
(use-package ef-themes)

(use-package nord-theme)

(use-package solarized-theme)

(use-package gruvbox-theme)

(cond
 ;; osx-specific setup goes here
 (t (progn
      (setq aa/light-theme 'ef-kassio)
      (setq aa/dark-theme 'ef-duo-dark))))

;; use https://git.sr.ht/~grtcdr/darkman.el
;; to integrate with https://darkman.whynothugo.nl/

(if (file-exists-p "/usr/bin/darkman")
    (use-package darkman
      :config
      (setq darkman-themes `(:light ,aa/light-theme :dark ,aa/dark-theme))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 20) (load-theme aa/dark-theme t))
   (t (load-theme aa/light-theme t))))
