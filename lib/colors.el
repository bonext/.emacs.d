                                        ; COLORS
(use-package ef-themes)

(use-package nord-theme)

(use-package solarized-theme)

(use-package gruvbox-theme)

;; solarized-dark
;; cf. https://github.com/bbatsov/solarized-emacs

;; modus theme
;; https://protesilaos.com/emacs/modus-themes
;; included in emacs
;; (load-theme 'modus-vivendi)

;; use https://git.sr.ht/~grtcdr/darkman.el
;; to integrate with https://darkman.whynothugo.nl/
(if (file-exists-p "/usr/bin/darkman")
    (use-package darkman
      :config
      (setq darkman-themes '(:light solarized-selenized-white :dark solarized-selenized-black))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 21) (load-theme 'solarized-selenized-black t))
   (t (load-theme 'solarized-selenized-white t))))
