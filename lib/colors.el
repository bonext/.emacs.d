                                        ; COLORS
(use-package nord-theme
  :ensure t)

(use-package solarized-theme
  :ensure t)

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
      :ensure t
      :config
      (setq darkman-themes '(:light solarized-selenized-white :dark solarized-dark))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 21) (load-theme 'solarized-dark t))
   (t (load-theme 'solarized-selenized-white t))))
