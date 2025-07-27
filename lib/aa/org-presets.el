;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/use-package-presets)
(require 'aa/org-font-presets)

(defun aa/org-common-hooks ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-agenda
             org-capture
             org-store-link)
  :hook (org-mode . aa/org-common-hooks)
  :config
  (setq org-directory "~/Documents/Notes")
  (setq org-agenda-files `(,org-directory))
  ;; TODO: make the following part of fonts
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  ;; enable scaling of inline images with attr_org width
  (setq org-image-actual-width nil)
  ;; show inline images by default
  (setq org-startup-with-inline-images t)
  ;; Make C-c a t reuse current window
  (setq org-agenda-window-setup 'current-window)
  ;; offer tags from all agenda files (not current buffer)
  (setq org-complete-tags-always-offer-all-agenda-tags t)
  ;; wrap text by default
  ;; TODO: do we need this?
  (setq org-startup-truncated nil)
  ;; show table of contents on load
  (setq org-startup-folded 'content)
  ;; log time when todos are done
  ;; cf. https://orgmode.org/guide/Progress-Logging.html
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(!)" "DONE(!)")))
  (setq org-refile-targets
        '((nil :maxlevel . 5)))
  ;; refile in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; show full paths for refiling
  (setq org-refile-use-outline-path t)
  ;; capture setup
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  (setq aa/capture-templates-dir (concat user-emacs-directory "org-capture-templates"))
  (setq org-capture-templates
        `(("b" "book" entry
           (file ,(concat org-directory "/finished-books.org"))
           (file ,(concat aa/capture-templates-dir "/book"))
           :kill-buffer t)
          ("d" "mind dump" entry
           (file ,(concat org-directory "/mind-dumps.org"))
           (file ,(concat aa/capture-templates-dir "/dump"))
           :prepend t :kill-buffer t)))
  (aa/org-setup-fonts))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun aa/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
       visual-fill-column-center-text t)
 (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . aa/org-mode-visual-fill))

;; org-journal
(defun aa/org-journal-setup ()
  ;; common steps
  (setq org-journal-dir "~/Documents/journal")
  ;; "2024-05-13, Monday"
  (setq org-journal-date-format "%Y-%m-%d, %A")
  ;; 2024-05-13.org
  (setq org-journal-file-format "%F.org")
  ;; system-specific
  (cond
   ;; osx-specific stuff
   (t (progn
        (setq org-journal-file-type 'daily)
        (setq org-journal-encrypt-journal t)))))
  
(use-package org-journal
  :commands org-journal-new-entry
  :after org
  :init
  (aa/org-journal-setup))

;; TODO: research
;; support for image paste
;; https://github.com/abo-abo/org-download

(use-package org-roam
  :if (aa/work-p)
  :commands (org-roam-node-find
             org-roam-capture
             org-roam-buffer-toggle
             org-roam-alias-add
             org-roam-dailies-capture-today
             org-roam-dailies-goto-date
             org-roam-dailies-goto-today
             org-roam-dailies-goto-previous-note
             org-roam-dailies-goto-next-note)
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  (org-roam-dailies-directory "daily/")
  :config
  (org-roam-setup))

(provide 'aa/org-presets)
