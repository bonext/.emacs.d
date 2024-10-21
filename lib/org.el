;; -*- lexical-binding: t; -*-
(defun aa/org-common-hooks ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(load (concat user-emacs-directory "lib/org-fonts.el"))

(use-package org
  :hook (org-mode . aa/org-common-hooks)
  :config
  (setq org-directory "~/Documents/Notes")
  (setq org-agenda-files '("~/Documents/Notes"
                           "~/Documents/RoamNotes"
                           "~/Documents/RoamNotes/daily"))
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
  ;; log time when todos are done
  ;; cf. https://orgmode.org/guide/Progress-Logging.html
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(!)" "DONE(!)")))
  (setq org-refile-targets
        '((nil :maxlevel . 9)))
  ;; refile in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; show full paths for refiling
  (setq org-refile-use-outline-path t)
  ;; capture setup
  (setq org-default-notes-file (concat org-directory "/captured.org"))
  (setq org-my-capture-templates-dir (concat user-emacs-directory "org-capture-templates"))
  (setq org-capture-templates
        '(("b" "book" entry
           (file "~/Documents/Notes/finished-books.org")
           (file "~/.emacs.d/book")
           :kill-buffer t)
          ("d" "mind dump" entry
           (file "~/Documents/Notes/mind-dumps.org")
           (file "~/.emacs.d/dump")
           :prepend t :kill-buffer t)))
  (aa/org-setup-fonts)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (aa/with-leader
    :states 'normal
    :keymaps 'override
    "a" '(org-agenda :which-key "org-agenda")))

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
  :after org
  :init
  (aa/org-journal-setup))

;; separate because the package load is deferred
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(aa/with-leader
  :states 'normal
  :keymaps 'override
  "j" '(:ignore t :which-key "org-journal")
  "jj" #'org-journal-new-entry)

;; TODO: research
;; support for image paste
;; https://github.com/abo-abo/org-download
