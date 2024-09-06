(defun aa/org-common-hooks ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(load (concat user-emacs-directory "lib/org-fonts.el"))

(use-package org
  :hook (org-mode . aa/org-common-hooks)
  :config
  (setq org-directory "~/Documents/Notes")
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
  (global-set-key (kbd "C-c l") 'org-store-link))

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
(use-package org-journal
  :after org
  :init
  (setq org-journal-file-type 'daily)
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-file-format "%F.org") ; yyyy-mm-dd.org
  (setq org-journal-encrypt-journal t))

;; separate because the package load is deferred
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
