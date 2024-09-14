;; org-roam  -*- lexical-binding: t; -*-
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))
