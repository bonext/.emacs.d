;; org-roam  -*- lexical-binding: t; -*-
(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  :bind (:map aa/leader-map
              ("r l" . org-roam-buffer-toggle)
              ("r f" . org-roam-node-find)
              ("r i" . org-roam-node-insert))
  :config
  (org-roam-setup))
