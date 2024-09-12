;; org-roam  -*- lexical-binding: t; -*-
;; mostly via https://systemcrafters.net/build-a-second-brain-in-emacs/getting-started-with-org-roam/
(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))
