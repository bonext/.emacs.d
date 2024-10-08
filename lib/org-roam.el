;; org-roam  -*- lexical-binding: t; -*-
(use-package org-roam
  :straight t
  :general
  (aa/with-leader
    :states 'normal
    :keymaps 'override
    "r" '(:ignore t :which-key "org-roam")
    "rf" #'org-roam-node-find
    "ri" #'org-roam-node-insert
    "rl" #'org-roam-buffer-toggle)
  (aa/with-insert-leader
    :states 'insert
    :keymaps 'org-mode-map
    "i" #'org-roam-node-insert)
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  :config
  (org-roam-setup))
