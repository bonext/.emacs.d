;; org-roam  -*- lexical-binding: t; -*-
(use-package org-roam
  :general
  (aa/with-leader
    :states 'normal
    :keymaps 'override
    "r" '(:ignore t :which-key "org-roam")
    "rf" #'org-roam-node-find
    "ri" #'org-roam-node-insert
    "rl" #'org-roam-buffer-toggle
    "rd" '(:ignore t :which-key "dailies")
    "rdc" '(org-roam-dailies-capture-today :which-key "capture today")
    "rdd" '(org-roam-dailies-goto-date :which-key "goto date")
    "rdt" '(org-roam-dailies-goto-today :which-key "goto today")
    "rdp" '(org-roam-dailies-goto-previous-note :which-key "previous note")
    "rdn" '(org-roam-dailies-goto-next-note :which-key "next note"))
  (aa/with-insert-leader
    :states 'insert
    :keymaps 'org-mode-map
    "i" #'org-roam-node-insert)
  :custom
  (org-roam-directory "~/Documents/RoamNotes")
  (org-roam-dailies-directory "daily/")
  :config
  (org-roam-setup))
