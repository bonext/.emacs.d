;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/wk-presets)

;; Make ESC quit prompts
;; (keymap-global-set "<escape>" #'keyboard-escape-quit)

;; bind C-SPC to activate leader map (note: this overrides mark set)
(defvar aa/leader-map (make-sparse-keymap) "Normal state leader map")
(keymap-global-set "C-c SPC" aa/leader-map)

                                        ; dired

(defvar aa/leader-map-dired (make-sparse-keymap) "SPC d: Dired")
(keymap-global-set "C-c d" aa/leader-map-dired)
(define-key aa/leader-map-dired (kbd "d") #'dired)
(define-key aa/leader-map-dired (kbd "j") #'dired-jump)
(which-key-add-key-based-replacements
  "C-c d" "dired")
(which-key-add-key-based-replacements
  "C-c d j" "dired-jump"
  "C-c d d" "open dired")

                                        ; pulsar

(keymap-global-set "C-c SPC" #'pulsar-pulse-line)

                                        ; org

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c n" `(lambda ()
                              (interactive)
                              (find-file ,(concat org-directory "/all.org"))))
(which-key-add-key-based-replacements
  "C-c a" "org-agenda"
  "C-c c" "org-capture"
  "C-c l" "org-store-link"
  "C-c n" "open notes")

                                        ; org-journal

(keymap-global-set "C-c j" #'org-journal-new-entry)
(which-key-add-key-based-replacements
  "C-c j" "journal")

                                        ; org-roam (work only)

(if (aa/work-p)
    (progn
      (defvar aa/leader-map-org-roam (make-sparse-keymap) "SPC r: org-roam")
      (keymap-global-set "C-c r" aa/leader-map-org-roam)
      (define-key aa/leader-map-org-roam (kbd "f") #'org-roam-node-find)
      (define-key aa/leader-map-org-roam (kbd "c") #'org-roam-capture)
      (define-key aa/leader-map-org-roam (kbd "l") #'org-roam-buffer-toggle)
      (define-key aa/leader-map-org-roam (kbd "a") #'org-roam-alias-add)
      ;; org-roam-dailies
      (defvar aa/leader-map-org-roam-dailies (make-sparse-keymap) "SPC r d: org-roam dailies")
      (define-key aa/leader-map-org-roam (kbd "d") aa/leader-map-org-roam-dailies)
      (define-key aa/leader-map-org-roam-dailies (kbd "c") #'org-roam-dailies-capture-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "d") #'org-roam-dailies-goto-date)
      (define-key aa/leader-map-org-roam-dailies (kbd "t") #'org-roam-dailies-goto-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "p") #'org-roam-dailies-goto-previous-note)
      (define-key aa/leader-map-org-roam-dailies (kbd "n") #'org-roam-dailies-goto-next-note)
      (keymap-set org-mode-map "C-c i" #'org-roam-node-insert)
      (which-key-add-key-based-replacements
        "C-c r" "org-roam"
        "C-c r f" "find node"
        "C-c r c" "capture"
        "C-c r l" "toggle backlinks"
        "C-c r a" "add node alias"
        "C-c r d" "dailies"
        "C-c r d c" "capture today"
        "C-c r d d" "date"
        "C-c r d t" "today"
        "C-c r d p" "previous note"
        "C-c r d n" "next note")))

                                        ; consult

(keymap-global-set "C-c f" #'consult-line)
(keymap-global-set "C-c g" #'consult-ripgrep)
(keymap-global-set "C-c o" #'consult-outline)
(keymap-global-set "C-c b" #'consult-buffer)
(which-key-add-key-based-replacements
  "C-c f" "search line"
  "C-c g" "ripgrep"
  "C-c o" "search outline"
  "C-c b" "select buffer")

                                        ; cape

(keymap-global-set "C-c p" #'cape-prefix-map)

                                        ; package management

(defun aa/recompile-all-packages nil
  (interactive)
  (message "recompiling elpa/ contents")
  (native-compile-async (file-name-concat user-emacs-directory "elpa") t))

                                        ; window management

(defvar aa/leader-map-windows (make-sparse-keymap) "SPC w: window management")
(keymap-global-set "C-c w" aa/leader-map-windows)
(which-key-add-key-based-replacements
  "C-c w" "window management")
;; switch to other window even in a different frame
(keymap-set aa/leader-map-windows "o" #'next-multiframe-window)
(which-key-add-key-based-replacements
  "C-c w o" "next window")
;; ace-window
(keymap-global-set "C-x o" #'ace-window)
(which-key-add-key-based-replacements
  "C-x o" "ace-window")

                                        ; terminal

(keymap-global-set "C-c s" #'eshell)
(which-key-add-key-based-replacements
  "C-c s" "eshell")

(provide 'aa/keymaps)

;; TODO: research hydra
;; tldr: transient keybindings
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra
