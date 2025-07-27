;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/wk-presets)

(defvar aa/leader-map (make-sparse-keymap) "Normal state leader map")

;; Make ESC quit prompts
;; TODO: this does not really work because evil overloads this setting
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; bind C-SPC to activate leader map (note: this overrides mark set)
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

(keymap-global-set "C-c C-c" #'pulsar-pulse-line)

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
      (define-key aa/leader-map (kbd "r") aa/leader-map-org-roam)
      (define-key aa/leader-map-org-roam (kbd "f") #'org-roam-node-find)
      (define-key aa/leader-map-org-roam (kbd "c") #'org-roam-capture)
      (define-key aa/leader-map-org-roam (kbd "l") #'org-roam-buffer-toggle)
      (define-key aa/leader-map-org-roam (kbd "a") #'org-roam-alias-add)
      (defvar aa/leader-map-org-roam-dailies (make-sparse-keymap) "SPC r d: org-roam dailies")
      (define-key aa/leader-map-org-roam (kbd "d") aa/leader-map-org-roam-dailies)
      (define-key aa/leader-map-org-roam-dailies (kbd "c") #'org-roam-dailies-capture-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "d") #'org-roam-dailies-goto-date)
      (define-key aa/leader-map-org-roam-dailies (kbd "t") #'org-roam-dailies-goto-today)
      (define-key aa/leader-map-org-roam-dailies (kbd "p") #'org-roam-dailies-goto-previous-note)
      (define-key aa/leader-map-org-roam-dailies (kbd "n") #'org-roam-dailies-goto-next-note)
      (keymap-set org-mode-map (kbd "C-c i") #'org-roam-node-insert)
      (which-key-add-key-based-replacements
        "SPC r" "org-roam"
        "SPC r f" "find node"
        "SPC r c" "capture"
        "SPC r l" "toggle backlinks"
        "SPC r a" "add node alias"
        "SPC r d" "dailies"
        "SPC r d c" "capture today"
        "SPC r d d" "date"
        "SPC r d t" "today"
        "SPC r d p" "previous note"
        "SPC r d n" "next note")))

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

                                        ; recentf

(keymap-global-set "C-c r" #'recentf-open-files)
(which-key-add-key-based-replacements
  "C-c r" "recent files")

                                        ; terminal

(keymap-global-set "C-c s" #'eshell)
(which-key-add-key-based-replacements
  "C-c s" "eshell")

(provide 'aa/keymaps)

;; TODO: research hydra
;; tldr: transient keybindings
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra
