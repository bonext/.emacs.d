;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/wk-presets)

(defvar aa/leader-map (make-sparse-keymap) "Normal state leader map")

;; Make ESC quit prompts
;; TODO: this does not really work because evil overloads this setting
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; bind "SPC" in normal mode to activate leader map
(evil-define-key 'normal 'global (kbd "SPC") aa/leader-map)

                                        ; dired

(defvar aa/leader-map-dired (make-sparse-keymap) "SPC d: Dired")
(define-key aa/leader-map (kbd "d") aa/leader-map-dired)
(define-key aa/leader-map-dired (kbd "d") #'dired)
(define-key aa/leader-map-dired (kbd "j") #'dired-jump)
(which-key-add-key-based-replacements
  "SPC d" "dired")
(which-key-add-key-based-replacements
  "SPC d j" "dired-jump"
  "SPC d d" "open dired")

                                        ; pulsar

(define-key aa/leader-map (kbd "SPC") #'pulsar-pulse-line)

                                        ; org

(define-key aa/leader-map (kbd "a") #'org-agenda)
(define-key aa/leader-map (kbd "n") `(lambda ()
                                       (interactive)
                                       (find-file ,(concat org-directory "/all.org"))))
(which-key-add-key-based-replacements
  "SPC a" "org-agenda"
  "SPC n" "notes")

                                        ; org-journal

(define-key aa/leader-map (kbd "j") #'org-journal-new-entry)
(which-key-add-key-based-replacements
  "SPC j" "journal")

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
      (evil-define-key 'insert 'org-mode-map (kbd "C-c i") #'org-roam-node-insert)
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

(define-key aa/leader-map (kbd "/") #'consult-line)
(define-key aa/leader-map (kbd "f") #'consult-ripgrep)
(define-key aa/leader-map (kbd "o") #'consult-outline)
(define-key aa/leader-map (kbd "b") #'consult-buffer)
(which-key-add-key-based-replacements
  "SPC /" "search line"
  "SPC f" "ripgrep"
  "SPC o" "search outline"
  "SPC b" "select buffer")

                                        ; cape

(evil-define-key 'insert 'global (kbd "C-c p") #'cape-prefix-map)

                                        ; package management

(defun aa/recompile-all-packages nil
  (interactive)
  (message "recompiling elpa/ contents")
  (native-compile-async (file-name-concat user-emacs-directory "elpa") t))

(defvar aa/leader-map-packages (make-sparse-keymap) "SPC p: package management")
(define-key aa/leader-map (kbd "p") aa/leader-map-packages)
(define-key aa/leader-map-packages (kbd "l") #'list-packages)
(define-key aa/leader-map-packages (kbd "r") #'aa/recompile-all-packages)
(which-key-add-key-based-replacements
  "SPC p" "package management")
(which-key-add-key-based-replacements
  "SPC p l" "list packages")
(which-key-add-key-based-replacements
  "SPC p r" "recompile packages")

                                        ; window management

(defvar aa/leader-map-windows (make-sparse-keymap) "SPC w: window management")
(define-key aa/leader-map (kbd "w") aa/leader-map-windows)
(which-key-add-key-based-replacements
  "SPC w" "window management")

;; switch to other window even in a different frame
(define-key aa/leader-map-windows (kbd "o") #'next-multiframe-window)
(which-key-add-key-based-replacements
  "SPC w o" "next window")

                                        ; recentf

(define-key aa/leader-map (kbd "c") #'recentf-open-files)
(which-key-add-key-based-replacements
  "SPC c" "recent files")

                                        ; eglot

(define-key aa/leader-map (kbd "e") #'eglot)
(which-key-add-key-based-replacements
  "SPC e" "eglot")

                                        ; vterm

(define-key aa/leader-map (kbd "t") #'vterm)
(which-key-add-key-based-replacements
  "SPC t" "terminal")

(provide 'aa/keymaps)

;; TODO: research hydra
;; tldr: transient keybindings
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-03.org#hydra
