                                        ; BUILT-INS
;; only spaces
(setq-default indent-tabs-mode nil)

;; beep -> visual bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; disable menu bar (it causes glitches on wayland)
(tool-bar-mode -1)

;; show lines in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; startup to scratch
;; (setq inhibit-startup-screen t)

;; add russian as C-\ bind
(setq default-input-method "russian-computer")

;; customize to file
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;; smoother scrolling
(pixel-scroll-precision-mode)

;; encryption
(require 'epa-file)
(setq epa-pinentry-mode 'loopback)
(epa-file-enable)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)

;; tree-sitter
;; install grammars to ~/.emacs.d/tree-sitter
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; eglot
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("~/.local/bin/python-lsp-entrypoint.sh"))))
                                        ; ORG-MODE

;;; enable scaling of inline images with attr_org width
(setq org-image-actual-width nil)
;;; show inline images by default
(setq org-startup-with-inline-images t)
;;; capture setup
(setq org-directory "~/Documents/Notes")
(setq org-default-notes-file (concat org-directory "/captured.org"))
;;; suggested shortcut keys
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

                                        ; colors
;; via https://www.emacswiki.org/emacs/CustomThemes#h5o-3
;; (let ((basedir "~/.emacs.d/themes/"))
;;   (dolist (f (directory-files basedir))
;;     (if (and (not (or (equal f ".") (equal f "..")))
;;              (file-directory-p (concat basedir f)))
;;         (add-to-list 'custom-theme-load-path (concat basedir f)))))

                                        ; PACKAGES

;; parinfer-rust-mode
(setq parinfer-rust-auto-download t)
(add-to-list 'load-path "~/.emacs.d/manual-packages/parinfer-rust-mode")
(autoload 'parinfer-rust-mode "parinfer-rust-mode" nil t)
(add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)

;; TODO: add these as parinfer hooks instead?
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; actual packages
(require 'package)

;; MELPA
;;; set up to bleeding-edge
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize

;; solarized-dark
;; cf. https://github.com/bbatsov/solarized-emacs
;; (load-theme 'solarized-dark t)
;; Don't change size of org-mode headlines (but keep other size-changes)
;; (setq solarized-scale-org-headlines nil

;; nord theme
;; (load-theme 'nord t)

;; modus theme
;; https://protesilaos.com/emacs/modus-themes
;; included in emacs
 (load-theme 'modus-vivendi))

;; org-journal
(setq org-journal-file-type 'daily)
(setq org-journal-dir "~/Documents/journal")
(setq org-journal-date-format "%Y-%m-%d, %A")
(setq org-journal-file-format "%F.org") ; yyyy-mm-dd.org
(setq org-journal-encrypt-journal t)
(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(require 'org-journal)

;; company
(setq company-minimum-prefix-length 5)
(add-hook 'after-init-hook 'global-company-mode)

;; evil
;;; evil setup goes here
;;; (setq ...)
;; uncomment to go evil on demand
;; (setq evil-default-state 'emacs)
(require 'evil)
;;; disable evil-mode in some buffers (by their name, cf. C-xC-b)
;; (add-to-list 'evil-buffer-regexps '("^\\*info\\*$"))
(add-to-list 'evil-buffer-regexps '("^\\*Geiser.*REPL\\*$"))
(add-to-list 'evil-buffer-regexps '("^\\*slime-repl.*\\*$"))
(evil-mode 1)

;; evil-org
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; slime
(setq inferior-lisp-program "/usr/bin/sbcl")

;; geiser
;;; racket
(setq geiser-racket-binary "/usr/bin/racket")
;;; guile
(setq geiser-guile-binary "/usr/bin/guile")

;; ElDoc support
(require 'eldoc)

;; dts-mode
(require 'dts-mode)
(add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode))

;; AoC 2023
(if (file-directory-p "~/src/aoc23-mode")
    (progn
      (add-to-list 'load-path "~/src/aoc23-mode")
      (require 'aoc23-mode)))

;; wayland clipboard support in terminal
;; TODO: make this conditional on wayland
;; credit: yorickvP on Github: https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

                                        ; save position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
