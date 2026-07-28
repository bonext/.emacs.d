;; -*- lexical-binding: t; eval: (outline-minor-mode 1) -*-

;; configurable variables

(defvar aa-face-default-name "Cascadia Code NF")
(defvar aa-face-default-height 100)
(defvar aa-dark-theme 'tokyo-night)
(defvar aa-light-theme 'tokyo-night-moon)

(defvar aa-face-org-height 140)
(defvar aa-org-directory "~/Documents/Notes")
(defvar aa-org-journal-directory "~/Documents/journal")

;; vendored code
(defvar aa-vendor-directory (file-name-concat user-emacs-directory "lib/3rdparty"))

(defvar aa-vendor--autoloads-filename (file-name-concat aa-vendor-directory "autoloads.el"))

(defun aa-vendor--find-elisp-dirs (root)
  (let (out)
    (dolist (item (directory-files-recursively root "\\.el\\'"))
      (push (file-name-directory item) out))
    (delete-dups out)))

(defun aa-vendor-update-autoloads ()
  (interactive)
  (message "Using vendor autoloads file: %s" aa-vendor--autoloads-filename)
  (when (file-exists-p aa-vendor--autoloads-filename)
    (progn
      (delete-file aa-vendor--autoloads-filename)
      (message "Cleaned up stale autoloads at %s" aa-vendor--autoloads-filename)))
  (loaddefs-generate (aa-vendor--find-elisp-dirs aa-vendor-directory) aa-vendor--autoloads-filename)
  (message "Autoloads updated at %s" aa-vendor--autoloads-filename))

;; setup load-path for vendored code
(when (file-exists-p aa-vendor-directory)
  (add-to-list 'load-path aa-vendor-directory)
  (dolist (dir (directory-files aa-vendor-directory t "^[^.]" t))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)
      (message "Added vendored code directory %s to load-path" dir))))

;; load vendored autoloads
(when (file-exists-p aa-vendor--autoloads-filename)
  (progn
    (load aa-vendor--autoloads-filename)
    (message "Loaded vendor autoloads from %s" aa-vendor--autoloads-filename)))

;; other load-path tweaks

;; custom themes (to be able to M-x load-theme)
(add-to-list 'custom-theme-load-path (file-name-concat aa-vendor-directory "doric-themes"))
(add-to-list 'custom-theme-load-path (file-name-concat aa-vendor-directory "tokyo-night"))

;; vertico extensions
(add-to-list 'load-path (file-name-concat aa-vendor-directory "vertico" "extensions"))

;; exposed hooks

(defvar aa-before-load-theme-hook nil
  "Hooks to run before calling load-theme.")
(defvar aa-after-load-theme-hook nil
  "Hooks run after calling load-theme.")

(let ((aa-host
       (cond ((and (eq system-type 'gnu/linux)
                   (string-match "-[Mm]icrosoft" operating-system-release))
              'wsl)
             ((eq system-type 'darwin) 'work)
             (t 'home))))
  (defun aa-home-p ()
    (eq aa-host 'home))
  (defun aa-work-p ()
    (eq aa-host 'work))
  (defun aa-wsl-p ()
    (eq aa-host 'wsl)))

(advice-add
 'load-theme
 :before
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa-before-load-theme-hook)))

(advice-add
 'load-theme
 :after
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa-after-load-theme-hook)))

;; unload theme before switching
(add-hook 'aa-before-load-theme-hook
          #'(lambda () (mapc #'disable-theme custom-enabled-themes)))

;; 

(defun aa-recompile-all-packages nil
  (interactive)
  (message "recompiling lib/3rdparty/ contents")
  (native-compile-async (file-name-concat user-emacs-directory "lib" "3rdparty") t))

;; colors
(load-theme aa-dark-theme t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; remove window titlebar
(if (aa-home-p)
    (add-to-list 'default-frame-alist '(undecorated . t)))
;; allow more space around the sides
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode 10))
;; move tooltips to echo area
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; show keybindings info
(which-key-mode)

;; start to scratch
(setopt inhibit-startup-message t)

;; default fonts
(defun aa-set-default-faces ()
  (set-face-attribute 'default nil :font aa-face-default-name :height aa-face-default-height))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f) (with-selected-frame f (aa-set-default-faces))))
  (aa-set-default-faces))

;; beep -> visual bell
(setopt ring-bell-function nil)
(setopt visible-bell nil)
;; name buffers with same files with / paths
(setopt uniquify-buffer-name-style 'forward)

;; show line and column numbers in prog-mode
(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; highlight matching parens
(add-hook 'prog-mode-hook #'show-paren-mode)

;; add russian as C-\ bind
(setopt default-input-method "russian-computer")

;; minibuffers inside minibuffers
;; (setopt enable-recursive-minibuffers t)
;; recommemded with the above set up
;; (minibuffer-depth-indicate-mode)

;; hide commands in M-x that do not apply in current mode
;; if no completion-predicate is specified for command then
;; predicate is true when command is applicable to major or any of minor modes
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; prompt y/n stuff in emacs rather than in dialogue boxes
(setopt use-dialog-box nil)

;; enter gpg password in echo area
(setopt epg-pinentry-mode 'loopback)
;; workaround for gpg getting stuck
;; this may have side-effects. If so, downgrading to GnuPG 2.4.0 should help
(fset 'epg-wait-for-status 'ignore)

;; autorevert buffers for changed files
(global-auto-revert-mode)
;; auto-refresh dired too
(setopt global-auto-revert-non-file-buffers t)

;; vertical bar cursor in active frame/window/(mini)buffer
(setopt cursor-type 'bar)
(setopt cursor-in-non-selected-windows 'hollow)

;; search match counting
(setopt isearch-lazy-count t)

;; save a lot of time
(setopt use-short-answers t)

;; kill ring setup
;; save paste to kill ring (C-y M-y to get it after killing stuff)
(setopt save-interprogram-paste-before-kill t)
;; no dupes in kill ring
(setopt kill-do-not-save-duplicates t)

;; ultra-scroll: smoother scrolling
(setopt scroll-conservatively 3)
(setopt scroll-margin 0)
(ultra-scroll-mode t)

;; highlight after typing is done
;; setq because C
(setq redisplay-skip-fontification-on-input t)

;; disable right-to-left text tweaks
;; setq because these are in C code
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Ignore custom
(setopt custom-file (file-name-concat user-emacs-directory "ignored-custom.el"))

;; only spaces
(setq-default indent-tabs-mode nil)

;; add final newline on save
(setopt require-final-newline t)

;; backup files
(unless backup-directory-alist
  (setopt backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "backups")))))
(setopt backup-by-copying t)

;; enable recent files list
(recentf-mode)

;; save cursor location on exit
(save-place-mode)

;; save minibuffer history
(savehist-mode)

;; (info "(emacs)Killing by Lines")
;; C-k acts as vim dd and kills newline too
(setopt kill-whole-line t)

;; repeat actions without modifiers for some commands
(repeat-mode)
;; first mark pop is C-u C-SPC, next C-SPC only
(setopt set-mark-command-repeat-pop t)

;; jump to help automatically
(setopt help-window-select t)

;; undo/redo window configuration changes with C-c left/right
(winner-mode)

;; make C-x 1 reversible
(defun aa-toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; resize all windows in frame on split
(setopt window-combination-resize t)

;; learn proper keybindings
(guru-global-mode 1)

;; (use-package dired
;;   :ensure nil
;;   :commands (dired dired-jump)
;;   :config
;;   (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;;   :custom
;;   ;; NOTE: these require GNU ls
;;   (dired-listing-switches "-agho --group-directories-first"))

(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-x-hands-off-my-keys nil)
  ;; dired cd with 'a' to reuse buffer
  ;; NOTE: this kills existing dired buffer so current directory is lost in dired
  (put 'dired-find-alternate-file 'disabled nil))

(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        ;; disable dictionary word completion in text modes
        text-mode-ispell-word-completion nil)

;; vertico (frontend / UI)
;; changes default completion buffer to vertical scrollable thing
(vertico-mode)

;; marginalia provides marninalia info to completions in minibuffer
(marginalia-mode)

;; in-buffer completion (completion-at-point)
;; dual wield completion-preview (ghost text) + corfu
;; ghost text works immediately by default
;; <TAB> completes ghost text
;; otherwise C-M-i cycles ghost text if there are <= 3 matches
;; otherwise a corfu popup is shown
(setopt tab-always-indent 'complete)
(setopt completion-preview-minimum-symbol-length 4)
(setopt completion-preview-exact-match-only nil)
(setopt completion-cycle-threshold 3)
(global-completion-preview-mode t)
(setopt corfu-quit-no-match t)
(global-corfu-mode)

;; cape
;; suite of completion-at-point functions
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)

;; orderless
;; decides how to match completion candidates
(setopt completion-styles '(orderless basic))
(setopt completion-category-overrides
        '((file (styles basic partial-completion))))

;;
;; ORG-MODE
;;
(setopt org-directory aa-org-directory)
(setopt org-hide-emphasis-markers t)
(setopt org-hide-leading-stars t)
(setopt org-ellipsis " ▾")
(setopt org-startup-truncated t)
(setopt org-startup-folded 'fold)
(setopt org-complete-tags-always-offer-all-agenda-tags t)
;; generate IDs to link to nodes linked interactively (via C-c l)
(setopt org-id-link-to-org-use-id 'create-if-interactive)
;; log time when todos are done
;; cf. https://orgmode.org/guide/Progress-Logging.html
(setopt org-log-into-drawer "LOGBOOK")
(setopt org-log-done 'time)
(setopt org-todo-keywords
      '((sequence "TODO(!)" "DONE(!)")))
;; capture setup
(setopt org-default-notes-file (file-name-concat org-directory "captured.org"))
(setopt aa-capture-templates-dir (file-name-concat user-emacs-directory "org-capture-templates"))
(setopt org-capture-templates
      `(("b" "book" entry
         (file ,(file-name-concat org-directory "finished-books.org"))
         (file ,(file-name-concat aa-capture-templates-dir "book"))
         :kill-buffer t)
        ("m" "mind dump" entry
         (file ,(file-name-concat org-directory "mind-dumps.org"))
         (file ,(file-name-concat aa-capture-templates-dir "dump"))
         :prepend t :kill-buffer t)
        ("j" "journal" entry
         (file ,(file-name-concat aa-org-journal-directory "journal.org"))
         (file ,(file-name-concat aa-capture-templates-dir "journal"))
         :prepend t
         :kill-buffer t)))
;; agenda uses everything under org-directory
(setopt org-agenda-files (list org-directory))
;; (add-to-list 'org-agenda-files org-directory)

(defun aa-org-common-hooks ()
  (custom-theme-set-faces
   'user
   '(org-document-title ((t . (:height 2.0))))
   '(org-level-1 ((t . (:height 1.7))))
   '(org-level-2 ((t . (:height 1.5))))
   '(org-level-3 ((t . (:height 1.3))))
   '(org-level-4 ((t . (:height 1.2))))
   '(org-level-5 ((t . (:height 1.1))))
   ))

(add-hook 'org-mode-hook #'aa-org-common-hooks)

(defun aa-org-toggle-markup ()
  "Toggle org-hide-emphasis-markers and refontify the buffer"
  (interactive)
  (setopt org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-fontify-buffer))

(defun aa-org-toggle-wrap ()
  "Disable line truncation and enable word wrap"
  (interactive)
  (toggle-truncate-lines (not truncate-lines))
  (toggle-word-wrap (not word-wrap)))

                                        ;
                                        ; C O D I N G
                                        ;

;; slime
(when (and
       (file-exists-p "/usr/bin/sbcl")
       (file-exists-p "~/src/3rdparty/slime"))
  (add-to-list 'load-path "~/src/3rdparty/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  ;; ;; Local CL HyperSpec
  ;; ;;
  ;; (let ((local-hyperspec-directory
  ;;        (expand-file-name "~/Documents/docs/Common Lisp HyperSpec/HyperSpec")))
  ;;   (if (file-exists-p local-hyperspec-directory)
  ;;       (progn
  ;;         (add-to-list
  ;;          'browse-url-handlers
  ;;          '(
  ;;            "file://.*HyperSpec/.*\\.htm"
  ;;            . (lambda (url &rest args) (eww url))))
  ;;         (setq common-lisp-hyperspec-root
  ;;               (concat "file://" local-hyperspec-directory)))))
  )

;; smartparens
(require 'smartparens-config)
(setopt sp-base-key-bindings 'sp)
(add-hook 'lisp-data-mode-hook #'smartparens-strict-mode)

;; EGLOT
;; reduce number of reads for large language server responses
(setq read-process-output-max (* 4 1024 1024))

;; ;; the following assumes that language-servers are in $PATH
;; ;; eglot is opt-in (M-x eglot or whatever)
;; (use-package eglot
;;   :commands eglot
;;   ;; :custom
;;   ;; (eglot-ignored-server-capabilities
;;   ;;  ;; https://github.com/joaotavora/eglot/discussions/1393
;;   ;;  '(:documentOnTypeFormattingProvider))
;;   :config
;;   (dolist (mode `((nix-mode . ("nixd"))
;;                   (zig-mode . ("zls"))
;;                   ;; use uv run instead of venv
;;                   ((python-mode python-ts-mode) .
;;                    ,(eglot-alternatives
;;                      '(("uv" "run" "ty" "server"))))))
;;     (add-to-list 'eglot-server-programs mode)))

;; ;; let project.el recognize python project roots
;; (add-to-list 'project-vc-extra-root-markers "pyproject.toml")

                                        ;
                                        ; T E R M
                                        ;

;; keybindings
;; completions
(define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
(define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate)

;; dired
(defvar aa-leader-map-dired (make-sparse-keymap) "SPC d: Dired")
(keymap-global-set "C-c d" aa-leader-map-dired)
(define-key aa-leader-map-dired (kbd "d") #'dired)
(define-key aa-leader-map-dired (kbd "j") #'dired-jump)
(which-key-add-key-based-replacements
  "C-c d" "dired")
(which-key-add-key-based-replacements
  "C-c d j" "dired-jump"
  "C-c d d" "open dired")

;; org
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c l" #'org-store-link)
(which-key-add-key-based-replacements
  "C-c a" "org-agenda"
  "C-c c" "org-capture"
  "C-c l" "org-store-link")

(with-eval-after-load 'org
  (defvar aa-org-toggle-map (make-sparse-keymap) "C-c t: org toggles")
  (define-key org-mode-map (kbd "C-c t") aa-org-toggle-map)
  (define-key aa-org-toggle-map (kbd "l") #'org-toggle-link-display)
  (define-key aa-org-toggle-map (kbd "m") #'aa-org-toggle-markup)
  (define-key aa-org-toggle-map (kbd "w") #'aa-org-toggle-wrap)
  (which-key-add-key-based-replacements
    "C-c t" "org toggles"
    "C-c t l" "toggle link display"
    "C-c t m" "toggle markup display"
    "C-c t w" "toggle word line wrap"))

;; notes
(defvar aa-leader-map-notes (make-sparse-keymap) "C-c n: Notes")
(keymap-global-set "C-c n" aa-leader-map-notes)
(define-key aa-leader-map-notes
            (kbd "a")
            `(lambda ()
               (interactive)
               (view-file ,(file-name-concat org-directory "all.org"))))
(which-key-add-key-based-replacements
  "C-c n a" "read all notes")



;; consult
(keymap-global-set "C-c f" #'consult-line)
(keymap-global-set "C-c g" #'consult-ripgrep)
(keymap-global-set "C-c o" #'consult-outline)
(keymap-global-set "C-c b" #'consult-buffer)
(which-key-add-key-based-replacements
  "C-c f" "search line"
  "C-c g" "ripgrep"
  "C-c o" "search outline"
  "C-c b" "select buffer")

;; cape
(keymap-global-set "C-c p" #'cape-prefix-map)

;; window management
(defvar aa-leader-map-windows (make-sparse-keymap) "SPC w: window management")
(keymap-global-set "C-c w" aa-leader-map-windows)
(which-key-add-key-based-replacements
  "C-c w" "window management")
;; switch to other window even in a different frame
(keymap-set aa-leader-map-windows "o" #'next-multiframe-window)
(which-key-add-key-based-replacements
  "C-c w o" "next window")

;; enable reversible C-x 1 via winner-mode
(keymap-global-set "C-x 1" #'aa-toggle-delete-other-windows)

;; terminals
(keymap-global-set "C-c s" #'eshell)
(which-key-add-key-based-replacements
  "C-c s" "eshell")

;; isearch remaps
(keymap-global-set "C-s" #'isearch-forward-regexp)
(keymap-global-set "C-r" #'isearch-backward-regexp)
(keymap-global-set "C-M-s" #'isearch-forward)
(keymap-global-set "C-M-r" #'isearch-backward)

;; apropos
(keymap-global-set "C-h u" #'apropos-user-option)

;; TODO: remap defaults to better defaults
;; via https://www.matem.unam.mx/~omar/apropos-emacs.html#underappreciated-emacs-built-ins
(keymap-global-set "<remap> <forward-word>" #'forward-to-word)
;; backward-word -> backward-to-word
;; up-list: love this command: it puts point after the current s-expression
(keymap-global-set "<remap> <delete-char>" #'delete-forward-char)

(server-start)
;; recondiser configuration if this exceeds 500 lines
;; -- init.el ends here --
