                                        ; ORG-MODE
(setq org-directory "~/Documents/Notes")

;; populate org-agenda
(load-library "find-lisp")
(defun org-reload-my-agenda-files ()
  (setq org-agenda-files
        (append
         (find-lisp-find-files org-directory "\.org$"))))

;;; fonts and colors
;;; via https://yannesposito.com/posts/0020-cool-looking-org-mode/index.html
;;; and https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;;; make bold show bold and not *bold*
(setq org-hide-emphasis-markers t)
;;; variable pitch
(add-hook 'org-mode-hook 'variable-pitch-mode)
;;; set specific fonts for stuff
;;; use (font-family-list) and C-j in scratch to list fonts
(defun org-reset-my-fonts ()
  (if (display-graphic-p)
      (let* ((variable-tuple
              (cond
               ((x-list-fonts "PT Serif") '(:family "PT Serif" :height 120))))
             (fixed-tuple
              (cond
               ((x-list-fonts "Adobe Source Pro") '(:family "Adobe Source Pro"))
               ((x-list-fonts "IBM Plex Mono") '(:family "IBM Plex Mono"))
               ((x-list-fonts "PT Mono") '(:family "PT Mono"))))
             (base-font-color (face-foreground 'default nil 'default))
             (headline `(:inherit default :weight bold :foreground ,base-font-color)))
        (custom-theme-set-faces
         'user
         `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
         `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
         `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
         `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
         `(org-level-5 ((t (,@headline ,@variable-tuple))))
         `(org-level-6 ((t (,@headline ,@variable-tuple))))
         `(org-level-7 ((t (,@headline ,@variable-tuple))))
         `(org-level-8 ((t (,@headline ,@variable-tuple))))
         `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))
         `(variable-pitch     ((t ,@variable-tuple)))
         `(fixed-pitch        ((t ,@fixed-tuple)))

         '(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray40" :underline nil))))
         '(org-block            ((t (:inherit fixed-pitch))))
         '(org-block-begin-line ((t (:inherit fixed-pitch))))
         '(org-block-end-line   ((t (:inherit fixed-pitch))))
         '(org-src              ((t (:inherit fixed-pitch))))
         '(org-properties       ((t (:inherit fixed-pitch))))
         '(org-code             ((t (:inherit (shadow fixed-pitch)))))
         '(org-date             ((t (:inherit (shadow fixed-pitch)))))
         '(org-document-info    ((t (:inherit (shadow fixed-pitch)))))
         '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
         '(org-drawer           ((t (:inherit (shadow fixed-pitch)))))
         '(org-indent           ((t (:inherit (org-hide fixed-pitch)))))
         `(org-link             ((t (:inherit fixed-pitch :foreground ,base-font-color :underline t))))
         '(org-meta-line        ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-property-value   ((t (:inherit fixed-pitch))) t)
         '(org-special-keyword  ((t (:inherit (font-lock-comment-face fixed-pitch)))))
         '(org-table            ((t (:inherit fixed-pitch))))
         '(org-tag              ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
         '(org-verbatim         ((t (:inherit (shadow fixed-pitch)))))))))

;;; enable scaling of inline images with attr_org width
(setq org-image-actual-width nil)

;;; show inline images by default
(setq org-startup-with-inline-images t)

;;; capture setup
(setq org-default-notes-file (concat org-directory "/captured.org"))

;; Make C-c a t reuse current window
(setq org-agenda-window-setup 'current-window)

;; offer tags from all agenda files (not current buffer)
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; wrap text by default
(setq org-startup-truncated nil)

;; log time when todos are done
;; cf. https://orgmode.org/guide/Progress-Logging.html
(setq org-log-into-drawer "LOGBOOK")
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(!)" "DONE(!)")))

(setq org-refile-targets
      '((nil :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(setq org-my-capture-templates-dir (concat user-emacs-directory "org-capture-templates"))
(setq org-capture-templates
      '(("b" "book" entry
         (file "~/Documents/Notes/finished-books.org")
         (file "~/.emacs.d/book")
         :kill-buffer t)
        ("d" "mind dump" entry
         (file "~/Documents/Notes/mind-dumps.org")
         (file "~/.emacs.d/dump")
         :prepend t :kill-buffer t)))

;;; borrowed from suggested shortcut keys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c r") 'org-reload-my-agenda-files)

(org-reset-my-fonts)
(org-reload-my-agenda-files)

;; org-bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-journal
(use-package org-journal
  :init
  (setq org-journal-file-type 'daily)
  (setq org-journal-dir "~/Documents/journal")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-file-format "%F.org") ; yyyy-mm-dd.org
  (setq org-journal-encrypt-journal t)
  :config
  (global-set-key (kbd "C-c j") 'org-journal-new-entry))
