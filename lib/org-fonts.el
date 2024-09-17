;; -*- lexical-binding: t; -*-
(cond
 ;; osx-specific stuff goes here
 (t (setq aa/org-font-height 140)))

(defun aa/org-setup-fonts ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; setup fonts
  ;; via https://yannesposito.com/posts/0020-cool-looking-org-mode/index.html
  ;; and https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (let* (
         ;;
         (font-height aa/org-font-height)
         ;; variable-width font setup
         (variable-tuple
          (cond
           ((x-list-fonts "PT Sans") `(:family "PT Sans" :height ,font-height))
           ((x-list-fonts "Source Sans Pro") `(:family "Source Sans Pro" :height ,font-height))
           ((x-list-fonts "IBM Plex Sans") `(:family "IBM Plex Sans" :height ,font-height))))
         ;; fixed-width font setup
         (fixed-tuple
          (cond
           ((x-list-fonts "Fira Code Retina") '(:family "Fira Code Retina"))
           ((x-list-fonts "Adobe Source Pro") '(:family "Adobe Source Pro"))
           ((x-list-fonts "IBM Plex Mono") '(:family "IBM Plex Mono"))
           ((x-list-fonts "PT Mono") '(:family "PT Mono"))))
         ;; store default font color to reset headlines
         (base-font-color (face-foreground 'default nil 'default))
         ;; store some settings for headlines
         (headline `(:inherit default :weight bold :foreground ,base-font-color)))
    
    (custom-theme-set-faces
     ;; 'user is "current user custom settings"
     'user

     ;; variable and fixed pitch fonts
     `(variable-pitch     ((t ,@variable-tuple)))
     `(fixed-pitch        ((t ,@fixed-tuple)))

     ;; headings
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.05))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))

     ;; things that should be fixed-pitch
     '(org-ellipsis ((t (:inherit fixed-pitch :foreground "gray60" :underline nil))))
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
     '(org-verbatim         ((t (:inherit (shadow fixed-pitch))))))))

;; hook this function to load-theme to avoid reloading emacs on theme switch
(add-hook 'aa/after-load-theme-hook #'aa/org-setup-font)
