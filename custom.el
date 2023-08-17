                                        ; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-capture-templates
   '(("b" "book" entry
      (file "~/Documents/Notes/finished-books.org")
      (file "~/.emacs.d/org-capture-templates/book")
      :kill-buffer t)
     ("d" "mind dump" entry
      (file "~/Documents/Notes/mind-dumps.org")
      (file "~/.emacs.d/org-capture-templates/dump")
      :prepend t :kill-buffer t)))
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(vimrc-mode solarized-theme slime paredit org-journal markdown-mode lua-mode geiser-racket geiser-guile evil-org company base16-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
