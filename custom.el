                                        ; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" default))
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
   '(nord-theme dts-mode nix-mode vimrc-mode solarized-theme slime paredit org-journal markdown-mode lua-mode geiser-racket geiser-guile evil-org company base16-theme)))
