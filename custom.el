                                        ; CUSTOMIZE

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" default))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
