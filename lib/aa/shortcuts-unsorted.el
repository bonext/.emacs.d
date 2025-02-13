;; -*- lexical-binding: t; -*-
(require 'aa/leader)
                                        ; package management

(defun aa/recompile-all-packages nil
  (interactive)
  (message "recompiling elpa/ contents")
  (native-compile-async (file-name-concat user-emacs-directory "elpa") t))

(aa/with-leader
  :states 'normal
  :keymaps 'override
  "p" '(:ignore t :which-key "packages")
  "pl" #'list-packages
  "pr" #'aa/recompile-all-packages)

                                        ; unsorted goodies

;; switch to other window even in a different frame
(aa/with-leader
  :states 'normal
  :keymaps 'override
  "w" '(:ignore t :which-key "window")
  "wo" #'next-multiframe-window)

(provide 'aa/shortcuts-unsorted)
