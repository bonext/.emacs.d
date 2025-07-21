;; -*- lexical-binding: t; -*-
(require 'aa/use-package-presets)

                                        ; emacs built-in

;; disable ispell completion
(setopt text-mode-ispell-word-completion nil)

                                        ; minibuffer

;; vertico (frontend)
;; changes default completion buffer to vertical scrollable thing
(use-package vertico
  :init
  (vertico-mode))

;; marginalia provides marninalia info to completions in minibuffer
;; annotations are per-category
(use-package marginalia
  :init
  (marginalia-mode))

;; ignore case for file name completions
(setopt read-file-name-completion-ignore-case t)


                                        ; in-buffer

;; corfu (frontend)
(use-package corfu
  :custom
  ;; enable auto completion
  (corfu-auto t)
  ;; close corfu buffer if no completion matches
  (corfu-quit-no-match t)
  ;; delay in seconds before completion
  (corfu-auto-delay 0.2)
  ;; prefix for completion
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode))

;; cape (completion-at-point extensions)
;; use prefix map for now
(use-package cape
  :commands cape-prefix-map
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))


                                        ; completing-read

(use-package consult
  :commands (consult-line
             consult-ripgrep
             consult-outline
             consult-buffer)
  :config
  ([remap switch-to-buffer] . consult-buffer))

                                        ; completion styling

;; orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(provide 'aa/completion-presets)
