;; -*- lexical-binding: t; -*-

                                        ; minibuffer

;; vertico
;; changes default completion buffer to vertical scrollable thing
(use-package vertico
  :straight t
  :init
  (vertico-mode))

;; marginalia provides marninalia info to completions in minibuffer
;; annotations are per-category
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
