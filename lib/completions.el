;; -*- lexical-binding: t; -*-

                                        ; minibuffer

;; vertico (frontend)
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

                                        ; in-buffer

;; corfu (frontend)
(use-package corfu
  :straight t
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
