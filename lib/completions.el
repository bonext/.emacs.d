;; -*- lexical-binding: t; -*-

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

                                        ; completing-read

(use-package consult
  :bind
  ;; TODO: convert this to general as well
  ([remap switch-to-buffer] . consult-buffer)
  :general
  (aa/with-leader
    :states 'normal
    :keymaps 'override
    "/" #'consult-line
    "f" #'consult-ripgrep
    "o" #'consult-outline
    "b" #'consult-buffer))

                                        ; completion styling

;; orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))
