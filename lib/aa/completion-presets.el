;; -*- lexical-binding: t; -*-
(require 'aa/use-package-presets)
(require 'aa/leader)

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
  ;; :general
  ;; (aa/with-insert-leader
  ;;   :states 'insert
  ;;   "p" #'cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))


                                        ; completing-read

(use-package consult
  :bind
  ;; TODO: convert this to general as well
  ([remap switch-to-buffer] . consult-buffer)
  ;; :general
  ;; (aa/with-leader
  ;;   :states 'normal
  ;;   :keymaps 'override
  ;;   "/" #'consult-line
  ;;   "f" #'consult-ripgrep
  ;;   "o" #'consult-outline
  ;;   "b" #'consult-buffer)
  )

                                        ; completion styling

;; orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(provide 'aa/completion-presets)
