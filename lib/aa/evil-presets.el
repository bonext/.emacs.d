;; -*- lexical-binding: t; -*-
(use-package evil
  :init
  ;; the first two are due to evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; disable C-u prefix and scroll with it instead (like in vim)
  (setq evil-want-C-u-scroll t)
  ;; enable org-mode visibility cycle with tab
  (setq evil-want-C-i-jump nil)
  ;; setup undo-redo
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  ;; C-g does Insert -> Normal in evil
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; C-h in insert mode deletes back (as in vim)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; support visual-line-mode navigation
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(provide 'aa/evil-presets)
