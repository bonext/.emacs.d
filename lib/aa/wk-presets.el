;; -*- lexical-binding: t; -*-
;; show help on key prefix after `which-key-idle-delay` seconds
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
(provide 'aa/wk-presets)
