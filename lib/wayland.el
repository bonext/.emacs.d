;; -*- lexical-binding: t; -*-
                                        ; WAYLAND SUPPORT

(defun aa/wayland-p ()
  "Check if running under wayland"
  ;; (display-graphic-p): https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html#index-display_002dgraphic_002dp
  ;; $WAYLAND_DISPLAY: https://unix.stackexchange.com/a/559950
  (and (display-graphic-p) (getenv "WAYLAND_DISPLAY")))

;; wayland clipboard support in terminal
;; credit: yorickvP on Github: https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
(defun aa/wayland-setup-clipboard ()
  (setq aa/wayland-copy-process nil)
  (defun aa/wayland-copy (text)
    (setq aa/wayland-copy-process (make-process :name "wl-copy"
                                           :buffer nil
                                           :command '("wl-copy" "-f" "-n")
                                           :connection-type 'pipe))
    (process-send-string aa/wayland-copy-process text)
    (process-send-eof aa/wayland-copy-process))
  (defun aa/wayland-paste ()
    (if (and aa/wayland-copy-process (process-live-p aa/wayland-copy-process))
        nil       ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'aa/wayland-copy)
  (setq interprogram-paste-function 'aa/wayland-paste))


(if (aa/wayland-p)
    (progn
      ;; setup interprogram clipboard
      (aa/wayland-setup-clipboard)
      
      ;; disable window decorations
      ;; cf. https://codeberg.org/river/wiki#woraround-for-emacs
      (setq default-frame-alist '((undecorated . t)))))
