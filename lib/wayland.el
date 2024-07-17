                                        ; WAYLAND SUPPORT

;; wayland clipboard support in terminal
;; credit: yorickvP on Github: https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
;; (display-graphic-p): https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html#index-display_002dgraphic_002dp
;; $WAYLAND_DISPLAY: https://unix.stackexchange.com/a/559950
(if (and (display-graphic-p) (getenv "WAYLAND_DISPLAY"))
    (progn
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            :command '("wl-copy" "-f" "-n")
                                            :connection-type 'pipe))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil   ; should return nil if we're the current paste owner
          (shell-command-to-string "wl-paste -n | tr -d \r")))
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))
