(setq aa/host
      (cond
       ((and (eq system-type 'gnu/linux)
             (string-match "-[Mm]icrosoft" operating-system-release))
        'wsl)
       ((eq system-type 'darwin) 'work)
       (t 'home)))
(provide 'aa/detect-host)
