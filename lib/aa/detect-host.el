;; -*- lexical-binding: t; -*-
(let ((aa/host (cond
                ((and (eq system-type 'gnu/linux)
                      (string-match "-[Mm]icrosoft" operating-system-release))
                 'wsl)
                ((eq system-type 'darwin) 'work)
                (t 'home))))
  (defun aa/home-p ()
    (eq aa/host 'home))
  (defun aa/work-p ()
    (eq aa/host 'work))
  (defun aa/wsl-p ()
    (eq aa/host 'wsl)))
(provide 'aa/detect-host)
