;; colors
(load-theme 'tango-dark)

;;
;; packages
;;

(require 'package)

;; MELPA
(add-to-list 
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; stuff from teh internet
;; https://www.reddit.com/r/emacs/comments/4fqu0a/automatically_install_packages_on_startup/d2ba42o
;; === CUSTOM CHECK FUNCTION ===
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
   packages)
  )

;; === List my packages ===
;; simply add package names to the list
(ensure-package-installed
  'company
  ;; etc
)

;; company
(add-hook 'after-init-hook 'global-company-mode)


;; save position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
