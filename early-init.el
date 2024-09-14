;; -*- lexical-binding: t; -*-
;; cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; "This file is loaded before the package system and GUI is initialized, so in it you can customize variables that affect the package initialization process..."

;; disable package.el because of staight.el
;; cf. https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
(setq package-enable-at-startup nil)
