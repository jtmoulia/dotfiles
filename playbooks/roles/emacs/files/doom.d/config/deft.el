;;; doom.d/config/deft.el -*- lexical-binding: t; -*-

;; Assumes that nextcloud is mounted to ~/ocloud and org/ is sync'd
(setq deft-directory "~/ocloud/org/notes")

;; Search down in deft sub-directories
(setq deft-recursive t)

;; bind normal mode `q' to quit
(map! :map deft-mode :n "q" #'kill-this-buffer)
