;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/elfeed.el -*- lexical-binding: t; -*-

;; default to look 6 mo's back and show unread
(setq elfeed-search-filter "@6-months-ago +unread")

;; automatically update the elfeed when opened
(add-hook 'elfeed-search-mode-hook #'elfeed-update)
