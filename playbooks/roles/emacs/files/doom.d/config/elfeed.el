;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/elfeed.el -*- lexical-binding: t; -*-

;; default to look 6 mo's back and show unread
(setq elfeed-search-filter "@6-months-ago +unread")

;; automatically update the elfeed when opened
(add-hook 'elfeed-search-mode-hook #'elfeed-update)

;; (elfeed-tube-setup)
;; (define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
;; (define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
;; (define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
;; (define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save)
