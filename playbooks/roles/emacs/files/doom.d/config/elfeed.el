;;; config/elfeed.el -*- lexical-binding: t; -*-

(require 'elfeed-tube)
(require 'f)

;; default to look 6 mo's back and show unread
(setq elfeed-search-filter "@6-months-ago +unread")

;; use synchronized folder for elfeed
(setq elfeed-db-directory (f-join my-sync-dir "elfeed.db"))

;; automatically update the elfeed when opened
(add-hook 'elfeed-search-mode-hook #'elfeed-update)

;; load and configure elfeed-tube
(elfeed-tube-setup)
(define-key elfeed-show-mode-map (kbd "F") 'elfeed-tube-fetch)
(define-key elfeed-show-mode-map [remap save-buffer] 'elfeed-tube-save)
(define-key elfeed-search-mode-map (kbd "F") 'elfeed-tube-fetch)
(define-key elfeed-search-mode-map [remap save-buffer] 'elfeed-tube-save)
