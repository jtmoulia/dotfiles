;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; apply the Python Black auto-formatter
;; Ideally this will be done by the Python LSP
(package! blacken)

;; reveal.js backend for org-mode
(package! ox-reveal)

;; multi-section org agenda buffers
(package! org-super-agenda)

;; desktop alerts with new mu4e emails
(package! mu4e-alert)
