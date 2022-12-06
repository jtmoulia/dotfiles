;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; multi-section org agenda buffers
;; https://github.com/alphapapa/org-super-agenda
(package! org-super-agenda)

;; create links with auto backlinks
;; https://github.com/toshism/org-super-links
(package! org-super-links
  :recipe (:type git
           :host github
           :repo "toshism/org-super-links"
           :branch "develop"))

;; an org query language, including search commands and saved views
;; https://github.com/alphapapa/org-ql
(package! org-ql)

;; org-page to build jtmoulia site
;; WARNING: this package is deprecated and in the emacs orphanage
(package! org-page)

;; Preview org files [as html]
;; https://github.com/jakebox/org-preview-html
(package! org-preview-html)

;; Temporary blacken as it seems to have been removed from python layer
;; TODO: See if LSP can do the trick
(package! blacken)

;; org driven mu4e dashboard
;; NOTE: package wasn't in a registry when added
(package! mu4e-dashboard :recipe (:host github :repo "rougier/mu4e-dashboard"))

;; Helpers for adding / removing guix packages
(package! guix)

;; slow chats
(package! mastodon)

;; detached for dtach'ing commands to be run in the BG
;; Depends on dtach utility
;; https://git.sr.ht/~niklaseklund/detached.el
(package! detached)

;; Emacs TLDR implementation
(package! tldr)
