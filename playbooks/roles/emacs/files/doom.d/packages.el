;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Hyperbole for adding implicit links
(package! hyperbole)

;; multi-section org agenda buffers
;; https://github.com/alphapapa/org-super-agenda
(package! org-super-agenda)

;; an org query language, including search commands and saved views
;; https://github.com/alphapapa/org-ql
(package! org-ql)

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

;; elfeed for youtube
(package! elfeed-tube)
(package! elfeed-tube-mpv)

;; gopher / gemini
(package! elpher)

;; Extempore-mode for extemporaneous music
;; Extempore: https://github.com/digego/extempore
;; (package! extempore-mode)

;; create links with auto backlinks
;; https://github.com/toshism/org-super-links
(package! chatgpt-shell)

;; Unofficial github copilot for emacs
;; https://github.com/zerolfx/copilot.el
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
