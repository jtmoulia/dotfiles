;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; apply the Python Black auto-formatter
;; Ideally this will be done by the Python LSP
(package! blacken)

;; reveal.js backend for org-mode
(package! ox-reveal)

;; mermaid-mode for working with mermaid graphs
(package! mermaid-mode)

;; multi-section org agenda buffers
;; https://github.com/alphapapa/org-super-agenda
(package! org-super-agenda)

;; org mode quick searching
;; https://github.com/alphapapa/org-ql
(package! org-ql)

;; org mode sidebar for quick browsing
;; https://github.com/alphapapa/org-sidebar
(package! org-sidebar)

;; org-page to build jtmoulia site
;; WARNING: this package is deprecated
(package! org-page)

;; Preview org files [as html]
;; https://github.com/jakebox/org-preview-html
(package! org-preview-html)

;; deft for quick nots and connections
;; https://jblevins.org/projects/deft/
(package! deft)

;; smart folding
;; https://github.com/gregsexton/origami.el
(package! origami)

;; desktop alerts with new mu4e emails
(package! mu4e-alert)

;; org driven mu4e dashboard
;; NOTE: package wasn't in a registry when added
(package! mu4e-dashboard :recipe (:host github :repo "rougier/mu4e-dashboard"))

;; display trees of LSP info in treemacs
;; https://github.com/emacs-lsp/lsp-treemacs
(package! lsp-treemacs)

;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit
;; and the rest of Emacs.
;; https://github.com/magit/forge
(package! forge)
