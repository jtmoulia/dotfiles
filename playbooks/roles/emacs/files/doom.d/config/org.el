;; Org

(setq-default org-agenda-files '("~/ocloud/org/agenda/"))

;; By default enable auto fill mode in org mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; By default enable flyspell mode in org mode buffers
(add-hook 'org-mode-hook 'flyspell-mode)
