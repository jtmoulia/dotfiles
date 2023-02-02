;; Configure company mode

;; Disable company triggering after an idle period. It slows emacs and results
;; in mis-hits when you delay just a lil too long
(setq company-idle-delay nil)
(setq company-tooltip-idle-delay nil)

;; Unbind `ret' as company completion because it breaks inferior modes
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "t") nil)

;; Bind `tab' to selection complete
(define-key company-active-map (kbd "tab") 'company-complete-selection)
(define-key company-active-map [tab] 'company-complete-selection)

;; company.el ends here
