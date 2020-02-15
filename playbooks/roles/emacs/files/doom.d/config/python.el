;; Python
(defun python-use-tabs ()
  (interactive)
  (setq indent-tabs-mode t
        tab-width 4
        py-indent-tabs-mode t))


;; (add-hook 'python-mode-hook
;;           (lambda () (setq-local imenu-create-index-function 'python-imenu-create-index)))

;; Some extra LSP keybindings
(if (boundp 'lsp-ui-doc-show)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "Tp" 'lsp-ui-doc-show))
(if (boundp 'lsp-ui-doc-hide)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "TP" 'lsp-ui-doc-hide))

;; Configure black
(if (boundp 'blacken-line-length)
    (setf black-line-length 108))
