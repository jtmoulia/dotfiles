;;; ~/.dotfiles/playbooks/roles/emacs/files/doom.d/config/python.el -*- lexical-binding: t; -*-

;; Configure blacken mode for black autoformatting
(setq blacken-line-length 108)

;; Configure the Python Language Server (pyls)
;; Use flake8 over autopep8
(setq lsp-pyls-plugins-flake8-enabled t)
(setq lsp-pyls-plugins-pycodestyle-enabled nil)
(setq lsp-pyls-plugins-autopep8-enabled nil)

;;; python.el ends here
