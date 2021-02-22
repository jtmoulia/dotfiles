;;; ~/.dotfiles/playbooks/roles/emacs/files/doom.d/config/python.el -*- lexical-binding: t; -*-

;; Configure blacken mode for black autoformatting
(setq blacken-line-length 108)

;; Configure the Python Language Server (pyls)
;; Use flake8 over autopep8
(setq lsp-pyls-plugins-flake8-enabled t)
(setq lsp-pyls-plugins-pycodestyle-enabled nil)
(setq lsp-pyls-plugins-autopep8-enabled nil)


(defun my//lsp-query-tcp-connection ()
  (list
   :connect (lambda (filter _sentinel name)
              (let* ((host "localhost")
                     (port (string-to-number (read-string "Enter port: ")))
                     (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc nil)))
   :test? (lambda () t)))


;; Create an automed python language server
;; (lsp-register-client
;;  (make-lsp-client :new-connection (my//lsp-query-tcp-connection)
;;                   :major-modes '(python-mode cython-mode)
;;                   :priority 0
;;                   :server-id 'automed-pyls
;;                   :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
;;                   :initialized-fn (lambda (workspace)
;;                                     (with-lsp-workspace workspace
;;                                       (lsp--set-configuration (lsp-configuration-section "pyls"))))
;;                   ))
;;; python.el ends here
