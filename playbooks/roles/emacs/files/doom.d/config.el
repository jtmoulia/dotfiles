;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Setup paths
(defun personal//join-path (root component)
  (concat (file-name-as-directory root) component))

(defun personal//eval-config-after-load (mode config)
  (let ((path (personal//join-path "~/.doom.d" config)))
    (eval `(after! ,mode (load-file ,path)))))

;; Evil config
;; TODO this should likely be ported to its own config file
(after! 'evil-escape
  ;; some heavy modes make the default delay of 0.15s too short
  (setq evil-escape-delay 0.5))

(load-file (personal//join-path "~/.doom.d" "config/text.el"))
(personal//eval-config-after-load 'ansible "config/ansible.el")
(personal//eval-config-after-load 'js-mode "config/js-mode.el")
(personal//eval-config-after-load 'mu4e "config/mu4e.el")
(personal//eval-config-after-load 'org "config/org.el")
(personal//eval-config-after-load 'python "config/python.el")
(personal//eval-config-after-load 'sql "config/sql.el")
(personal//eval-config-after-load 'web-mode "config/web-mode.el")

;; style and fonts. looking good, feeling good
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Fira Code" :size 14))

;; Spacemacs style `,' local leader.
(setq doom-localleader-key ",")

;; Allow deeper callstacks
(setq max-specpdl-size 10000)
