;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Setup paths
(defun personal//join-path (root component)
  (concat (file-name-as-directory root) component))

(defun personal//eval-config-after-load (mode config)
  (let ((path (personal//join-path "~/.doom.d" config)))
    (eval `(after! ,mode (load-file ,path)))))

(personal//eval-config-after-load 'web-mode "config/web-mode.el")
(personal//eval-config-after-load 'js-mode "config/js-mode.el")
;; (personal//eval-config-after-load 'eshell "config/eshell.el")
(personal//eval-config-after-load 'org "config/org.el")
(personal//eval-config-after-load 'mu4e "config/mu4e.el")
;; (personal//eval-config-after-load 'prodigy "config/prodigy.el")
;; superseded by doom's default python configuration
;; (personal//eval-config-after-load 'python "config/python.el")
(personal//eval-config-after-load 'ansible "config/ansible.el")

;; style and fonts. looking good, feeling good
;; (setq doom-theme 'doom-one-light)
(setq doom-font (font-spec :family "Fira Code" :size 14))

;; Spacemacs style `,' local leader.
(setq doom-localleader-key ",")

;; Allow deeper callstacks
(setq max-specpdl-size 10000)
