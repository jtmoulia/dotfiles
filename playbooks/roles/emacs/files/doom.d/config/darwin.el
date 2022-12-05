;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/darwin.el -*- lexical-binding: t; -*-

;; MacOS specific config
;;
;; This script is intended to be wrapped in a darwin check, e.g.
;;
;;     `(if (eq system-type 'darwin) (load-file "darwin.el"))'
;;

(let ((mu4e-load-path (-> "/opt/homebrew/Cellar/mu/*"
                              file-expand-wildcards reverse first
                              file-name-as-directory (concat "share/emacs/site-lisp/mu/mu4e"))))
      (if (file-directory-p mu4e-load-path)
          (add-to-list 'load-path mu4e-load-path)))

;; Use command key as meta, preserve behavior of option key
(setq mac-option-modifier 'nil)
(setq mac-command-modifier 'meta)
