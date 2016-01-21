;;; extensions.el --- mu4e Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Thomas Moulia <jtmoulia@pocketknife.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mu4e-post-extensions '(mu4e))
(setq mu4e-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e/")

(defun mu4e/init-mu4e ()
  "mu4e configuration."
  (use-package mu4e
    :commands mu4e
    :init
    (progn
      (add-to-list 'load-path mu4e-path)
      (evil-leader/set-key "am" 'mu4e)
      ;; TODO: Turn on with org-mode?
      (require 'org-mu4e)
      (if (fboundp 'dotmu4e/init) (dotmu4e/init)))
    :config
    (progn
      (setq-default
       mu4e-headers-skip-duplicates t
       mu4e-compose-dont-reply-to-self t
       mu4e-compose-complete-only-personal t
       mu4e-hide-index-messages t)

      ;; drafts are saved as *message*-
      (add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))

      ;; key bindings
      (evilify mu4e-main-mode mu4e-main-mode-map
               ;; TODO: mu4e-main-mode `[j]ump' is hardwired + wrong
               "J" 'mu4e~headers-jump-to-maildir)

      (evilify mu4e-headers-mode mu4e-headers-mode-map
               "J"    'mu4e~headers-jump-to-maildir
               "\C-j" 'mu4e-headers-next
               "\C-k" 'mu4e-headers-prev)

      (evilify mu4e-view-mode mu4e-view-mode-map
               "J"    'mu4e~headers-jump-to-maildir
               "\C-j" 'mu4e-view-headers-next
               "\C-k" 'mu4e-view-headers-prev)

      (if (fboundp 'dotmu4e/config) (dotmu4e/config))
      )))
