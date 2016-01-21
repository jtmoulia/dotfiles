;;; packages.el --- lfe Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lfe-packages
      '((lfe-mode :location local)))

(defun lfe/init-lfe-mode ()
  (use-package lfe-mode
   :commands (lfe-mode inferior-lfe)
   :init
   (progn
     (load "inferior-lfe")
     (add-to-list 'auto-mode-alist
                  '("\\.lfe\\'" . lfe-mode)))
   :config
   (progn
     (defun require-lfe-mode-hook ()
       ;; (load "inferior-lfe")
       (load "lfe-indent"))
     (add-hook 'lfe-mode-hook 'require-lfe-mode-hook))))
