;;; packages.el --- pdf-tools Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq pdf-tools-packages '(pdf-tools))

;; List of packages to exclude.
(setq pdf-tools-excluded-packages '())

(defun pdf-tools/init-pdf-tools ()
  "Initialize pdf-tools."
  :config
  (message "config'ing"))
