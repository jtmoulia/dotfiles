;;; packages.el --- org-page Layer packages File for Spacemacs
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

(setq org-page-packages
    '(org-page dash))

(defun org-page/init-org-page ()
  "Initialize the org-page package."
  (use-package org-page
    :commands (op/do-publication
               op/do-publication-and-preview-site
               my-op/select
               my-op/publish-to-build)
    :init
    (progn
      (defvar my-op-projects-alist nil
        "The alist of project configurations.")

      ;; Helper Functions

      (defun my-op//apply-variables (vars)
        (-each vars
          (lambda (var)
            (let ((name (car var))
                  (value (cdr var)))
              (set name value)))))

      (defun my-op//get-vars (names)
        "Return an alist of variable `(name . value)' for NAMES."
        (-map
         (lambda (name)
           ;; TODO a value other than `nil' should be used so the var may be unbound
           (let ((value (if (boundp name) (eval name) nil)))
             `(,name . ,value)))
         names))

      (defun my-op//apply-vars (&optional vars)
        "Apply the org-page configuration."
        (let ((vars jtsite/vars-alist)) ;; TODO: overlay config
          (my-op//apply-variables vars)
          vars))

      (defun my-op//read-project ()
        "Helper function for reading a PROJECT given `my-op-projects-alist'."
        (completing-read "Project: "
                         (-map (function car) my-op-projects-alist)))

      ;; Helper Macros

      (defmacro my-op|with-vars (vars form)
        "Not Used: apply VARS overlay, restoring the original variables
after evaluating form."
        `(let ((old-vars my-op//get-vars ,(-map (function car) vars)))
           (my-op//apply-vars ,vars)
           ;; TODO error handling when body fails
           ,form
           (my-op//apply-vars old-vars)))

      (defmacro my-op|with-default-directory (directory form)
        `(let ((old-default-directory default-directory))
           (setq default-directory ,directory)
           (let ((result ,form))
             (setq default-directory old-default-directory)
             result)))

      ;; Public Interface

      (defun my-op/select (&optional project)
        "Select PROJECT by applying its configuration. Returns `nil' if
PROJECT is invalid.

See `my-op-projects-alist'."
        (interactive (list (my-op//read-project)))
        (let ((vars (assoc project my-op-projects-alist)))
          (if vars (my-op//apply-vars vars))))

      (defun my-op/do-publication (&optional project force-all base-git-commit pub-base-dir auto-commit auto-push)
        "Publish the PROJECT given the settings."
        (interactive (list (my-op//read-project)))
        (my-op/select project)
        (my-op|with-default-directory op/repository-directory
                                      (op/do-publication force-all
                                                         base-git-commit
                                                         pub-base-dir
                                                         auto-commit
                                                         auto-push)))

      (defun my-op/publish-to-build (&optional project)
        "Publish PROJECT to the build directory."
        (interactive (list (my-op//read-project)))
        (my-op/do-publication project t nil "_build" nil nil))

      (defun my-op/publish-to-master (&optional project)
        (interactive (list (my-op//read-project)))
        (my-op/do-publication project nil nil nil nil nil))

      )))
