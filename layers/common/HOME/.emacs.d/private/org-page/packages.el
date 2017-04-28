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
      (defvar my-op-sites nil
        "The alist of site configurations.")

      (defvar my-op-site nil
        "The active site configuration.")

      (defvar my-op-site-policy 'my-op/ask-if-nil
        "The policy for selecting `my-op-site'.")

      (defvar my-op-force-all t
        "Whether to force rebuilding all files by default when publishing.")

      ;; Helper Functions

      (defun my-op//apply-variables (vars)
        (-each vars
          (lambda (var)
            (let ((name (car var))
                  (value (cdr var)))
              (set name value))))
        vars)

      (defun my-op//get-vars (names)
        "Return an alist of variable `(name . value)' for NAMES."
        (-map
         (lambda (name)
           ;; TODO a value other than `nil' should be used so the var may be unbound
           (let ((value (if (boundp name) (eval name) nil)))
             `(,name . ,value)))
         names))

      ;; (defun my-op//apply-vars ()
      ;;   "Apply the org-page configuration."
      ;;   (let ((vars jtsite/vars-alist)) ;; TODO: overlay config
      ;;     (my-op//apply-variables vars)
      ;;     vars))

      (defun my-op//read-site ()
        "Helper function for reading a SITE given `my-op-sites'."
        (intern-soft
         (completing-read "Site: "
                          (-map (function car) my-op-sites))))

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

      (defun my-op/ask-if-nil ()
        "Return the current site. This is `my-op-project' if it is truthy,
else it asks for and sets the active site."
        (if my-op-site
            my-op-site
          (setq my-op-site (my-op//read-site))))

      (defun my-op/site ()
        "Get the current site by calling `my-op-project-policy'."
        (funcall my-op-site-policy))

      (defun my-op/select (&optional site)
        "Select SITE by applying its configuration. Returns `nil' if
SITE is invalid.

See `my-op-sites'."
        (interactive (list (my-op/proj)ect))
        (let ((vars (cdr (assoc site my-op-sites))))
          (if vars (my-op//apply-variables vars))))

      (defun my-op/do-publication (&optional site force-all base-git-commit pub-base-dir auto-commit auto-push)
        "Publish the SITE given the settings."
        (interactive (list (my-op/site)))
        (my-op/select site)
        (my-op|with-default-directory op/repository-directory
                                      (op/do-publication force-all
                                                         base-git-commit
                                                         pub-base-dir
                                                         auto-commit
                                                         auto-push)))

      (defun my-op/publish-to-build (&optional site)
        "Publish SITE to the build directory."
        (interactive (list (my-op/site)))
        (my-op/do-publication site my-op-force-all nil "_build" nil nil))

      (defun my-op/publish-to-master (&optional site)
        (interactive (list (my-op/site)))
        (my-op/do-publication site my-op-force-all nil nil nil nil))

      )))
