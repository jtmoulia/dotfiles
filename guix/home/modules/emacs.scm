(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home services shells)
  ;; from rde
  #:use-module (rde gexp)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define-public emacs-packages
  (map specification->package
       (list
        ;; for dtache emacs command detaching
        "dtach"
        "emacs-alert"
        "emacs-company"
        "emacs-docker"
        "emacs-dracula-theme"
        "emacs-evil"
        "emacs-hydra"
        "emacs-guix"
        "emacs-org"
        "emacs-org-modern"
        "emacs-org-pomodoro"
        "emacs-org-super-agenda"
        "emacs-pdf-tools"
        "emacs-s"
        "emacs-tldr"
        "emacs-treemacs"
        "emacs-vertico"
        "emacs-vterm")))

(define-public emacs-services
  (list
    (service home-emacs-service-type
             (home-emacs-configuration
              (rebuild-elisp-packages? #t)
              (elisp-packages emacs-packages)))))
