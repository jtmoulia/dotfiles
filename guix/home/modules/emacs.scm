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
        "emacs-company"
        "emacs-dracula-theme"
        "emacs-evil"
        "emacs-guix"
        "emacs-vertico")))

(define-public emacs-services
  (list
    (service home-emacs-service-type
             (home-emacs-configuration
              (rebuild-elisp-packages? #t)
              (elisp-packages emacs-packages)))))
