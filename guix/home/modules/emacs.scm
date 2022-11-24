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
  #:use-module (gnu home-services-utils)
  )

(define-public emacs-packages
  (map specification->package
       (list
        "emacs-vertico"
        "emacs-company"
        "emacs-evil")))


(define-public emacs-services
  (list
    (service home-emacs-service-type
             (home-emacs-configuration
              ;; (package emacs-next-pgtk-treesitter)
              (rebuild-elisp-packages? #t)
              (elisp-packages emacs-packages)
              ;; (init-el
              ;;  (list (slurp-file-like (local-file "../files/init.el"))))
              ;; (early-init-el
              ;;  (list (slurp-file-like (local-file "../files/early-init.el")))))
             ))))
