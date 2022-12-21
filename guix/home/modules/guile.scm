(define-module (home modules guile)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public guile-packages
  (map specification->package
       (list
        ;; Guile v3
        "guile-next"
        ;; Better REPL
        "guile-colorized"
        "guile-readline"
        ;; LLVM numerical arrays and tensors
        ;; TODO: Not working
        ; "guile-aiscm"
        ;; Guile computer vision lib
        ;; TODO: need to test
        ;; "guile-cv"
        ;; Guile project tooling
        "guile-hall"
        ;; A full quiver of SRFI's
        ;; Comparators
        "guile-srfi-128"
        ;; Mappings
        "guile-srfi-146"
        ;; Generators and Accumulators
        "guile-srfi-158"
        ;; Formatting combinators
        "guile-srfi-159"
        ;; JSON parser
        "guile-srfi-180"
        ;; Maybe / Either monads
        "guile-srfi-189"
        )))

(define-public guile-services
  (list
   (simple-service 'my-guile-config-files-service
                   home-files-service-type
                   `((".guile" ,(local-file "../files/guile/guile-init"))))))
