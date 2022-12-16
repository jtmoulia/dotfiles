(define-module (home modules guile)
  #:use-module (gnu packages))

(define-public guile-packages
  (map specification->package
       (list
        ;; Guile v3
        "guile-next"
        ;; Better REPL
        "guile-colorized"
        "guile-readline"
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
