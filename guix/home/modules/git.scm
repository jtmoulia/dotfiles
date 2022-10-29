(define-module (home modules git)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  ; #:use-module (gnu home-services version-control)
  )

(define-public git-packages
  (list (specification->package "git")))

(define-public git-services
  (list
   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
                 ((name . "Thomas Moulia")
                  (email . "jtmoulia@gmail.com")))
                (github
                 ((user . "jtmoulia")))))
              ))))
