(define-module (home modules email)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public email-packages
  (map specification->package
       (list
        "offlineimap3"
        "mu")))

(define-public email-services
  (list
   (simple-service 'my-sway-config-files-service
                   home-xdg-configuration-files-service-type
                   `(("offlineimap/pass-get" ,(local-file "../files/offlineimap/pass-get.bash"))
                     ("offlineimap/offlineimap.py" ,(local-file "../files/offlineimap/offlineimap.py"))
                     ("offlineimap/config" ,(local-file "../files/offlineimap/offlineimaprc"))))))
