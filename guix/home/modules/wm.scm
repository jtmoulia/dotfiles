(define-module (home modules wm)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

;; NOTE: foot is configured along with sway for simplicity's sake.

(define-public sway-packages
  (map specification->package
       (list
        "foot"
        "sway"
        "wofi")))

(define-public sway-services
  (list
   (simple-service 'my-sway-env-vars-service
                   home-environment-variables-service-type
                   '(("XKB_DEFAULT_OPTIONS" . "ctrl:nocaps,")))
   (simple-service 'my-sway-config-files-service
                   home-xdg-configuration-files-service-type
                   `(("sway/config" ,(local-file "../files/sway/config"))
                     ("foot/foot.ini" ,(local-file "../files/foot/foot.ini"))))))
