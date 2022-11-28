(define-module (system0 sync)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services syncthing))

(define-public sync-packages
  (map specification->package
       (list
        "syncthing"
        "syncthing-gtk")))

(define-public sync-services
  (list
   (service syncthing-service-type
            (syncthing-configuration))))
