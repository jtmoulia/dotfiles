(define-module (home modules virtualization)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu services virtualization))

(define-public virtualization-packages
  (map specification->package
       (list
        "libvirt")))

(define-public virtualization-services
  (list
   (service libvirt-service-type
            (libvirt-configuration))))
