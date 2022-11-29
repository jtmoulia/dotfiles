(define-module (system0 pd)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services pm))

(define-public pd-packages
  (map specification->package
       (list "tlp")))

(define-public pd-services
  (list
   (service tlp-service-type
            (tlp-configuration
             (cpu-scaling-governor-on-ac (list "performance"))
             (sched-powersave-on-bat? #t)))))
