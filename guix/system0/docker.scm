(define-module (system0 docker)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services docker))

(define-public docker-packages
  (map specification->package
       (list
        ;; a whale of container runtimes
        ;; https://www.docker.com/
        "docker"
        ;; CLI interface for the docker whale
        "docker-cli")))

(define-public docker-services
  (list
   (service docker-service-type)))
