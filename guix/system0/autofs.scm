(define-module (system0 autofs)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))
(use-service-modules admin shepherd)
(use-package-modules linux nfs onc-rpc file-systems)

;; a stripped down version of autofs:
(define-public my-autofs
  (package
    (inherit autofs)
    (name "my-autofs")
    (arguments
     `(,@(substitute-keyword-arguments
             (package-arguments autofs)
           ((#:configure-flags cf)
            `(list "--enable-ignore-busy"
                   "--enable-sloppy-mount"
                   "--with-libtirpc")))))
    (inputs
     `(("e2fsprogs" ,e2fsprogs)
       ("libtirpc" ,libtirpc)
       ("nfs-utils" ,nfs-utils)
       ("util-linux" ,util-linux)))))

(define-public (automount-shepherd-service config)
  (list (shepherd-service
         (provision '(automount))
         (documentation "Run the automount server.")
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   (list #$(file-append my-autofs "/sbin/automount") "-f")
                   #:log-file "/var/log/automount.log"))
         (stop #~(make-kill-destructor)))))

(define-public automount-service-type
  (service-type
   (name 'automount)
   (description "Run the automount server.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             automount-shepherd-service)
          (service-extension rottlog-service-type
                             (const
                              (list (log-rotation
                                     (files (list "/var/log/automount.log"))))))
          (service-extension etc-service-type
                        (lambda (config)
                          `(
                            ("auto.media" ,(local-file "files/autofs/auto.media"))
                            ("auto.master" ,(local-file "files/autofs/auto.master")))))))
   (default-value '())))
