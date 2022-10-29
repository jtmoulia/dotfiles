(define-module (home modules shell)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells))

(define-public zsh-packages
  (map specification->package
       (list
        "zsh"
        "zsh-autosuggestions"
        "fzf"
        "zoxide")))

(define-public zsh-services
  (list
   (service home-zsh-service-type
            (home-zsh-configuration
             (xdg-flavor? #t)
             (environment-variables
              '(("EDITOR" . "nvim")
                ("XCURSOR_THEME" . "Nordzy-cursors")
                ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
                ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")))
             (zshrc
              (list
               (local-file "../files/zshrc")))))))
