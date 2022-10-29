;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  ;; personal modules
  #:use-module (home modules git)
  #:use-module (home modules guile)
  #:use-module (home modules shell))

(define base-packages
  (map specification->package
       (list
        "alacritty"
        "bat"
        "exa"
        "ispell"
        "curl"
        "fd"
        "ripgrep"
        "neovim"
        "inetutils"
        "nss-certs"
        "glibc-locales")))

(define font-packages
  (map specification->package
       (list
        "font-awesome")))

(define desktop-packages
  (map specification->package
       (list
        "gnome"
        "gnome-keyring"
        "gnome-tweaks"
        "seahorse"
        "transmission-remote-gtk"
        "flatpak"
        "bluez")))

(define home-scripts
  (package
   (name "home-scripts")
   (version "0.1")
   (source (local-file "files/scripts"
                       #:recursive? #t))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      '(
        ("reconfigure.sh" "bin/reconfigure"))))
   (home-page "https://github.com/jtmoulia/dotfiles")
   (synopsis "jtmoulia's scripts")
   (description "Some personal scripts.")
   (license license:expat)))

(home-environment
 (packages
  `(,@base-packages
    ,@zsh-packages
    ,@font-packages
    ; ,@git-packages
    ,home-scripts
    ))
 (services
  `(
    ,@zsh-services
    ;; ,@git-services
    )))
