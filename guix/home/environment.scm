;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.
;;
;; (set! %load-path (cons (getcwd) %load-path))

(define-module (home environment)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  ;; personal modules
  #:use-module (home modules emacs)
  #:use-module (home modules git)
  #:use-module (home modules guile)
  #:use-module (home modules shell))

(define base-packages
  (map specification->package
       (list
        "glibc-locales"
        "gnupg"
        "inetutils"
        "nss-certs"
        )))

(define term-fu-packages
  (map specification->package
       (list
        "bat"
        "curl"
        "exa"
        "fd"
        "glibc-locales"
        "inetutils"
        ;; TODO: ispell vs aspell
        "ispell"
        "neovim"
        "nss-certs"
        "make"
        "ripgrep"
        "tmux"
        )))


(define font-packages
  (map specification->package
       (list
        "font-awesome"
        "font-dejavu")))

;; TODO break these into sub-groups
(define desktop-packages
  (map specification->package
       (list
        "bluez"
        "evince"
        "evolution"
        "evolution-data-server"
        "firefox-wayland"
        "flatpak"
        "gnome"
        "gnome-keyring"
        "gnome-tweaks"
        "seahorse"
        "transmission-remote-gtk"
        "xdg-utils"
        )))

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
        ("reconfigure-home.sh" "bin/reconfigure-home")
        ("reconfigure-system.sh" "bin/reconfigure-system"))))
   (home-page "https://git.sr.ht/~jtmoulia/dotfiles")
   (synopsis "jtmoulia's scripts")
   (description "Some personal scripts for jtmoulia's system config.")
   (license license:expat)))

(home-environment
 (packages
  `(,@base-packages
    ,@term-fu-packages
    ,@zsh-packages
    ,@font-packages
    ; ,@git-packages
    ,home-scripts
    ))
 (services
  `(
    ,@emacs-services
    ,@zsh-services
    ; ,@git-services
    )))
