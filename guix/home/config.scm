;; This "home-config" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.
;;
;; (set! %load-path (cons (getcwd) %load-path))

(define-module (home config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  ;; for dbus
  #:use-module (gnu home services desktop)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  ;; personal modules
  #:use-module (home modules emacs)
  #:use-module (home modules git)
  #:use-module (home modules guile)
  #:use-module (home modules shell)
  #:use-module (home modules wm))

(define audio-packages
  (map specification->package
       (list
        "pamixer"
        "pavucontrol"
        "playerctl")))

(define base-packages
  (map specification->package
       (list
        "glibc-locales"
        "gnupg"
        "inetutils"
        "nss-certs"
        )))

(define desktop-packages
  (map specification->package
       (list
        "freecad")))

(define python-packages
  (map specification->package
       (list
        "jupyter"
        "poetry"
        "python"
        "python-ipython"
        "python-jupyter-client"
        "python-jupyter-console"
        "python-matplotlib"
        "python-pandas"
        "python-pdbpp"
        "python-pytest"
        "python-seaborn")))

(define utility-packages
  (map specification->package
       (list
        "unzip")))

(define languages-packages
  (map specification->package
       (list
        ;; python: python-packages
        ;; guile: guile-packages
        "node"
        "ruby"
        "rust"
        "sbcl")))

(define term-fu-packages
  (map specification->package
       (list
        ;; an oxidized pager
        "bat"
        "curl"
        ;; an oxidized ls
        "exa"
        ;; an oxidized find
        "fd"
        "glibc-locales"
        ;; monitor processes
        "htop"
        ;; TODO: ispell vs aspell?
        "ispell"
        ;; system description utility (for fashion)
        "neofetch"
        "neovim"
        "make"
        ;; an oxidized grep
        "ripgrep"
        ;; an oxidized tldr
        ;; NOTE: tealdeer build is broke: https://issues.guix.gnu.org/57867
        ;; "tealdeer"
        ;; top for hunting power gremlins
        "powertop"
        ;;

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
        "firefox-wayland"
        "flatpak"
        "transmission-remote-gtk"
        )))

(define my-services
  (list
   (service home-dbus-service-type)))

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
        ("reconfigure-system.sh" "bin/reconfigure-system")
        ("backup-to-cogmind.sh" "bin/backup-to-cogmind"))))
   (home-page "https://git.sr.ht/~jtmoulia/dotfiles")
   (synopsis "jtmoulia's scripts")
   (description "Some personal scripts for jtmoulia's system config.")
   (license license:expat)))

(home-environment
 (packages
  `(,@audio-packages
    ,@base-packages
    ,@font-packages
    ,@python-packages
    ,@sway-packages
    ,@term-fu-packages
    ,@tmux-packages
    ,@utility-packages
    ,@zsh-packages
    ; ,@git-packages
    ,home-scripts
    ))
 (services
  `(
    ,@my-services
    ,@emacs-services
    ,@sway-services
    ,@tmux-services
    ,@zsh-services
    ; ,@git-services
    )))
