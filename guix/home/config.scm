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
  #:use-module (home modules email)
  #:use-module (home modules git)
  #:use-module (home modules guile)
  #:use-module (home modules shell)
  #:use-module (home modules wm))

(define audio-packages
  (map specification->package
       (list
        "pamixer"
        "pavucontrol"
        "playerctl"
        )))

(define pro-audio-packages
  (map specification->package
       (list
        "calf"
        "extempore"
        "helm"
        "lilypond"
        "orca-music"
        ;; pipewire source/sink graph interface
        "qpwgraph"
        ;; Supercollider along wtih emacs editor
        ;; "supercollider"
        ;; "emacs-scel"
        ;; Open source digital audio workstation along with its plugins (AGPL3+)
        ;; "zrythm"
        ;; "zplugins"
        )))

(define ml-packages
  (map specification->package
       (list
        "blis"
        "openblas"
        "lapack"
        )))

(define base-packages
  (map specification->package
       (list
        "glibc-locales"
        "gnupg"
        "pinentry"
        "inetutils"
        "nss-certs"
        ;; as it says on the tin, tools for working with wireguard
        ;; TODO: is this redundant with nm-cli
        "wireguard-tools"
        ;; Fix for texlive build failure: https://issues.guix.gnu.org/63043
        "texlive-scheme-basic"
        )))

(define python-packages
  (map specification->package
       (list
        ;; the OG journal
        "jupyter"
        ;; python packaging, current best option
        ;; Broken build see https://issues.guix.gnu.org/63139
        ;; "poetry"
        "python"

        "python-black"
        "python-ipython"
        "python-jupyter-client"
        "python-jupyter-console"
        ;; Language server protocol for Python, integrates with emacs et al
        "python-lsp-server"
        "python-matplotlib"
        "python-pandas"
        "python-pdbpp"
        ;; Commented out due to conflict with `python-lsp-server'
        ;; "python-pytest"
        "python-seaborn")))

(define go-packages
  (map specification->package
       (list
        "go")))

(define utility-packages
  (map specification->package
       (list
        "gcc-toolchain"
        "git"
        ;; double-entry accounting library
        "ledger"
        ;; pass CLI interface
        "password-store"
        "sqlite"
        "unzip"
        ;; magic wormhole for one-off point-to-point transfers
        ;; https://github.com/magic-wormhole/magic-wormhole
        "magic-wormhole"
        )))

(define language-packages
  (map specification->package
       (list
        ;; python: see python-packages
        ;; guile: see guile-packages
        "node"
        ;; Perl6?
        ;; "rakudo"
        ;; OOPs
        "ruby"
        "rust"
        ;; NOTE: common-lisp should be broken out
        ;; Steel-bank common lisp
        "sbcl"
        "cl-asdf"
        "sbcl-coalton")))

(define term-fu-packages
  (map specification->package
       (list
        ;; an oxidized pager
        "bat"
        ;; the venerable HTTP[S] CLI client
        "curl"
        ;; ncurses disk usage explorer
        "ncdu"
        ;; an oxidized ls
        "exa"
        ;; an oxidized find
        "fd"
        "glibc-locales"
        ;; monitor processes
        "htop"
        ;; TODO: ispell vs aspell?
        ;; NOTE: I need both?!
        "aspell"
        "aspell-dict-en"
        "ispell"
        ;; json CLI slicing and dicing
        "jq"
        ;; system description utility (for fashion)
        "neofetch"
        ;; [n[[vi]m]] my term editor
        "neovim"
        ;; run Makefiles, now with guile integration
        "make"
        ;; an oxidized grep
        "ripgrep"
        ;; synchronize two directories
        "rsync"
        ;; an oxidized tldr
        "tealdeer"
        ;; top for hunting power gremlins
        "powertop"
        )))


(define font-packages
  (map specification->package
       (list
        ;; TODO: Do I actually use font-awesome for anything
        "font-awesome"
        ;; A nice vanilla font with a mellow monospace
        "font-dejavu")))

;; Packages / runtimes for managing applications
(define application-packages
  (map specification->package
       (list
        ;; escape hatch for installing pre-packaged bundles
        ;; https://www.flatpak.org/
        "flatpak")))


;; gnome-based dekstop applications
(define gnomish-packages
  (map specification->package
       (list
        ;; default icons for GTK application
        "adwaita-icon-theme"
        ;; GIMP Toolkits for e.g. file pickers
        ;; the gnome toolkit, old style (don't let the + fool you)
        "gtk+"
        ;; the gnome toolkit, new style
        "gtk"
        ;; gnome filesystem browser
        "nautilus"
        ;; torrenting
        "transmission-remote-gtk"
        )))

;; TODO break these into sub-groups
(define desktop-packages
  (map specification->package
       (list
        ;; flagship open-source 3D modeling
        "blender"
        ;; bluetooth controller and device management
        "bluez"
        ;; wayland compliant firefox web browser
        "firefox-wayland"
        ;; wayland compliant chromium without the google
        "ungoogled-chromium-wayland"
        ;; FreeCAD modeling software
        ;; Currently pyside-2 is broken
        "freecad"
        ;; a raster swiss army knife
        "gimp"
        ;; painting with pixel
        "krita"
        ;; gnome icon theme for nautilus et al
        "hicolor-icon-theme"
        ;; vector graphics editor
        ;; https://inkscape.org
        "inkscape"
        ;; windows emulation layer
        "wine"
        ;; player of videos
        "vlc"
        ;; signal secure instant messaging, now without SMS
        "signal-desktop"
        ;; Nyxt, the browser of the future; tomorrow
        "nyxt"

        ;; KDE packages
        ;; kde filesystem browser
        ;; "dolphin"
        ;; KDE connect for integrating with my phone
        ;; "kdeconnect"
        ;; kdenlive -- KDE non-linear video editor
        "kdenlive"

        ;; XDG stuff
        "xdg-desktop-portal"
        "xdg-desktop-portal-gtk"
        "xdg-desktop-portal-wlr"
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
  `(,@application-packages
    ,@audio-packages
    ,@base-packages
    ,@desktop-packages
    ,@email-packages
    ,@font-packages
    ,@gnomish-packages
    ,@go-packages
    ,@guile-packages
    ,@language-packages
    ,@ml-packages
    ,@pro-audio-packages
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
    ,@email-services
    ,@sway-services
    ,@tmux-services
    ,@zsh-services
    ; ,@git-services
    )))
