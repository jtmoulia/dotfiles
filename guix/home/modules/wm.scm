(define-module (home modules wm)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

;; NOTE: foot is configured along with sway for simplicity's sake.

(define-public sway-packages
  (map specification->package
       (list
        ;; TODO: alacritty hasn't been working, drop or fix
        "alacritty"
        ;; CLI utility to control screen brightness
        "brightnessctl"
        ;; a simple shell (because alacritty isn't working?!)
        "foot"
        ;; emoji friendly noto font: =Noto Emoji [Weight]=
        "font-noto-emoji"
        ;; grimshot for taking screenshots of windows / full screen
        "grimshot"
        ;; desktop notification plumbing
        "libnotify"
        ;; wayland desktop notification presentation
        ;; NOTE: mako build currently broken
        "mako"
        ;; sway window manager (@ddevault)
        ;; https://github.com/swaywm/sway
        "sway"
        ;; idle management daemon for Wayland compositors
        "swayidle"
        ;; Swaylock for sway screen locking
        ;; NOTE: Installed by the system profile for setuid privs
        ;; https://github.com/swaywm/swaylock
        ;; "swaylock"
        ;; sway window LRU, gathered by separate running server
        ;; https://sr.ht/~tsdh/swayr/
        "swayr"
        ;; waybar for a wayland system bar; configured with sway
        "waybar"
        ;; an application for reading inputs within wayland
        "wev"
        ;; screen recorder for wayland
        "wf-recorder"
        ;; wayland friendly clipboard (alias `xc` from shell)
        "wl-clipboard"
        ;; launcher, wofi style
        "wofi")))

(define-public sway-services
  (list
   (simple-service 'my-sway-env-vars-service
                   home-environment-variables-service-type
                   '(("XKB_DEFAULT_OPTIONS" . "ctrl:nocaps,")))
   (simple-service 'my-sway-config-files-service
                   home-xdg-configuration-files-service-type
                   `(("sway/config" ,(local-file "../files/sway/config"))
                     ("sway/status.sh" ,(local-file "../files/sway/status.sh"))
                     ("sway/bluetooth-toggle.sh" ,(local-file "../files/sway/bluetooth-toggle.sh"))
                     ("swayr/config.toml" ,(local-file "../files/swayr/config.toml"))
                     ("waybar/config" ,(local-file "../files/waybar/config"))
                     ("waybar/style.css" ,(local-file "../files/waybar/style.css"))
                     ("wofi/style.css" ,(local-file "../files/wofi/style.css"))
                     ("foot/foot.ini" ,(local-file "../files/foot/foot.ini"))))))
