(define-module (home modules wm)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

;; NOTE: foot is configured along with sway for simplicity's sake.

(define-public sway-packages
  (map specification->package
       (list
        "alacritty"
        ;; CLI utility to control screen brightness
        "brightnessctl"
        ;; a simple shell (because alacritty isn't working?!)
        "foot"
        "font-noto-emoji"
        ;; grimshot for taking screenshots
        "grimshot"
        ;; desktop notification plumbing
        "libnotify"
        ;; wayland desktop notification presentation
        "mako"
        ;; launcher, rofiway style
        "rofi-wayland"
        "sway"
        ;; NOTE: swayr isn't currently used, but I want it
        "swayr"
        ;; waybar for a wayland system bar; configured with sway
        ;; NOTE;
        "waybar"
        ;; wayland friendly clipboard (alias `xc` from shell)
        "wl-clipboard")))

(define-public sway-services
  (list
   (simple-service 'my-sway-env-vars-service
                   home-environment-variables-service-type
                   '(("XKB_DEFAULT_OPTIONS" . "ctrl:nocaps,")))
   (simple-service 'my-sway-config-files-service
                   home-xdg-configuration-files-service-type
                   `(("sway/config" ,(local-file "../files/sway/config"))
                     ("sway/status.sh" ,(local-file "../files/sway/status.sh"))
                     ("sway/tree-switcher.sh" ,(local-file "../files/sway/tree-switcher.sh"))
                     ("sway/focus-or-start.sh" ,(local-file "../files/sway/focus-or-start.sh"))
                     ("waybar/config" ,(local-file "../files/waybar/config"))
                     ("waybar/style.css" ,(local-file "../files/waybar/style.css"))
                     ("foot/foot.ini" ,(local-file "../files/foot/foot.ini"))))))
