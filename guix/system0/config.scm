;; This is an operating system configuration generated
;; by the graphical installer.

(define-module (system0 config)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages shells)
  #:use-module (gnu system setuid)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (system0 autofs)
  #:use-module (system0 docker)
  #:use-module (system0 pd)
  #:use-module (system0 sync))

(use-service-modules desktop networking ssh xorg)
(use-package-modules security-token)

(define sway-packages
  (map
   specification->package
   (list "sway" "swaylock")))

(define %my-desktop-services
  (modify-services
   %desktop-services
   (delete login-service-type)
   (delete mingetty-service-type)
   (delete gdm-service-type)
   (elogind-service-type config =>
                         (elogind-configuration
                          (inherit config)
                          (handle-lid-switch-external-power 'suspend)
                          (idle-action 'suspend)
                          (idle-action-seconds (* 30 10))))))

(define greetd-service
  (service greetd-service-type
           (greetd-configuration
            (terminals
             (list
              (greetd-terminal-configuration
               (terminal-vt "1")
               (terminal-switch #t)
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))

              ;; (greetd-terminal-configuration
              ;;  (terminal-vt "2")
              ;;  (default-session-command
              ;;    (greetd-wlgreet-session)))
              (greetd-terminal-configuration
               (terminal-vt "2")
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))

              (greetd-terminal-configuration
               (terminal-vt "3")
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))

              (greetd-terminal-configuration
               (terminal-vt "4")
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))

              (greetd-terminal-configuration
               (terminal-vt "5")
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh")))))

              (greetd-terminal-configuration
               (terminal-vt "6")
               (default-session-command
                 (greetd-agreety-session (command (file-append zsh "/bin/zsh"))))))))))

(operating-system
 (kernel linux)
 (kernel-arguments (append (list "mem_sleep_default=deep" "nvme.noacpi=1")
                           %default-kernel-arguments))
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "cog")
 (users (cons* (user-account
                (name "jtmoulia")
                (comment "Thomas Moulia")
                (group "users")
                (shell (file-append zsh "/bin/zsh"))
                (home-directory "/home/jtmoulia")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "docker" "plugdev")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "nss-certs"))
   docker-packages
   pd-packages
   sway-packages
   sync-packages
   %base-packages))
 (services
  (append
   (list
    greetd-service
    (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
    (service openssh-service-type)
    (service automount-service-type)
    (bluetooth-service)
    (set-xorg-configuration
     (xorg-configuration (keyboard-layout keyboard-layout))))
   docker-services
   pd-services
   sync-services
   %my-desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid "1c9d7a3f-4871-4afc-b154-421551abe5a2"))
         (target "cryptroot")
         (type luks-device-mapping))))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "11D8-CB38" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device "/dev/mapper/cryptroot")
          (type "ext4")
          (dependencies mapped-devices))
         %base-file-systems))
 (setuid-programs
  (append (list
           (file-like->setuid-program
            (file-append (specification->package "swaylock") "/bin/swaylock")))
          %setuid-programs))
 )
