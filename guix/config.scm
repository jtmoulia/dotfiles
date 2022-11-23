;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu packages shells)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules desktop networking ssh xorg)

(define %my-desktop-services
  (modify-services %desktop-services
                   (elogind-service-type config =>
                                         (elogind-configuration (inherit config)
                                                                (handle-lid-switch-external-power 'suspend)))))

(operating-system
 (kernel linux)
 (kernel-arguments (append (list "mem_sleep_default-deep" "nvme.noacpi=1")
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
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service gnome-desktop-service-type)
         (service openssh-service-type)
         (bluetooth-service)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets "/boot/efi")
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
         %base-file-systems)))
