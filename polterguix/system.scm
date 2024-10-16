;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules
 (gnu)
 (gnu packages)
 (gnu packages linux)
 (gnu packages audio)
 (gnu packages emacs)
 (gnu packages curl)
 (gnu packages emacs)
 (gnu packages pkg-config)
 (gnu packages emacs-xyz)
 (gnu packages file-systems)
 (gnu packages fonts)
 (gnu packages gcc)
 (gnu packages autotools)
 (gnu packages gnome)
 (gnu packages password-utils)
 (gnu packages package-management)
 (gnu packages commencement)
 (gnu packages node)
 (gnu packages ghostscript)
 (gnu packages shells)
 (gnu packages version-control)
 (gnu packages vim)
 (gnu packages wm)
 (guix packages)
 (rosenthal packages wm) 
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (nongnu packages game-client))

(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "akhetaten")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "polhuang")
                  (comment "Paul Huang")
                  (group "users")
                  (shell "/run/current-system/profile/bin/zsh")
                  (home-directory "/home/polhuang")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                (user-account
                  (name "admin")
                  (comment "Admin")
                  (group "users")
                  (home-directory "/home/admin")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (cons* bluez
                   bluez-alsa
                   brightnessctl
                   curl
                   git
		   emacs-no-x-toolkit
                   zsh
                   emacs-desktop-environment
                   emacs-exwm
                   hyprland
                   exfat-utils
                   fuse-exfat
                   gvfs
                   ntfs-3g
                   stow
		   vim
                   %base-packages))
            
  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 (service tor-service-type)
                 (service cups-service-type)
                 (service bluetooth-service-type
                          (bluetooth-configuration
                           (auto-enable? #t)))
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "98fe1cbd-b9db-4740-bb07-be1544034649"))
                          (target "crypt-akhenaten")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/crypt-akhenaten")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "6CCB-3E30"
                                       'fat32))
                         (type "vfat")) %base-file-systems)))
