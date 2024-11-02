(define-module (polterguix systems core-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages video)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages shells)
  #:use-module (nongnu packages linux)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (polterguix packages hyprland)
  #:use-module (rosenthal packages wm)

  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services guix)
  )

(define-public core-operating-system
  (operating-system
   (host-name "akhetaten")
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)
   (timezone "America/Chicago")
   (locale "en_US.utf8")
   (keyboard-layout (keyboard-layout "us"))

   ;; uefi grub with efi
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))

   (mapped-devices (list (mapped-device
                          (source (uuid
                                   "29df3858-a0bb-46c1-8d52-64a11734a789"))
                          (target "crypt-akhetaten")
                          (type luks-device-mapping))))

   ;; placeholder file system
   (file-systems (cons* (file-system
                         (mount-point "/tmp")
                         (device "none")
                         (type "tmpfs")
                         (check? #f))
			(file-system
                         (mount-point "/")
                         (device "/dev/mapper/crypt-akhetaten")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "6CCB-3E30"
                                       'fat32))
                         (type "vfat"))
                       %base-file-systems))

   (swap-devices (list (swap-space
			(target "/swap/swapfile"))))
   

   ;; users
   (users (append (list (user-account
			 (name "pol")
			 (comment "Paul Huang")
			 (group "users")
			 (home-directory "/home/pol")
			 (shell (file-append zsh "/bin/zsh"))
			 (supplementary-groups '("wheel"
						 "netdev"
						 "kvm"
						 "tty"
						 "input"                
						 "realtime"
						 "lp"
						 "audio"
						 "video")))
			(user-account
			 (name "admin")
			 (comment "Admin")
			 (group "users")
			 (home-directory "/home/admin")
			 (supplementary-groups '("wheel" "netdev" "audio" "video"))))
		  %base-user-accounts))

   ;; real-time group
   (groups (cons (user-group (name "realtime"))
                 %base-groups))

   ;; essential system packages
   (packages (cons* bluez
                    bluez-alsa
                    brightnessctl
                    emacs-no-x-toolkit
                    emacs-desktop-environment
                    hyprland
                    exfat-utils
                    fuse-exfat
                    git
                    gvfs
                    libva-utils
                    ntfs-3g
                    stow
                    vim
                    zsh
                    %base-packages))
   (services
 (append (modify-services %desktop-services
              (guix-service-type config => (guix-configuration
                                               (inherit config)
                                               (substitute-urls
                                                (append (list "https://substitutes.nonguix.org")
                                                        %default-substitute-urls))
                                               (authorized-keys
                                                (append (list (local-file "../files/signing-key.pub"))
                                                        %default-authorized-guix-keys)))))
         (list
          (service gnome-desktop-service-type)
          (service openssh-service-type)
          (service tor-service-type)
          (service cups-service-type)
          (set-xorg-configuration
           (xorg-configuration (keyboard-layout keyboard-layout)))))

   )))

core-operating-system
