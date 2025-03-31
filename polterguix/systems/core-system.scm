(define-module (polterguix systems core-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  
  
  #:use-module (gnu services avahi)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  ;; #:use-module (gnu services guix)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sound) 
  #:use-module (gnu services xorg)
  
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages emacs)  
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages suckless) 
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xdisorg)
  
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  
)


(define-public core-operating-system
  (operating-system
   (host-name "akhetaten")
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)
   (timezone "America/Chicago")
   (keyboard-layout (keyboard-layout "us"))			
   (locale "en_US.utf8")
   

   ;; uefi grub with efi
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))

   (file-systems (cons* (file-system
                         (mount-point "/tmp")
                         (device "none")
                         (type "tmpfs")
                         (check? #f))
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
                         (shell (file-append zsh "/bin/zsh"))
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
                    exfat-utils
                    font-awesome
                    fuse-exfat
                    git
                    libva-utils
                    ntfs-3g
                    solaar
                    stow
                    vim
                    zsh
                    %base-packages))
   (services
    %desktop-services)))

