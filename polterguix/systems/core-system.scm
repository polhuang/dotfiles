(define-module (polterguix systems core-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux))
  
(use-service-modules guix admin sysctl pm nix avahi dbus cups desktop linux
		     mcron networking xorg ssh docker audio virtualization)

(use-package-modules audio video nfs certs shells ssh linux bash emacs gnome
		     networking wm fonts libusb cups freedesktop file-systems
		     version-control package-management vim)
  
(define-public base-operating-system
  (operating-system
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
   
   ;; placeholder file system
   (file-systems (cons* (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))
   
   ;; users
   (users (cons* (user-account
                 (name "pol")
                 (comment "Paul Huang")
                 (group "users")
                 (home-directory "/home/polhuang")
                 (shell (file-append zsh "/bin/zsh"))
                 (supplementary-groups '("wheel"
                                         "netdev"
                                         "kvm"
                                         "tty"
                                         "input"
                                          "docker"
                                          "realtime"
                                          "lp"
                                          "audio"
                                          "video")))
                (user-account
                  (name "admin")
                  (comment "Admin")
                  (group "users")
                  (home-directory "/home/admin")
                                   (supplementary-groups '("wheel"
                                         "netdev"
                                         "kvm"
                                         "tty"
                                         "input"
                                          "docker"
                                          "realtime"
                                          "lp"
                                          "audio"
                                          "video"))))
                %base-user-accounts))

   ;; real-time group
   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))
   
   ;; essential system packages
   (packages (cons* bluez
                    bluez-alsa
                    brightnessctl
                    emacs-no-x-toolkit
                    emacs-desktop-environment ; add to desktop module
                    hyprland                  ; add to desktop module  
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

   ;; essential services
   (services (append (list
                      (service guix-home-service type
                               `(("polhuang" ,home)))
                      (service elogind-service-type)
                      (service console-font-service-type
                               (map (lambda (tty)
                                      (cons tty (file-append
                                                 font-terminus
                                                 "/share/consolefonts/ter-132n")))
                                    '("tty1" "tty2" "tty3")))
                      (service greetd-service-type
                               (greetd-configuration
                                (greeter-supplementary-groups (list "video" "input"))
                                (terminals (list
                                            (greetd-terminal-configuration
                                             (terminal-vt "1")
                                             (terminal-switch #t)
                                             (greetd-terminal-configuration (terminal-vt"2"))
                                             (greetd-terminal-configuration (terminal0-vt "3"))))))

                      )
                     %base-services))))
