(define-module (polterguix systems core-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu packages linux)
  
  #:use-module (gnu system privilege)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages video)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages libusb)
  #:use-module (polterguix packages hyprland)
  #:use-module (rosenthal packages wm)
  
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services avahi)
  #:use-module (gnu services dbus)
  #:use-module (gnu services pm)
  #:use-module (gnu services linux)
  #:use-module (gnu services docker)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services ssh)
  #:use-module (gnu services cups)
  #:use-module (gnu services nix)
  #:use-module (gnu services mcron))

(define-public core-operating-system
  (operating-system
   (host-name "core")
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
   (users (cons (user-account
                  (name "pol")
                  (comment "Paul Huang")
                  (group "users")
                  (home-directory "/home/pol")
                  ;; (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups '("wheel"
                                          "netdev"
                                          "kvm"
                                          "tty"
                                          "input"                
                                          "realtime"
                                          "lp"
                                          "audio"
                                          "video")))
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

   ;; essential services
   (services (append (list (service elogind-service-type)
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
                                                  (terminal-switch #t))
                                                 (greetd-terminal-configuration (terminal-vt "2"))
                                                 (greetd-terminal-configuration (terminal-vt "3"))))))
                           ;; put hyprlock here eventually

                           (simple-service 'add-nonguix-substitutes
                                           guix-service-type
                                           (guix-extension
                                            (substitute-urls
                                             (append (list "https://substitutes.nonguix.org")
                                                     %default-substitute-urls))
                                            (authorized-keys
                                             (append (list (plain-file "nonguix.pub"
                                                                       "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                                     %default-authorized-guix-keys))))
                           polkit-wheel-service

                           (service network-manager-service-type
                                    (network-manager-configuration
                                     (vpn-plugins
                                      (list network-manager-openvpn))))
                           (service wpa-supplicant-service-type)
                           (service modem-manager-service-type)
                           (service bluetooth-service-type
                                    (bluetooth-configuration
                                     (auto-enable? #t)))
                           (service usb-modeswitch-service-type)

                           (service avahi-service-type)
                           (service udisks-service-type)
                           (service upower-service-type)
                           (service cups-pk-helper-service-type)
                           (service geoclue-service-type)
                           (service polkit-service-type)
                           (service dbus-root-service-type)
                           fontconfig-file-system-service

                           (service thermald-service-type)
                           (service tlp-service-type
                                    (tlp-configuration
                                     (cpu-boost-on-ac? #t)
                                     (wifi-pwr-on-bat? #t)))

               ;; realtime mode
               (service pam-limits-service-type
                        (list
                         (pam-limits-entry "@realtime" 'both 'rtprio 99)
                         (pam-limits-entry "@realtime" 'both 'nice -19)
                         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

               ;; v4l2loopback module for virtual cameras
               ;; see: https://stackoverflow.com/a/66072635
               ;;           https://github.com/umlaeute/v4l2loopback
               (service kernel-module-loader-service-type '("v4l2loopback"))
               (simple-service 'v4l2loopback-config etc-service-type
                               (list `("modprobe.d/v4l2loopback.conf"
                                       ,(plain-file "v4l2loopback.conf"
                                                    "options v4l2loopback devices=1 video_nr=2 exclusive_caps=1 card_label=\"OBS Virtual Camera\""))))


               ;; docker
               (service containerd-service-type)
               (service docker-service-type)
               (service libvirt-service-type
                        (libvirt-configuration
                         (unix-sock-group "libvirt")
                         (tls-port "16555")))

               ;; ssh
               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)
                         (port-number 2222)))

               ;; printing/scanning
               (service sane-service-type)
               (service cups-service-type
                        (cups-configuration
                         (web-interface? #t)
                         (extensions
                          (list cups-filters))))

               ;; x11 socket directory
               (service x11-socket-directory-service-type)

               ;; sync system clock
               (service ntp-service-type)
               
               ;; udev rules 
               (udev-rules-service 'pipewire-add-udev-rules pipewire)
               (udev-rules-service 'brightnessctl-udev-rules brightnessctl)
               (simple-service 'mtp udev-service-type (list libmtp)) ; udev rules for non-root user access for mpt devices

               ;; enable nix build service
               (service nix-service-type)

               ;; system cron jobs
               (simple-service 'system-cron-jobs
                               mcron-service-type
                               (list
                                ;; run `guix gc' 5 minutes after midnight every day.
                                ;; clean up generations older than 2 months and free
                                ;; at least 10g of space.
                                #~(job "5 0 * * *" "guix gc -d 2m -F 10G"))))
                     (modify-services %base-services
                                      (delete login-service-type)
                                      (delete mingetty-service-type)
                                      (delete console-font-service-type))))))

core-operating-system
