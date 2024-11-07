(define-module (polterguix systems akhetaten)
  #:use-module (gnu)
  #:use-module (gnu home)
  ;;  #:use-module (gnu packages)
  ;;  #:use-module (gnu packages autotools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  ;;  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages fonts)
  ;;  #:use-module (gnu packages base)
  ;;  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xdisorg)
  #:use-module (nongnu packages mozilla)
  #:use-module (polterguix packages cli)
  #:use-module (polterguix packages desktop)
  #:use-module (polterguix packages hyprland)
  ;;  #:use-module (gnu services)
  ;;  #:use-module (gnu services networking)
   
  ;;  #:use-module (gnu services ssh)
  
  ;; #:use-module (gnu services xorg)

   
  ;;  #:use-module (guix gexp)
  ;;  #:use-module (polterguix packages desktop)
  ;;  #:use-module (polterguix packages security)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  
  #:use-module (polterguix systems core-system))

(define system
  (operating-system
   (inherit core-operating-system)
   (host-name "akhetaten")

   (mapped-devices (list (mapped-device
                          (source (uuid
                                   "29df3858-a0bb-46c1-8d52-64a11734a789"))
                          (target "crypt-akhetaten")
                          (type luks-device-mapping)))) 
   ;; placeholder file system
   (file-systems (cons* (file-system
                         (mount-point "/")
                         (device "/dev/mapper/crypt-akhetaten")
                         (type "btrfs")
                         (dependencies mapped-devices))
                        (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "6CCB-3E30"
                                       'fat32))
                         (type "vfat"))
                        %base-file-systems))))


(define home
  (home-environment
   (packages (list asciiquarium
                   btop
                   emacs-next-pgtk-xwidgets
                   emacs-guix
                   emacs-jinx
                   firefox
                   font-fira-code
                   font-google-noto
                   font-google-noto-emoji
                   font-google-noto-sans-cjk 
		   flatpak
		   font-dejavu
		   font-fira-code
		   font-ghostscript
		   font-gnu-freefont
                   fzf
                   hunspell
                   hypridle
                   hyprpaper
                   kitty
		   librewolf
                   mu
                   neofetch
                   neovim
                   nix
                   obs
                   qutebrowser
		   password-store
                   pinentry
                   pinentry-emacs
                   ripgrep
                   
                   rofi-wayland
                   sh-z
                   
                   starship-bin
                   swaynotificationcenter
                   waybar
                   zsh-autosuggestions
                   zsh-completions
                   zsh-syntax-highlighting
		   ))
   (services
    (list (service home-zsh-service-type
                   (home-zsh-configuration
                    (zshrc (list 
                            (mixed-text-file "zsh-autosuggestions"
                                             "source $HOME/.guix-home/profile/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh")
                            (mixed-text-file "zsh-syntax-highlighting"
                                             "source $HOME/.guix-home/profile/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh")
                            (mixed-text-file "zsh-completions"
                                             "fpath=($HOME/.guix-home/share/zsh/site-functions $fpath)")
                            (local-file
                             "/home/pol/polterguix/files/.zshrc" "zshrc")
                            ))
                    (zprofile (list (local-file
                                     "/home/pol/polterguix/files/.zprofile"
                                     "zprofile")))))
          (service home-openssh-service-type
                   (home-openssh-configuration
                    (hosts
                     (list (openssh-host (name "babylon")
                                         (host-name "192.168.0.111")
                                         (user "pol")
                                         (identity-file "/home/pol/.ssh/babylon")
                                         (port 39902))))))
          (simple-service 'dotfiles
                          home-xdg-configuration-files-service-type
                          `(("hypr/hyprland.conf"  ,(local-file "../files/hypr/hyprland-akhetaten.conf"))
                            ("hypr/hyprland-base.conf"  ,(local-file "../files/hypr/hyprland-base.conf"))))
          ))))

(if (equal? (getenv "GUIX_TARGET") "home")
    home
    system)
