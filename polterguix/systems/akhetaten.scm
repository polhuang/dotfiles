(define-module (polterguix systems akhetaten)
   #:use-module (gnu)
   #:use-module (gnu home)
   #:use-module (gnu packages)
   #:use-module (gnu packages admin)
   #:use-module (gnu packages xdisorg)
   #:use-module (gnu packages autotools)
   #:use-module (gnu packages package-management)
   #:use-module (gnu packages emacs-xyz)
   #:use-module (gnu packages emacs)
   #:use-module (gnu packages librewolf)
   #:use-module (gnu packages video)
   #:use-module (gnu packages fonts)
   #:use-module (gnu packages base)
   #:use-module (gnu packages pkg-config)
   #:use-module (gnu packages ghostscript)
   #:use-module (gnu packages rust)
   #:use-module (gnu packages rust-apps)
   #:use-module (gnu packages gnupg)
   #:use-module (gnu packages vim)
   #:use-module (gnu packages mail)
   #:use-module (gnu packages shellutils)
   #:use-module (gnu packages password-utils)
   #:use-module (gnu packages wm)
   #:use-module (gnu packages web-browsers)
   #:use-module (gnu packages terminals)
   #:use-module (gnu packages admin)
   #:use-module (nongnu packages mozilla)
   #:use-module (gnu services)
   #:use-module (guix gexp)
   #:use-module (polterguix packages hyprland)
   #:use-module (polterguix packages desktop)
   #:use-module (polterguix packages security)
   #:use-module (polterguix packages cli)
   #:use-module (gnu home services)
   #:use-module (gnu home services shells)
   #:use-module (gnu home services ssh)
   #:use-module (polterguix systems core-system))

(define system
  (operating-system
   (inherit core-operating-system)
   (host-name "akhetaten")
   ))

(define home
  (home-environment
   (packages (list pinentry
                   pinentry-emacs
                   emacs-next-pgtk-xwidgets
                   emacs-guix
                   emacs-jinx
                   neofetch
                   firefox
                   asciiquarium
                   kitty
                   qutebrowser
                   font-fira-code
                   rofi-wayland
                   font-google-noto
                   font-google-noto-emoji
                   font-google-noto-sans-cjk 
                   mu
		   ;;automake
                   emacs-jinx
                   btop
		   ;;libtool
                   ;;librewolf
		   flatpak
		   font-fira-code
		   password-store ;;new
		   binutils ;;new
		   ;;pkg-config
                   waybar
		   font-ghostscript
		   font-dejavu
		   font-gnu-freefont
                   swaynotificationcenter
                   neovim
                   ;;obs
                   ;;rust
                   ;;rust-cargo
                   hyprpaper
                   hypridle
                   ripgrep
                   zsh-autosuggestions
                   zsh-completions
                   zsh-syntax-highlighting
                   sh-z
                   fzf
                   nix))
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
                          `(("hypr/hyprland.conf"  ,(local-file "../files/hypr/hyprland-akhetaten.conf"))))
          ))))

(if (equal? (getenv "GUIX_TARGET") "home")
    home
    system)
