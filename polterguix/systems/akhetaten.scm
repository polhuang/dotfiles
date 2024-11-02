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
   #:use-module (gnu home services shells)
   #:use-module (polterguix packages hyprland)
   #:use-module (polterguix packages desktop)
   #:use-module (polterguix packages security)
   #:use-module (polterguix packages cli)
   #:use-module (polterguix systems core-system))

;; (use-modules (gnu home)
;;              (gnu packages)
;;              (gnu packages admin)
;;              (gnu packages xdisorg)
;;              (gnu packages emacs-xyz)
;;              (gnu packages emacs)
;;              (gnu packages gnupg)
;;              (gnu packages librewolf)
;;              (gnu packages video)
;;              (gnu packages fonts)
;;              (gnu packages rust)
;;              (gnu packages rust-apps)
;;              (gnu packages gnupg)
;;              (gnu packages vim)
;;              (gnu packages mail)
;;              (gnu packages password-utils)
;;              (gnu packages wm)
;;              (gnu packages autotools)
;;              (gnu packages web-browsers)
;;              (gnu packages terminals)
;;              (gnu packages package-management)
;;              (nongnu packages mozilla)
;;              (gnu services)
;;              (guix gexp)
;;              (gnu home services shells)
;;              (polterguix packages hyprland)
;;              (polterguix packages desktop)
;;              (polterguix packages security)
;;              (polterguix packages cli))

(define system
  (operating-system
   (inherit core-operating-system)
   (host-name "akhetaten")
   ))

(define home
  (home-environment
 ;; Below is the list of packages that will show up in your ;; Home profile, under ~/.guix-home/profile.
   (packages (list pinentry ;;new
                   pinentry-emacs ;new
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
                   ))
   ;; Below is the list of Home services.  To search for available
   ;; services, run 'guix home search KEYWORD' in a terminal.
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
                                    "zprofile")))))))))

(if (equal? (getenv "GUIX_TARGET") "home")
    home
    system)
