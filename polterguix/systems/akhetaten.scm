(define-module (polterguix systems akhetaten)
   #:use-module (gnu home)
   #:use-module (gnu packages)
   #:use-module (gnu packages admin)
   #:use-module (gnu packages xdisorg)
   #:use-module (gnu packages emacs-xyz)
   #:use-module (gnu packages emacs)
   #:use-module (gnu packages gnupg)
   #:use-module (gnu packages librewolf)
   #:use-module (gnu packages video)
   #:use-module (gnu packages fonts)
   #:use-module (gnu packages rust)
   #:use-module (gnu packages rust-apps)
   #:use-module (gnu packages vim)
   #:use-module (gnu packages mail)
   #:use-module (gnu packages password-utils)
   #:use-module (gnu packages wm)
   #:use-module (gnu packages web-browsers)
   #:use-module (gnu packages terminals)
   #:use-module (nongnu packages mozilla)
   #:use-module (gnu services)
   #:use-module (guix gexp)
   #:use-module (gnu home services shells)
   #:use-module (polterguix packages hyprland)
   #:use-module (polterguix packages desktop)
   #:use-module (polterguix packages security)
   #:use-module (polterguix packages cli))

(define home
  (home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (list pinentry
                 pinentry-emacs
                 emacs-next-pgtk-xwidgets
                 emacs-desktop-environment
                 emacs-guix
                 emacs-jinx
                 firefox
                 kitty
                 qutebrowser
                 rofi-wayland
                 font-google-noto
                 font-google-noto-emoji
                 font-google-noto-sans-cjk
                 mu
                 gnupg-new
                 emacs-jinx
                 btop
                 librewolf
                 waybar
                 alacritty
                 swaynotificationcenter
                 neovim
                 password-store
                 obs
                 rust
                 rust-cargo
                 hyprpaper
                 hypridle
                 starship-bin))
 ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (guix-defaults? #t)
                   (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
                              ("ls" . "ls -p --color=auto")))
                   (bashrc (list (local-file
                                  "/home/polhuang/polterguix/files/.bashrc" "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/polhuang/polterguix/files/.bash_profile"
                                        "bash_profile"))))
                  )
         (service home-zsh-service-type
                  (home-zsh-configuration
                   (zshrc (list (local-file
                                 "/home/polhuang/polterguix/files/.zshrc" "zshrc")))
                   (zprofile (list (local-file
                                    "/home/polhuang/polterguix/files/.zprofile"
                                    "zprofile")))))))))
