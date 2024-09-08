;; no longer a manifest

(packages (cons* bluez
                 bluez-alsa
                 brightnessctl
                 curl
                 git
                 emacs-next-pgtk-xwidgets
                 emacs-exwm
                 gcc ;;
                 autoconf
                 automake
                 libtool
                 awesome
                 hyprland
                 font-ghostscript
                 font-dejavu
                 font-fira-code
                 font-gnu-freefont
                 exfat-utils
                 fuse-exfat
                 gvfs
                 intel-media-driver/nonfree
                 libva-utils
                 ntfs-3g
                 flatpak
                 stow
                 ;; password-store - security
                 binutils
                 pkg-config
                 vim
                 zsh
                 %base-packages))
