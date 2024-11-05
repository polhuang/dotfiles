;; Unlike the others, this machine does not run on guix system. It only uses the guix package manager.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages shellutils)
             (gnu packages terminals)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells)
             (polterguix packages cli))

(home-environment
  (packages (list
             emacs-next-pgtk-xwidgets
             emacs-guix
             kitty ;; locale errors (tab-completion problem with unicode characters) when not kitty isn't installed with guix package manager
             zsh-completions
             zsh-autosuggestions
             zsh-syntax-highlighting
             ;; for a future new install, add starship-bin from polterguix/packages/cli
             sh-z))

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
                                 "../files/.zshrc" "zshrc")
))
                   (zprofile (list (local-file
                                    "../files/.zprofile"
                                    "zprofile")))))

         (simple-service 'dotfiles
                          home-xdg-configuration-files-service-type
                          `(("hypr/hyprland.conf"  ,(local-file "../files/hypr/hyprland-nineveh.conf"))
                            ("hypr/hyprland-base.conf"  ,(local-file "../files/hypr/hyprland-base.conf")))))))
