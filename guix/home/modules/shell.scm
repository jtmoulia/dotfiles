(define-module (home modules shell)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells))

(define-public zsh-packages
  (map specification->package
       (list
        "zsh"
        "zsh-autosuggestions"
        "fzf"
        "zoxide")))

(define-public zsh-services
  (list
   (service home-zsh-service-type
            (home-zsh-configuration
             (xdg-flavor? #t)
             (environment-variables
              '(("EDITOR" . "nvim")
                ("PAGER" . "less")
                ("TERM" . "screen-256color")
                ("LOCAL_PATH" . "$HOME/.local/bin")
                ("GUIX_PROFILE" . "$HOME/.guix-profile")
                ("GUIX_HOME" . "$HOME/.guix-home")
                ("GUIX_LOCPATH" . "$GUIX_HOME/profile/lib/locale")
                ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")
                ("HISTFILE" . "$XDG_DATA_HOME/histfile")
                ("SAVEHIST" . "10000")
                ("HISTSIZE" . "10000")
                ("ZINIT_HOME" . "$XDG_DATA_HOME/zinit/zinit.git")
                ("FZF_TMUX_OPTS" . "-d 40%")))
             (zprofile
              (list (local-file "../files/zprofile.zsh")))
             (zlogin
              (list (local-file "../files/zprofile.zsh")))
             (zshrc
              (list
               (local-file "../files/zshrc.zsh")
               (local-file "../files/zprofile.zsh")))))))
