# Appends every command to the history file once it is executed
setopt inc_append_history
# Reloads the history whenever you use it
setopt share_history
# Ignore duplicates and spaces to keep the history clean
setopt hist_ignore_dups
setopt hist_ignore_space
setopt prompt_subst

# zinit for managing zsh modules
# TODO First time run, only if dir exists?
# git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

# Aliases
if $(which xdg-open &>/dev/null); then
  alias o='xdg-open'
fi

# Alias mkpass as an xkcdpass heper if installed
if $(which xkcdpass &>/dev/null); then
  alias mkpass='xkcdpass --max 16 --case random --random-delimiters --numwords 3'
fi

# Use nvim if it's installed
if $(which nvim &>/dev/null); then
  EDITOR=nvim
  alias vi=nvim
else
  # Fallback to vi, typically aliased to vim
  if $(which vi &>/dev/null); then
     EDITOR=vi
  fi

fi

# Use exa over ls if it's installed
if $(which exa &>/dev/null); then
  alias l='exa --oneline --icons'
  alias ls='exa --icons'
  alias ll='exa --long --all --icons'
fi

# Define helper wrapping xclip or wl-copy
# Usage: `echo "copy this" | xc``

if $(which wl-copy &>/dev/null); then
   alias xc='wl-copy'
elif $(which xclip &>/dev/null); then
   alias xc='xclip -selection c'
fi

if $(which keychain &>/dev/null); then
  eval $(keychain --eval --agents ssh,gpg id_rsa)
fi

# General fzf config and install
# zsh-fzf-history-search
# Usage: C-r to pull up history from shell
zinit ice lucid wait'0'
zinit light joshskidmore/zsh-fzf-history-search

if $(which fzf $>/dev/null); then
  alias f='fzf --preview "bat --color \"always\" {}"'
fi

# zoxide zsh setup for quick nav
# Usage: z $TARGET_PATH
if $(which zoxide &>/dev/null); then
  eval "$(zoxide init zsh)"
fi

# Pretty prompt
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats '%b '
PROMPT='%F{green}%*%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

# Get flatpak XDG paths added
if [ -f "$GUIX_PROFILE/etc/profile.d/flatpak.sh" ]; then
    source "$GUIX_PROFILE/etc/profile.d/flatpak.sh"
fi
