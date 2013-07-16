# A good path...
PATH=~/bin:/usr/local/bin:$PATH

# Set my editor and git editor
export EDITOR="/usr/bin/vim"
export GIT_EDITOR='/usr/bin/vim'

# Function to set process name
tt () { echo -ne "\033]0;$@\\007"; }

export TERM=screen-256color
