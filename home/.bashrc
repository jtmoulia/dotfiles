# A good path...
PATH=~/bin:/usr/local/bin:$PATH

export TERM=screen-256color

# Set my editor and git editor
export EDITOR="/usr/bin/vim"
export GIT_EDITOR='/usr/bin/vim'

# Function to set process name
tt () { echo -ne "\033]0;$@\\007"; }


PS1='\[\033[0;32m\]\u\[\033[0;34m\]::\[\033[0;31m\]\h \[\033[0;34m\]{ \[\033[0;34m\]\w \[\033[0;34m\]} \[\033[0;32m\]-> \[\033[00m\]'
