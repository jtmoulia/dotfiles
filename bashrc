PATH=$PATH:$HOME/bin:/usr/local/mysql/bin

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n\$ '

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel

# Set my editor and git editor
export EDITOR="/usr/bin/vim"
export GIT_EDITOR='/usr/bin/vim'

set -o vi

# Aliases
alias g="git"
alias v="vim"
# ..ssh
alias gumby="ssh ubuntu@gumby.runway20.com"

# Function to set process name
tt () { echo -ne "\033]0;$@\\007"; }
