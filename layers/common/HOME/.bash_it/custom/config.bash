set -o vi

# Func to append elements to the end of PATH
append_to_path () {
    for path_elt in $@; do
        PATH="${PATH}:${path_elt}"
    done
}

# Func to prepend elements to the beginning of PATH
prepend_to_path() {
    # NB: This doesn't reverse the args: the last arg
    # will become the first element in the path. Uglish
    for path_elt in $@; do
        PATH="${path_elt}:${PATH}"
    done
}

# Func to set process name
tt () { echo -ne "\033]0;$@\\007"; }


## Set Variables

# First, use the home bin, then /usr/local/bin, then the rest
append_to_path "/usr/local/bin" "${HOME}/bin"

# Set my editor and git editor
export EDITOR="$(which nvim)"
export GIT_EDITOR="$(which nvim)"

# virtualenv[wrapper]
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
source /usr/bin/virtualenvwrapper.sh

prepend_to_path "/Users/jtmoulia/local/bin" "${HOME}/.config/yarn/global/node_modules/.bin"

if [ $(command -v ruby) ]; then
    prepend_to_path "$(ruby -e 'print Gem.user_dir')/bin"
fi

export CESIUM="$USER@ip-172-16-10-20.ec2.internal"
