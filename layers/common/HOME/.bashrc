# ~/.bashrc
# author: jtmoulia

## Define Functions

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
export EDITOR="/usr/bin/vim"
export GIT_EDITOR="/usr/bin/vim"

# Set the bash prompt
PS1='\[\033[0;32m\]\u\[\033[0;34m\]::\[\033[0;31m\]\h \[\033[0;34m\]{ \[\033[0;36m\]\w \[\033[0;34m\]} \[\033[0;32m\]$ \[\033[00m\]'

# go Config
if hash go 2>/dev/null; then
    export GOPATH="$HOME/go"
    PATH="$PATH:$GOPATH/bin"
fi

# RVM Config
# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

PATH="/Users/jtmoulia/perl5/bin${PATH+:}${PATH}"; export PATH;
PERL5LIB="/Users/jtmoulia/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/jtmoulia/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/jtmoulia/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/jtmoulia/perl5"; export PERL_MM_OPT;
