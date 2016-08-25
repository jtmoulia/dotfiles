# ~/.bash_profile
# author: jtmoulia

# Load `~/.bashrc` on login
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
# Initialization for FDK command line tools.Thu Apr 23 15:32:12 2015
FDK_EXE="/Users/jtmoulia/bin/FDK/Tools/osx"
PATH=${PATH}:"/Users/jtmoulia/bin/FDK/Tools/osx"
export PATH
export FDK_EXE
