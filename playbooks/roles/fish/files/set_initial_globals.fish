set -U EDITOR nvim

if [ ! (contains $PATH ~/bin) ]
    set -U fish_user_paths ~/bin $fish_user_paths
end