set -U EDITOR nvim
set -U GDK_SCALE 1

if [ ! (contains $PATH ~/bin) ]
    set -U fish_user_paths ~/bin $fish_user_paths
end
