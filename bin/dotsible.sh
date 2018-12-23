#!/usr/bin/env bash
if which greadlink > /dev/null 2>&1 ; then
    script_path="$(greadlink -f "${BASH_SOURCE[0]}")"
else
    script_path="$(readlink -f "${BASH_SOURCE[0]}")"
fi
dotfiles_path="$( cd "$( dirname "$( dirname "${script_path}" )" )" && pwd )"

ansible-playbook \
    --inventory "${dotfiles_path}/playbooks/.inventory" \
    --ask-become-pass \
    "${dotfiles_path}/playbooks/main.yml" \
    --extra "local_user=$USER" \
    "$@"
