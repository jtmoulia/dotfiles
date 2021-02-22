#!/usr/bin/env bash
if which greadlink > /dev/null 2>&1 ; then
    script_path="$(greadlink -f "${BASH_SOURCE[0]}")"
else
    script_path="$(readlink -f "${BASH_SOURCE[0]}")"
fi
dotfiles_path="$( cd "$( dirname "$( dirname "${script_path}" )" )" && pwd )"

playbook=

while [ "$1" ]; do
    case "$1" in
        -p|--playbook)
            playbook="$2"
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Unexpected argument: $1"
            exit 1
            ;;
    esac
done

if [ ! "$playbook" ]; then
    echo "Playbook argument required: -p | --playbook"
    exit 1
fi

password_files=
for file in $(find etc/ -name "*.password"); do
    password_files="$password_files --vault-password-file=$file"
done

ANSIBLE_COW_SELECTION=random ansible-playbook \
    --inventory "${dotfiles_path}/playbooks/.inventory" \
    --ask-become-pass \
    "${dotfiles_path}/playbooks/${playbook}.yml" \
    --extra "local_user=$USER" \
    $password_files \
    "$@"
