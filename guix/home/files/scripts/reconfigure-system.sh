#!/usr/bin/env sh

DOTFILES="${DOTFILES:-"$HOME/.dotfiles"}"
GUIX_SYSTEM_CONFIG="${GUIX_SYSTEM_CONFIG:-"$DOTFILES/guix/system0/config.scm"}"
GUIX_SYSTEM_LOAD_PATH="${GUIX_SYSTEM_LOAD_PATH:-"$DOTFILES/guix"}"
sudo guix system reconfigure \
    --load-path="$GUIX_SYSTEM_LOAD_PATH" \
    $@ "$GUIX_SYSTEM_CONFIG"
