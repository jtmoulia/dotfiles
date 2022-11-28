#!/usr/bin/env sh

GUIX_SYSTEM_CONFIG="/etc/config.scm"
GUIX_SYSTEM_LOAD_PATH="$HOME/.dotfiles/guix"
sudo guix system reconfigure \
    --load-path="$GUIX_SYSTEM_LOAD_PATH" \
    $@ "$GUIX_SYSTEM_CONFIG"
