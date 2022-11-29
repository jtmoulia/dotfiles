#!/usr/bin/env sh

DOTFILES="${DOTFILES:-"$HOME/.dotfiles"}"
GUIX_HOME_LOAD_PATH="${GUIX_HOME_LOAD_PATH:-"$DOTFILES/guix"}"
GUIX_HOME_CONFIG="${GUIX_HOME_CONFIG:-"$GUIX_HOME_LOAD_PATH/home/environment.scm"}"

guix home reconfigure \
    --load-path="$GUIX_HOME_LOAD_PATH" \
    $@ "$GUIX_HOME_CONFIG"
