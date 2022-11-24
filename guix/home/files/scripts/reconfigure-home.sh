#!/usr/bin/env sh

GUIX_DIR="$HOME/.dotfiles/guix"
ENV_FILE="$GUIX_DIR/home/environment.scm"

# need to use this to prepend to the load path.
GUILE_LOAD_PATH="$GUILE_LOAD_PATH:$GUIX_DIR" \
    guix home reconfigure $@ $ENV_FILE
