#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Automed Compose
# @raycast.mode fullOutput

# Optional parameters:
# @raycast.icon ☤
# @raycast.argument1 { "type": "text", "placeholder": "Compose Arg", "optional": true }
# @raycast.packageName automed

# Documentation:
# @raycast.description automed compose launcher
# @raycast.author Thomas Moulia

AUTOMED_COMPOSE="$HOME/repos/automed/bin/automed-compose.sh"

if [ "$1" ]; then
    "$AUTOMED_COMPOSE" "$1"
else
    "$AUTOMED_COMPOSE"
fi
