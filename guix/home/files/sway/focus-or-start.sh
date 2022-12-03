#!/usr/bin/env bash

# Sway bash script to focus an application if its process exists, otherwise start the
# application in the current workspace.
#
# Example usage: focus-or-start.sh firefox

application="$1"

if [[ -n $(pidof -x "$application") ]]; then
    echo swaymsg "[app_id=.$application] focus";
    swaymsg "[app_id=$application] focus";
else swaymsg "exec $application;";
fi
