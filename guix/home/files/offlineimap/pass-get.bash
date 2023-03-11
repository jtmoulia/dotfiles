#!/usr/bin/env bash
USAGE="pass-get [ pass-name ] [ line-key ]"
passname="$1"
key="$2"

if [[ ! "$passname" || ! "$key" ]]; then
    echo "Error: missing required argument!"
    echo Usage: "$USAGE"
    exit 1
fi

pass show "$1" \
    | perl -n -e "print \$_ =~ s/${key}: //ri if /${key}/i"
