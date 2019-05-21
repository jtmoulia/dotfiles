#!/usr/bin/env bash
host="$1"
shift

ssh -t "$host" 'cd /usr/local/src/automed; sudo -u automed -sH' "$@"
