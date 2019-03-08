#!/usr/bin/env bash
case "$1" in
    stop)
        systemctl --user stop xsession.target
        ;;
    "")
        systemctl --user import-environment PATH DBUS_SESSION_ADDRESS
        systemctl --no-block --user start xsession.target
        ;;
    *)
        >&2 echo "unexpected argument: $1"
        exit 1
        ;;
esac
