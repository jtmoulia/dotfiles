#!/usr/bin/env sh

GUIX_SYSTEM_CONFIG=/etc/config.scm
sudo guix system reconfigure $@ $GUIX_SYSTEM_CONFIG
