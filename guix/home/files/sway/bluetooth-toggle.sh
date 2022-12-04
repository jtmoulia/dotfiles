#!/usr/bin/env sh

if bluetoothctl show | grep "Powered: yes" > /dev/null; then
   bluetoothctl power off > /dev/null
else
   bluetoothctl power on > /dev/null
fi
