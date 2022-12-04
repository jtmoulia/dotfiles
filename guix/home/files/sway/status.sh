#!/usr/bin/env sh

# The Sway configuration file in ~/.config/sway/config calls this script.
# You should see changes to the status bar after saving this script.
# If not, do "killall swaybar" and $mod+Shift+c to reload the configuration.

WIFI_NETWORK_DEVICE="${WIFI_NETWORK_DEVICE:-wlp0s20f3}"

upstatus="$(uptime | awk '{ print $3 }')"
upclock="${upstatus::-1}"
whoiam="$(whoami)@$(hostname)"

# The abbreviated weekday (e.g., "Sat"), followed by the ISO-formatted date
# like 2018-10-06 and the time (e.g., 14:01)
date_formatted=$(date "+%a %m-%d-%y %H:%M")

# Returns the battery status: "Full", "Discharging", or "Charging".
battery_status="$(cat /sys/class/power_supply/BAT1/status)"
battery_line="$(cat /sys/class/power_supply/BAT1/capacity)% (${battery_status@L})"

# Human-readable volume
volume_line="$(pamixer --get-volume-human)"

# Current network connection
wifi_essid="$(nmcli device show "$WIFI_NETWORK_DEVICE" | grep GENERAL.CONNECTION | awk '{ print $2 }')"

# Emojis and characters for the status bar
echo "🐂 $whoiam ⬆ $upclock ☁️ $wifi_essid 🤖 $volume_line ⚡ $battery_line 🕘 $date_formatted"
