#!/usr/bin/env elvish
use re

fn configure-monitors [&left-of=$true &verbose=$false &primary=eDP-1 &secondary=HDMI-1 &card=card0]{
  configurations = [
    &primary-37d96de79918e943875f54704387325e= [--auto --primary --pos 0x360]
    &secondary-37d96de79918e943875f54704387325e= [--auto --pos 1920x0]
  ]
  device_cards = [
    &HDMI-1= HDMI-A-1
  ]

  primary_args = [--auto --primary]
  secondary_position = $false
  if $left-of {
    secondary_position = --left-of
  } else {
    secondary_position = --right-of
  }
  secondary_args = [--auto $secondary_position $primary]
  xrandr-output = (xrandr -q | slurp)

  fn find-in-xrandr [regex]{
    for match [(re:find $regex $xrandr-output)] {
      for group $match[groups][1:] {
        put $group[text]
      }
    }
  }
  disconnected = [(find-in-xrandr "(.+) disconnected")]
  connected = [(find-in-xrandr "(.+) connected")]

  fn logger [@args]{
    if $verbose {
      echo $@args
    }
  }

  fn get-device [device]{
    device_card_name = $device
    if (has-key $device_cards $device) {
      device_card_name = $device_cards[$device]
    }
    path = "/sys/devices/pci0000:00/0000:00:02.0/drm/"$card"/"$card"-"$device_card_name"/edid"
    if ?(ls $path 1> /dev/null 2> &1 > /dev/null) {
      put (md5sum $path | cut -f 1 -d ' ')
    } else {
      put $false
    }
  }

  fn get-device-args [key device_id]{
    if $device_id {
      configuration_key = $key"-"$device_id
      if (and $configuration_key (has-key $configurations $configuration_key)) {
        put $configurations[$configuration_key]
        return
      }
    }
    if (eq $key primary) {
      put $primary_args
    } elif (eq $key secondary) {
      put $secondary_args
    } else {
      fail "unknown key: "$key
    }
  }

  logger "Connected monitors:" $@connected
  logger "Disconnected monitors:" $@disconnected

  if (has-value $connected $primary) {
    if (has-value $connected $secondary) {
      logger "Found two connected monitors..."
      logger Configuring with primary: $primary, secondary: $secondary ...
      secondary_device_id = (get-device $secondary)
      primary_device_args = (get-device-args primary $secondary_device_id)
      secondary_device_args = (get-device-args secondary $secondary_device_id)
      logger found primary (get-device $primary) $primary_device_args
      logger found secondary (get-device $secondary) $secondary_device_args
      xrandr --output $primary $@primary_device_args --output $secondary $@secondary_device_args
    } else {
      logger "Found one connected monitor..."
      logger Configuring with primary: $primary ...
      logger found primary (get-device $primary)
      # xrandr --output $primary $@primary_args --output $secondary --off
    }
  } else {
    # The primary monitor isn't connected??
    echo "Unexpected output number of screens!"
    exit 1
  }

  logger "Monitors successfully configured."
}

fn configure-monitors-watch [&sleep-interval=4 &verbose=$false]{
  while $true {
    configure-monitors &verbose=$verbose
    sleep $sleep-interval
  }
}

configure-monitors &verbose=$true
#configure-monitors-watch
