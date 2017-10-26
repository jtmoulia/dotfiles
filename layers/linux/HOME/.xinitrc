#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# Set HiDPI defaults
export QT_SCALE_FACTOR=1.8
export GDK_SCALE=2

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
    xmodmap $HOME/.Xmodmap
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

if [[ "$(which sleepd)" ]]; then
  sleepd
fi

xset -b
setxkbmap -option caps:ctrl_modifier

if [[ "$(which xscreensaver)" ]]; then
    xscreensaver -no-splash &
fi

eval "$(ssh-agent)"

DIRCOLORS_THEME=/home/jtmoulia/repos/dircolors-solarized/dircolors.ansi-dark
if [[ -b "$DIRCOLORS_THEME" ]]; then
    eval $(dircolors "$DIRCOLORS_THEME")
fi

exec stumpwm
