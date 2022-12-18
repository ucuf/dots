#!/bin/sh

[ -r /etc/xprofile ] && . /etc/xprofile

if [ -r "${XDG_CONFIG_HOME:-$HOME/.config}/x11/xprofile" ]; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/x11/xprofile"
elif [ -r ~/.xprofile ]; then
	. ~/.xprofile
fi

exec awesome
# exec dwm
