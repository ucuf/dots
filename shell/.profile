#!/bin/sh

export PATH="$HOME/.local/bin/":"$PATH"

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"$HOME/.config"}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-"$HOME/.local/share"}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-"$HOME/.local/state"}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-"$HOME/.cache"}"
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg

# Default programs:
export EDITOR="nvim"
export VISUAL="$EDITOR"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export PAGER="less"
export FILE_MANAGER="n"


# Have less display colours (from: https://wiki.archlinux.org/index.php/Color_output_in_console#man)
LESS_TERMCAP_mb="$(printf '\e[1;32m')"     # begin bold
export LESS_TERMCAP_mb
LESS_TERMCAP_md="$(printf '\e[1;34m')"     # begin blink
export LESS_TERMCAP_md
LESS_TERMCAP_so="$(printf '')"             # begin reverse video
export LESS_TERMCAP_so
LESS_TERMCAP_us="$(printf '\e[01;31m')"    # begin underline
export LESS_TERMCAP_us
LESS_TERMCAP_me="$(printf '\e[0m')"        # reset bold/blink
export LESS_TERMCAP_me
LESS_TERMCAP_se="$(printf '\e[0m')"        # reset reverse video
export LESS_TERMCAP_se
LESS_TERMCAP_ue="$(printf '\e[0m')"        # reset underline
export LESS_TERMCAP_ue

export LESSKEY="$XDG_CONFIG_HOME/less/lesskey"
export LESSHISTFILE="$XDG_DATA_HOME/less/history"
export LESS="--ignore-case --clear-screen --mouse"

export GROFF_NO_SGR=1                  # for konsole and gnome-terminal

export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

export ANDROID_PREFS_ROOT="$XDG_DATA_HOME/android"
export GOPATH="$XDG_DATA_HOME/go"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GPODDER_HOME="$XDG_CONFIG_HOME/gPodder"
