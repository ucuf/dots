export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg

export PATH=$PATH:"$HOME"/.local/bin/

# Have less display colours (from: https://wiki.archlinux.org/index.php/Color_output_in_console#man)
export LESS_TERMCAP_mb=$'\e[1;32m'     # begin bold
export LESS_TERMCAP_md=$'\e[1;34m'     # begin blink
export LESS_TERMCAP_so=$''             # begin reverse video
export LESS_TERMCAP_us=$'\e[01;31m'    # begin underline
export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
export LESSKEY="$XDG_CONFIG_HOME"/less/lesskey
export LESSHISTFILE="$XDG_DATA_HOME"/less/history
export LESS="--ignore-case --clear-screen --mouse"

export GROFF_NO_SGR=1                  # for konsole and gnome-terminal

export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}"/wget/wgetrc
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}"/gtk-2.0/gtkrc

export ANDROID_PREFS_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}"/android
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}"/go
export GNUPGHOME="${XDG_DATA_HOME:-$HOME/.local/share}"/gnupg
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/cargo
