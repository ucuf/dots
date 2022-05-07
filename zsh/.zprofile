[ -r "$HOME"/.profile ] && . "$HOME"/.profile

export ZDOTDIR="$XDG_CONFIG_HOME"/zsh

export HISTCONTROL="$HISTCONTROL:ignorespace"
export HISTORY_IGNORE="(ls|history|clear)"



#export ANKI_BASE=$XDG_DATA_HOME/Anki
#export XAUTHORITY="$XDG_RUNTIME_DIR/"Xauthority # This line will break some DMs.
#export RANGER_LOAD_DEFAULT_RC=false

export STARDICT_DATA_DIR="${XDG_DATA_HOME:-$HOME/.local/share}"/stardict/dic
export SDCV_HISTSIZE="${XDG_DATA_HOME:-$HOME/.local/share}"/stardict/sdcv_history
#export SDCV_PAGER="bat"


# Some program settings:

export SUDO_ASKPASS="$HOME"/.local/bin/dmenupass

export SDCV_HISTSIZE=1000
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig


export FZF_HISTORY="$XDG_DATA_HOME/fzf/history"
export FZF_DEFAULT_OPTS="--history=$FZF_HISTORY --reverse"

export _JAVA_AWT_WM_NONREPARENTING=1	# Fix for Java applications in dwm
