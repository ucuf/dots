XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}

# Default programs:
export EDITOR="nvim"
export VISUAL="$EDITOR"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export PAGER="less"
export FILE_MANAGER="n"

export path=(~/.local/share/cargo/bin $path)

export fpath=(~/.config/zsh/completions/ $fpath)

if command -v pass > /dev/null; then
	export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
	export PASSWORD_STORE_ENABLE_EXTENSIONS=true
fi

if command -v fzf > /dev/null; then
	export FZF_HISTORY="$XDG_DATA_HOME/fzf/history"
	export FZF_DEFAULT_OPTS="--history=$FZF_HISTORY --reverse --bind ctrl-n:down,ctrl-p:up,ctrl-j:next-history,ctrl-k:previous-history"
fi


export QT_QPA_PLATFORMTHEME="qt5ct"
export QTWEBENGINE_CHROMIUM_FLAGS="--no-sandbox"
