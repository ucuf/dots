# Default programs:
export EDITOR="nvim"
export VISUAL="$EDITOR"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export PAGER="less"
export FILE_MANAGER="n"

export fpath=(~/.config/zsh/completions/ $fpath)

if command -v pass > /dev/null; then
	export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}"/password-store
	export PASSWORD_STORE_ENABLE_EXTENSIONS=true
fi

export QT_QPA_PLATFORMTHEME="qt5ct"
export QTWEBENGINE_CHROMIUM_FLAGS="--no-sandbox"
