#’ Ignore duplicate commands
setopt HIST_IGNORE_DUPS
# Ignore commands that start with space
setopt HIST_IGNORE_SPACE
# Share command history data
setopt SHARE_HISTORY
# When HISTFILE size exceeds HISTSIZE delete duplicates first
setopt HIST_EXPIRE_DUPS_FIRST
# Remove unnecessary blanks from each command line being added to the history list.
setopt HIST_REDUCE_BLANKS

# Allow comments even in interactive shells.
setopt INTERACTIVE_COMMENTS

# Make cd push the old directory onto the directory stack.
setopt AUTO_PUSHD
# Don't push multiple copies of the same directory onto the directory stack.
setopt PUSHD_IGNORE_DUPS
# Do not print the directory stack after pushd or popd.
setopt PUSHD_SILENT

# Enable parameter expansion, command substitution and arithmetic expansion
# to be performed in prompts
setopt PROMPT_SUBST

# The file to save the history in when an interactive shell exists.
HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}"/zsh/history
# The maximum number of events stored in the internal history list.
HISTSIZE=100000
# The maximum number of history events to save in the history file.
SAVEHIST=50000


# Create shortcuts for directories.
# hash -d <name>=<path> makes ~<name> a shortcut for <path>.
# You can use this ~name anywhere you would specify a dir, not just with `cd`!
hash -d Trash="${XDG_DATA_HOME:-$HOME/.local/share}"/Trash
hash -d uni="$HOME"/C/university/smi_s4


autoload -Uz vcs_info
precmd_functions+=( vcs_info )
PS1='%n%F{9}@%f%M:%B%F{12}%3~%f%b${vcs_info_msg_0_}%f%(1j.[%j].)%(?.$.%F{9}\$)%f%b '


zstyle ':vcs_info:*' stagedstr 'M'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f'
#zstyle ':vcs_info:*' formats '%F{5}[%F{2}%b%F{5}]%F{2}%c%F{1}%u%f'
zstyle ':vcs_info:*' formats '%F{3}[%B%F{2}%b%%b%F{3}]%F{2}%c%F{1}%u%f'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
zstyle ':vcs_info:*' enable git
+vi-git-untracked() {
if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
	[[ $(git ls-files --other --directory --exclude-standard | sed q | wc -l | tr -d ' ') == 1 ]] ; then
	hook_com[unstaged]+='%F{1}??%f'
fi
}

# Enable colors and change prompt:
#autoload -U colors && colors	# Load colors
#PS1='%F{1}%n%F{15}@%F{10}%M%F{reset}:%b%F{12}%3~%F{reset}$%b '
#PS1='%n%F{9}@%F{reset}%M%F{reset}:%b%F{12}%3~%F{reset}${vcs_info_msg_0_}%F{reset}(%j)%(?.$.%F{9}\$)%F{reset}%b '
#PS1='%n%F{9}@%F{reset}%M%F{reset}:%B%F{12}%3~%b%F{reset}${vcs_info_msg_0_}%F{reset}(%j)%(?.$.%F{9}\$)%F{reset}%b '
#PS1='%n%F{9}@%F{reset}%M%F{reset}:%B%F{12}%3~%b%F{reset}${vcs_info_msg_0_}%F{reset}[%j]%(?.$.%F{9}\$)%F{reset}%b '
#PS1='%n%F{9}@%F{reset}%M%F{reset}:%B%F{12}%3~%b%F{reset}${vcs_info_msg_0_}%F{reset}%(1j.[%j].)%(?.$.%F{9}\$)%F{reset}%b '
#PS1='%n%F{9}@%f%M:%B%F{12}%3~%f%b${vcs_info_msg_0_}%f%(1j.[%j].)%(?.$.%F{9}\$)%f%b '




bPS1='%n%F{9}@%F{reset}%M%F{reset}:%b%F{12}%3~%F{reset}${vcs_info_msg_0_}%F{reset}(%j)%(?.$.%F{9}\$)%F{reset}%b '
sPS1='%n%F{9}@%F{reset}%M%F{reset}:%b%F{12}%3~%F{reset}
${vcs_info_msg_0_}%F{reset}(%j)%(?.$.%F{9}\$)%F{reset}%b '

function mkless() {
	if [ "$PS1" = "$bPS1" ]; then
		PS1="$sPS1";
	else
		PS1="$bPS1";
	fi
}


# Load aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/shell/aliasrc



function take() {
  mkdir -p $@ && cd ${@:$#}
}

function md {
	[ $# = 1 ] && mkdir -p -- "$1" && cd -- "$1"
}

#compdef _directories md
#compdef _default     open

compctl -V directories md

function fcd()
{
	local fzfopt
	[ -n "$1" ] && fzfopt="-q $1"
	cd "$(find ~ -type d | grep -v "\.git" | fzf --height=40% --reverse --preview="tree -L 1 {}" --bind="ctrl-w:toggle-preview" --preview-window=:hidden $fzfopt)"
}

function lf()
{
	local fzfopt
	[ -n "$1" ] && fzfopt="-q $1"
	#find ~ -type f | grep -v "\.git" | fzf --height=40% --reverse -m $fzfopt| xargs -r "${EDITOR:-vi}"
	find ~ -type f | grep -v "\.git" | fzf --height=40% --reverse -m $fzfopt | xargs -I '{}' -r open '{}'
}

function lf2()
{
	local fzfopt
	[ -n "$1" ] && fzfopt="-q $1"
	#find ~ -type f | grep -v "\.git" | fzf --height=40% --reverse -m $fzfopt| xargs -r "${EDITOR:-vi}"
	fd --hidden | fzf --height=40% --reverse -m $fzfopt| xargs -I '{}' -r open '{}'
}

# TODO: fix the 'up /' bug
# TODO: fix if two dircetory have the same prefex bug
function up() {
	[ -d "${PWD%"$1"*}/"$1"" ] && cd "${PWD%"$1"*}/"$1""
  case "${1}" in
    (*[!0-9]*)  : ;;
    ("")        cd .. || return ;;
    ([0-9]*)         cd "$(eval "printf -- '../'%.0s {1..$1}")" || return ;;
  esac
  pwd
}

_up () {
  # Get parents (in reverse order)
  local num_folders_we_are_in=${#${(ps:/:)${PWD}}}
  local i
  for i in {$num_folders_we_are_in..2}
  do
    reply=($reply "`echo $PWD | cut -d'/' -f$i`")
  done
  reply=($reply "/")
}

compctl -V directories -K _up up



# Basic auto/tab complete:
autoload -U compinit && compinit
zstyle ':completion:*' menu select
# list with colors
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zmodload zsh/complist
_comp_options+=(globdots)              # Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

autoload edit-command-line; zle -N edit-command-line
autoload -U edit-command-line && zle -N edit-command-line && bindkey -M vicmd "^v" edit-command-line

bindkey "^?" backward-delete-char
# K in normal mode run man for the word under the cursor
bindkey -M vicmd "K" run-help

# allow ctrl-p, ctrl-n for navigate history
bindkey '^P' up-history
bindkey '^N' down-history

cursor_mode() {
    # See https://ttssh2.osdn.jp/manual/4/en/usage/tips/vim.html for cursor shapes
    cursor_blinking_block='\e[1 q'
    cursor_blinking_beam='\e[5 q'

    function zle-keymap-select {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
            echo -ne $cursor_blinking_block
        elif [[ ${KEYMAP} == main ]] ||
            [[ ${KEYMAP} == viins ]] ||
            [[ ${KEYMAP} = '' ]] ||
            [[ $1 = 'beam' ]]; then
            echo -ne $cursor_blinking_beam
        fi
    }

    zle-line-init() {
         echo -ne $cursor_blinking_beam
    }

    zle -N zle-keymap-select
    zle -N zle-line-init
}
cursor_mode

# Edit the command line using the visual editor
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd '^e' edit-command-line


# Adding Text Objects
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km $c select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km $c select-bracketed
  done
done

#if command -v tmux &> /dev/null && [ -z "$TMUX" ] && [[ $- == *i* ]] && [ -n "$DISPLAY" ] ; then
#	: tmux
#fi

local file=
for file in $ZDOTDIR/rc.d/*.zsh; do
	. $file
done

# Source nnn's config file
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/nnn/nnnrc ]; then
	source "${XDG_CONFIG_HOME:-$HOME/.config}"/nnn/nnnrc
fi

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# TODO: make the aliases and function have a different color then the executables.

# Declare the variable
typeset -A ZSH_HIGHLIGHT_STYLES

# To differentiate aliases from other command types
ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta,bold'

# Load syntax highlighting; should be last.
if [ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

#if [ -f "/home/usef/.local/src/tes/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh" ]; then
#	source /home/usef/.local/src/tes/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
#fi
