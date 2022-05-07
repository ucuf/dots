[ -f "$HISTFILE" ] || ( mkdir -p "$(dirname $HISTFILE)" && touch "$HISTFILE")
[ -f "$LESSHISTFILE" ] || ( mkdir -p "$(dirname $LESSHISTFILE)" && touch "$LESSHISTFILE")
[ -d "$GNUPGHOME" ] || mkdir -p "$GNUPGHOME"
[ -f "$WGETRC" ] || ( mkdir -p "$(dirname $WGETRC)" && touch "$WGETRC")
[ -f "$FZF_HISTORY" ] || ( mkdir -p "$(dirname $FZF_HISTORY)" && touch "$FZF_HISTORY")
