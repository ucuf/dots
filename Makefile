histfile:
	[ -f ${HISTFILE} ] || (mkdir -p "$(dirname ${HISTFILE})" && touch ${HISTFILE})

less:
	[ -f "${LESSHISTFILE}" ] || (mkdir -p "$$(dirname ${LESSHISTFILE})" && touch ${LESSHISTFILE})

gnu:
	[ -d ${GNUPGHOME} ] || mkdir -p ${GNUPGHOME}

wget:
	[ -f ${WGETRC} ] || (mkdir -p "$(dirname ${WGETRC})" && touch ${WGETRC})

fzf:
	[ -f ${FZF_HISTORY} ] || (mkdir -p "$(dirname ${FZF_HISTORY})" && touch ${FZF_HISTORY})

gtk2:
	[ -f ${GTK2_RC_FILES} ] || (mkdir -p "$(dirname ${GTK2_RC_FILES})" && touch ${GTK2_RC_FILES})
