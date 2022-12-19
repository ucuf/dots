cursor_mode() {
    # See https://ttssh2.osdn.jp/manual/4/en/usage/tips/vim.html for cursor shapes
    blinking_block='\e[1 q'
    blinking_beam='\e[5 q'

    function zle-keymap-select {
        if [[ ${KEYMAP} == vicmd ]] ||
            [[ $1 = 'block' ]]; then
            echo -ne $blinking_block
        elif [[ ${KEYMAP} == main ]] ||
            [[ ${KEYMAP} == viins ]] ||
            [[ ${KEYMAP} = '' ]] ||
            [[ $1 = 'beam' ]]; then
            echo -ne $blinking_beam
        fi
    }

    zle-line-init() {
         echo -ne $blinking_beam
    }

    zle -N zle-keymap-select
    zle -N zle-line-init
}
cursor_mode

