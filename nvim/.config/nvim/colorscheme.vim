" use true color in the terminal
if (has("termguicolors"))
	set termguicolors
endif

if (has("nvim"))
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

syntax enable
"colorscheme tokyonight
"colorscheme wombat256grf
" colorscheme onehalfdark
" colorscheme onedark
" colorscheme space-vim-dark
" colorscheme sonokai
" colorscheme PaperColor
" colorscheme monokai
" colorscheme sublimemonokai
let g:oceanic_next_terminal_bold = 1
"let g:oceanic_next_terminal_italic = 1
colorscheme OceanicNext
hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi EndOfBuffer guibg=NONE ctermbg=NONE


