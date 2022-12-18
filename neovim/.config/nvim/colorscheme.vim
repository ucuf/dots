" use true color in the terminal
if (has("termguicolors"))
	set termguicolors
endif

if (has("nvim"))
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

set guifont=monospace:h13

syntax enable
"colorscheme tokyonight
"colorscheme wombat256grf
" colorscheme onehalfdark
" colorscheme onedark
" colorscheme space-vim-dark
" colorscheme sonokai
" colorscheme PaperColor
" colorscheme monokai
"colorscheme sublimemonokai
"colorscheme OceanicNext
"let g:material_style = "darker"
colorscheme darkside


" Show trailing whitespace:
highlight ExtraWhitespace ctermbg=red guibg=#F43841
match ExtraWhitespace /\s\+$/
"autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
