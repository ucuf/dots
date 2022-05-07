" =============================================================================
" Plugin Manager Setup
" =============================================================================
"

"filetype off

" Install the plugin manager if it doesn't exist
"let s:plugin_manager=expand('~/.local/share/nvim/site/autoload/plug.vim')
let s:plugin_manager=system('echo -n "${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/autoload/plug.vim"')
let s:plugin_url='https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

if empty(glob(s:plugin_manager))
	echom 'vim-plug not found. Installing...'
	if executable('curl')
		silent exec '!curl -fLo ' . s:plugin_manager . ' --create-dirs ' .
					\ s:plugin_url
	elseif executable('wget')
		call mkdir(fnamemodify(s:plugin_manager, ':h'), 'p')
		silent exec '!wget --force-directories --no-check-certificate -O ' .
					\ expand(s:plugin_manager) . ' ' . s:plugin_url
	else
		echom 'Could not download plugin manager. No plugins were installed.'
		finish
	endif
	augroup vimplug
		autocmd!
		autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
	augroup END
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
" emmet-vim provides support for expanding abbreviations similar to emmet
" see https://emmet.io/.
Plug 'mattn/emmet-vim'

" This plugin provides mappings to easily delete,
" change and add such surroundings in pairs.
Plug 'tpope/vim-surround'


" Oceanic Next theme for neovim
Plug 'mhartington/oceanic-next'

" Quickstart configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'


Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
"Plug 'hrsh7th/cmp-path'
"Plug 'hrsh7th/cmp-cmdline'

" This plugin adds vscode-like pictograms to neovim built-in lsp
Plug 'onsails/lspkind.nvim'

" For luasnip users.
Plug 'L3MON4D3/LuaSnip'
"Plug 'saadparwaiz1/cmp_luasnip'

"Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'ap/vim-css-color'
"Plug 'gko/vim-coloresque'
call plug#end()
