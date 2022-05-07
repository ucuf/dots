filetype off

source $XDG_CONFIG_HOME/nvim/plug.vim
source $XDG_CONFIG_HOME/nvim/general.vim
source $XDG_CONFIG_HOME/nvim/colorscheme.vim

" c c++ language server
lua require 'user.clangd'

"" javascript typescript language server
"lua require 'user.tsserver'
"
"" org mode
"lua require 'user.org'
"
lua require 'user.lspconfig'
lua require 'user.cmp'
lua require 'user.luasnip'

"" bash language server
"lua require'lspconfig'.bashls.setup{}
"
"lua require 'user.treesitter'
"
"
" Show trailing whitespace:
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

