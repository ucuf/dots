" configure the cursor style for each mode.
" In Normal and Visual mode, use a block cursor.
" In Insert-likes modes, use a vertical bar cursor.
" In Replace-likes modes, use a underline cursor.
" In all modes, blink
set guicursor=a:blinkwait0-blinkon1000-blinkoff1000,n-v:block,i-c-ci:ver25,r-cr-o:hor25

" Don't give the intro message when starting Vim
set shortmess+=I

" Enable mouse support in all modes
set mouse=a

" Enable line numbers.
" Print the numbers relative to the cursor.
" Except for the current selected line,
" print the actual line number.
set number relativenumber

" Always use the clipboard for all operations
set clipboard+=unnamedplus

" Set space as the leader key
let mapleader=" "
