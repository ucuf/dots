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

" Disable modelines as a security precaution
set modelines=0
set nomodeline

" set cursorlineopt=number
" set cursorline
set nowrap

" Make recognizes .h files as c files
let g:c_syntax_for_h=1

set listchars=tab:<->,eol:$,lead:·,trail:·

set tabstop=4
set shiftwidth=4
set softtabstop=4
set noexpandtab


set statusline=
set statusline+=%<%f
set statusline+=\ %((%H%W%R%M)%)
set statusline+=%=
set statusline+=%y
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\[%{&fileformat}\]
set statusline+=\ %-6.((%l,%c%(,%V%))%)
set statusline+=\ %p%%

" fu Quickfixtextfunc(info) abort
" 	if a:info.quickfix
" 		let qfl = getqflist(#{id: a:info.id, items: 1}).items
" 	else
" 		let qfl = getloclist(a:info.winid, #{id: a:info.id, items: 1}).items
" 	endif
" 	let l = []
" 	for idx in range(a:info.start_idx - 1, a:info.end_idx - 1)
" 		let e = qfl[idx]
" 		let fname = bufname(e.bufnr)->fnamemodify(':t')
" 		let displayed = printf('%s:%d:%d %s',
" 					\ fname,
" 					\ e.lnum,
" 					\ e.col,
" 					\ e.text
" 					\ )
" 		call add(l, displayed)
" 	endfor
" 	return l
" endfu
"
" set quickfixtextfunc=Quickfixtextfunc
