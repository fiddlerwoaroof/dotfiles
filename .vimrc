" Configuration file for vim
set modelines=0      " CVE-2007-2438

" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements
set nocompatible  " Use Vim defaults instead of 100% vi compatibility
set backspace=2      " more powerful backspacing

" Don't write backup file if vim is being called by "crontab -e"
au BufWrite /private/tmp/crontab.* set nowritebackup
" Don't write backup file if vim is being called by "chpass"
au BufWrite /private/etc/pw.* set nowritebackup


"Enable support for Color xterm
:if has("terminfo")
:  set t_Co=8
:  set t_Sf=[3%p1%dm
:  set t_Sb=[4%p1%dm
:else
:  set  t_Co=8
:  set t_Sf=[3%dm
:  set t_Sb=[4%dm
:endif




"Turn on line-numbering and auto-indent
:set nu
":set cindent shiftwidth=2
":set cindent cino=>2

"Diable the anti-python smart indent of #
:set ignorecase
:set smartcase


"SmartIndent for Python
:set smartindent
:autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
:autocmd BufRead *.mako set ft=mako
:autocmd BufRead *.tac set ft=python

:inoremap # X#

set incsearch
set scrolloff=3
set wildmode=longest,list
set autoread

"Render tabs as 2 columns wide
set tabstop=3 expandtab
set shiftwidth=3
":else
":  set tabstop=3 softtabstop=2 expandtab
":  set shiftwidth=2
":endif

"Code shifting with >> operator will shift code by 2 cols
"    is there a way to shift code by a tab?

"Turn Syntax Highlighting on by default, and assume the xterm background is black
set background=dark
syntax enable
let python_highlight_all = 1
let python_space_errors=1
let python_no_tab_space_error=1
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>
autocmd FileType python set complete+=k~/.vim/syntax/python.vim "isk+=.,(
autocmd FileType python set noexpandtab
autocmd FileType python map K \pW

map W wb"_dwP
map cs :sil! :%s/\s\+$//g<CR>``:%s/^\(\t\+\)\( \+\(\t*\)\)\+/\1\3/gc<CR>``
autocmd BufWritePre *.py normal m`:%s/\s\+$//e ``

map ZZ :w<CR>
map ZX :wq<cr>

map <BS> dh

" when we reload, tell vim to restore the cursor to the saved position
:au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

filetype plugin on
:set pastetoggle=<F11>

python << EOF
import os
import sys
import vim
for p in sys.path:
    if os.path.isdir(p):
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF

let counter = 0                                                                          
inoremap <expr> <C-L> ListItem()                                                         
inoremap <expr> <C-R> ListReset()                                                        
                                                                                        
func ListItem()                                                                          
 let g:counter += 1                                                                     
 return g:counter . '. '                                                                
endfunc                                                                                  
                                                                                        
func ListReset()                                                                         
 let g:counter = 0                                                                      
 return ''                                                                              
endfunc                                                                                  

:set diffopt=vertical,filler,iwhite,foldcolumn:0
