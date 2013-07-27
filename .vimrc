" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='xelatex'
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*' 


call pathogen#infect()

" Configuration file for vim
set modelines=1      " CVE-2007-2438

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
:autocmd! BufNewFile * silent! 0r ~/.vim/skel/tmpl.%:e

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
 return g:counter
endfunc

func ListReset()
 let g:counter = 0
 return ''
endfunc

:set diffopt=vertical,filler,iwhite,foldcolumn:0

map <F8> o:,!pbpaste
map <F9> o:,!pbpaste
imap <F8> o:,!pbpaste
imap <F9> o:,!pbpaste

let $PAGER=''
let maplocalleader=','
let g:pandoc_no_empty_implicits=1

command! -range FmtTable python FmtTable(<f-line1>,<f-line2>)

python << EOS
def FmtTable(line1,line2):
    import vim, string
    inputSeparator='|'
    outputSeparator="|"
    cb=vim.current.buffer.range(int(line1)-1,int(line2))
    colLen=[]
    # first we collect col lengths and calculate the longest
    for line in cb[1:]:
        spLine=line.split(inputSeparator)
        for i in range(len(spLine)):
            try:
                if len(spLine[i]) > colLen[i]:
                    colLen[i] = len(spLine[i])
            except IndexError:
                colLen.append(len(spLine[i]))
    tmpBuf=[]
    # Then we fill the cols with spaces
    for line in cb[1:]:
        spLine=line.split(inputSeparator)
        newLine=outputSeparator.join([spElt.ljust(colLen[i]) for i, spElt in enumerate(spLine)]) + outputSeparator
        tmpBuf.append(newLine)
    cb[1:]=tmpBuf[:]
EOS

let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
let g:ghc="/usr/bin/ghc"
au BufEnter *.hs compiler ghc

let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
let vimclojure#WantNailgun = 1 


