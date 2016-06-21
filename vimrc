let counter = 0
let g:syntastic_auto_loc_list=1
let g:sql_type_default = 'pgsql'
let g:airline_theme="murmur"
let g:haddock_browser_callformat = "%s %s"
let g:haddock_browser = "open"
let g:lisp_rainbow=1
"let g:pandoc_no_empty_implicits=1
"let g:pandoc_use_hard_wraps = 1
"let g:pandoc#modules#enabled =  ["formatting", "folding", "completion", "metadata","menu"]
"let g:pandoc#modules#disabled =  ["command", "bibliographies"]
"let g:pandoc_formatting_settings = "h"
"let g:pandoc#filetypes#handled = ["markdown", "rst", "textile"]
let g:snips_author="Edward Langley"
let g:solarized_termtrans=1
let g:syntastic_python_checkers = ['python']
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'
let g:tex_flavor='xelatex'
let g:unite_force_overwrite_statusline = 0
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
let g:vimclojure#ParenRainbow = 1
let g:virtualenv_directory = "$HOME/python_envs"
let g:phpcomplete_index_composer_command = "composer"
let maplocalleader=','
let $PAGER=''
let python_highlight_all = 1
let python_no_tab_space_error=1
let python_space_errors=1
let vimclojure#WantNailgun = 1
let g:syntastic_scss_sass_args = "-r sass-css-importer -r susy"

" This goes here in case a filetype overrides it

"NeoBundle Scripts-----------------------------
if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=$HOME/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand("$HOME/.vim/bundle"))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:



NeoBundle 'dbakker/vim-paragraph-motion'
 NeoBundle 'jceb/vim-editqf'
 NeoBundle 'mustache/vim-mustache-handlebars'
 NeoBundle 'davidoc/taskpaper.vim'
 NeoBundle 'altercation/vim-colors-solarized'
 NeoBundle 'bitc/vim-hdevtools'
 NeoBundle 'Blackrush/vim-gocode'
 NeoBundle 'bling/vim-airline'
 NeoBundle 'bling/vim-airline-themes'
 NeoBundle 'burnettk/vim-angular'
 NeoBundle 'christoomey/vim-tmux-navigator'
 NeoBundle 'curist/vim-angular-template'
 NeoBundle 'eagletmt/ghcmod-vim'
 NeoBundle 'eagletmt/neco-ghc'
 NeoBundle 'edsono/vim-matchit'
 "NeoBundle 'enomsg/vim-haskellConcealPlus'
 NeoBundle 'exu/pgsql.vim'
 NeoBundle 'fiddlerwoaroof/htmljinja'
 NeoBundle 'fiddlerwoaroof/vim-jinja'
 NeoBundle 'godlygeek/tabular'
 NeoBundle 'groenewege/vim-less'
 NeoBundle 'guns/vim-clojure-static'
 NeoBundle 'ivanov/vim-ipython'
 NeoBundle 'jmcantrell/vim-virtualenv'
 NeoBundle 'kien/rainbow_parentheses.vim'
 NeoBundle 'kovisoft/slimv'
 NeoBundle 'lukerandall/haskellmode-vim'
 "NeoBundle 'm2mdas/phpcomplete-extended'
 NeoBundle 'markcornick/vim-vagrant'
 NeoBundle 'matthewsimo/angular-vim-snippets'
 NeoBundle 'mattn/emmet-vim.git'
 NeoBundle 'msanders/snipmate.vim'
 NeoBundle 'othree/javascript-libraries-syntax.vim'
 NeoBundle 'pangloss/vim-javascript'
 NeoBundle 'raichoo/haskell-vim'
 NeoBundle 'rking/ag.vim' "Ag search utility
 NeoBundle 'rust-lang/rust.vim'
 NeoBundle 'scrooloose/nerdcommenter'
 NeoBundle 'scrooloose/nerdtree'
 NeoBundle 'scrooloose/syntastic'
 NeoBundle 'Shougo/unite.vim'
 NeoBundle 'Shougo/unite-outline'
 NeoBundle 'tsukkee/unite-tag'
 NeoBundle 'Shougo/vimfiler.vim'
 NeoBundle 'Shougo/vimproc'
 NeoBundle 'Shougo/vimshell.vim'
 NeoBundle 'sjl/gundo.vim'
 NeoBundle 'sjl/vitality.vim'
 NeoBundle 'sophacles/vim-bundle-mako'
 NeoBundle 'terryma/vim-multiple-cursors'
 NeoBundle 'tpope/vim-fireplace'
 NeoBundle 'tpope/vim-fugitive'
 NeoBundle 'tpope/vim-repeat'
 NeoBundle 'tpope/vim-surround'
 NeoBundle 'Twinside/vim-haskellFold'
 NeoBundle 'Twinside/vim-hoogle'
 "NeoBundle 'Valloric/YouCompleteMe'
 "NeoBundle 'vim-pandoc/vim-pandoc'
 NeoBundle 'vim-scripts/dbext.vim'
 NeoBundle 'vim-scripts/php.vim--Garvin'
 NeoBundle 'vim-scripts/pydoc.vim'
 NeoBundle 'vim-scripts/VimClojure'
 NeoBundle 'vim-voom/VOoM'
 NeoBundle 'ytsunetsune/unite-outline-euslisp'
 NeoBundle 'xolox/vim-misc'
 NeoBundle 'xolox/vim-easytags'
 NeoBundle 'majutsushi/tagbar'


" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

let counter = 0
let g:airline_theme="murmur"
let g:haddock_browser_callformat = "%s %s"
let g:haddock_browser = "open"
let g:lisp_rainbow=1
"let g:pandoc#filetypes#handled = ["markdown", "rst", "textile"]
"let g:pandoc_formatting_settings = "h"
"let g:pandoc#modules#disabled =  ["command", "bibliographies"]
"let g:pandoc#modules#enabled =  ["formatting", "folding", "completion", "metadata","menu"]
"let g:pandoc_no_empty_implicits=1
"let g:pandoc_use_hard_wraps = 1
let g:slimv_disable_clojure=1
let g:slimv_repl_split=3
let g:slimv_browser_command_suffix='2>&1 > /dev/null'
let g:snips_author="Edward Langley"
let g:solarized_termtrans=1
let g:sql_type_default = 'pgsql'
let g:syntastic_auto_loc_list=1
let g:syntastic_python_checkers = ['python']
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'
let g:tex_flavor='xelatex'
let g:unite_force_overwrite_statusline = 0
let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1
let g:virtualenv_directory = "$HOME/python_envs"
let maplocalleader=','
let $PAGER=''
let python_highlight_all = 1
let python_no_tab_space_error=1
let python_space_errors=1
let vimclojure#WantNailgun = 1


" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.

" OPTIONAL: This enables automatic indentation as you type.
filetype indent on

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

" Configuration file for vim

" Normally we use vim-extensions. If you want true vi-compatibility
" remove change the following statements

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

set omnifunc=syntaxcomplete#Complete
set ignorecase
set smartcase
set autoindent
set nu
set expandtab
set tabstop=3 softtabstop=2
set smarttab
set shiftwidth=2
set incsearch
set scrolloff=4
set wildmode=longest,list
set autoread
set background=dark
set pastetoggle=<F12>
set undodir=~/.vim/undodir
set undofile
set undolevels=10000
set undoreload=100000
set foldlevel=9
set hidden
set laststatus=2
set splitbelow
set splitright
set grepprg=grep\ -nH\ $*
set modelines=1      " CVE-2007-2438
set backspace=2      " more powerful backspacing

syntax enable
colorscheme solarized

if &term =~ "xterm\\|rxvt"
  " use an orange cursor in insert mode
  let &t_SI = "\<Esc>]12;blue\x7"
  " use a red cursor otherwise
  let &t_EI = "\<Esc>]12;yellow\x7"
  silent !echo -ne "\033]12;yellow\007"
  " reset cursor when vim exits
  autocmd VimLeave * silent !echo -ne "\033]112\007"
  " use \003]12;gray\007 for gnome-terminal
endif

"SmartIndent for Python
autocmd BufEnter *.hs compiler ghc
au FocusLost * :wa

function Checkft()
  if &filetype==""
    filetype detect
  endif
endfunction


" when we reload, tell vim to restore the cursor to the saved position
"autocmd FileType python map K \pW
autocmd! BufNewFile * silent! 0r ~/.vim/skel/tmpl.%:e
autocmd BufRead,BufNewFile *.twig set filetype=htmljinja
autocmd BufRead,BufNewFile *.mako set ft=mako
autocmd BufRead,BufNewFile *.md set dictionary+=/usr/share/dict/words ft=markdown
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
autocmd BufRead,BufNewFile *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd BufRead,BufNewFile *.tac set ft=python
autocmd bufwritepost * call Checkft()
autocmd BufWritePre *.py normal m`:%s/\s\+$//e ``

"autocmd FileType haskell set omnifunc=necoghc#omnifunc
autocmd FileType lisp set omnifunc=SlimvOmniComplete
autocmd FileType markdown set linebreak tw=110 noexpandtab nosmartindent autoindent spelllang=en spell
"autocmd FileType pandoc set linebreak tw=110 noexpandtab nosmartindent autoindent
"autocmd FileType pantondoc set linebreak tw=110 noexpandtab nosmartindent autoindent
autocmd FileType python set complete+=k~/.vim/syntax/python.vim "isk+=.,(
"Diable the anti-python smart indent of #
inoremap  # X#

"Turn Syntax Highlighting on by default, and assume the xterm background is black
imap <C-g> :Unite outline -buffer-name=outline -start-insert<CR>
imap <F7> :Unite outline -buffer-name=outline -start-insert<CR>
imap <F8> o:,!pbpaste
imap <F9> o:,!pbpaste
inoremap <expr> <C-L> ListItem()
inoremap <expr> <C-R> ListReset()
inoremap <F2> :set nonumber! foldcolumn=0<CR>
inoremap <F3> :!spot_control pr<CR>
inoremap <F4> :!spot_control p<CR><CR>
inoremap <F5> :!spot_control n<CR>
inoremap <Return> <Return><C-g>u
"inoremap <Space> <Space><C-g>u
"inoremap <Tab> <Tab><C-g>u

map <BS> dh
map <C-g> :Unite outline -buffer-name=outline -start-insert<CR>
map CS :sil! :%s/\s\+$//g<CR>``:%s/^\(\t\+\)\( \+\(\t*\)\)\+/\1\3/gc<CR>``
map <F7> :Unite outline -buffer-name=files -start-insert<CR>
map <F8> o:,!pbpaste<CR>
map <F9> o:,!pbpaste<CR>
map <leader>f :Unite file -buffer-name=files -start-insert<CR>
map <leader>q :Unite buffer -buffer-name=buffers -start-insert<CR>
map W wb"_dwP
map ZX :wq<cr>
map ZZ :w<CR>

nmap <Leader>ci <Plug>VCSCommit
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

python << EOF
import os
import sys
import vim
for p in sys.path:
    if os.path.isdir(p):
        vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF


func ListItem()
 let g:counter += 1
 return g:counter
endfunc

func ListReset()
 let g:counter = 0
 return ''
endfunc

set diffopt=vertical,filler,iwhite,foldcolumn:0

highlight PmenuSel ctermfg=LightGray  ctermbg=DarkRed

if filereadable(".vim.custom")
    so .vim.custom
endif

let g:syntastic_javascript_checkers = ['jshint']
call unite#custom#source('file,file/new,buffer,file_rec','matchers','matcher_fuzzy')

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " Overwrite settings.
  let b:SuperTabDisabled=1
  imap <buffer><expr> <C-v>     unite#do_action('vsplit')
  imap <buffer><expr> <C-s>     unite#do_action('split')
  imap <buffer>  <Tab>     <Plug>(unite_complete)
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <Down>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  imap <buffer> <Up>   <Plug>(unite_select_previous_line)

  " exit with esc
  nmap <buffer> <ESC> <Plug>(unite_exit)

  " exit with ctrl-c
  imap <buffer> <c-c> <Plug>(unite_exit)
  nmap <buffer> <c-c> <Plug>(unite_exit)
endfunction

if executable('ag')
  let g:unite_source_file_async_command = 'ag --follow --nocolor --nogroup --hidden -g "" --ignore ''.sass-cache'''
  "https://github.com/ggreer/the_silver_searcher
  "Use ag in unite grep source.
  let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '--hidden', '-g', '', '--ignore', '.git', '--ignore', '.sass-cache']
  let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '--hidden', '-g', '', '--ignore', '.git', '--ignore', '.sass-cache']
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
        \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
        \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'' ' .
        \ '--ignore ''**/*.pyc'''
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  let g:unite_source_grep_command = 'ack-grep'
  " Match whole word only. This might/might not be a good idea
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a -H'
  "let g:unite_source_grep_default_opts = '--no-heading --no-color -a -w'
  let g:unite_source_grep_default_opts = '--exclude ''\.(git|svn|hg|bzr)'''
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack')
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts = '--no-heading --no-color -a -w'
  let g:unite_source_grep_default_opts = '--exclude ''\.(git|svn|hg|bzr)'''
  let g:unite_source_grep_recursive_opt = ''
endif

nnoremap [unite] <Nop>
nmap <space> [unite]
nmap [unite]s :<C-u>Unite -auto-preview grep:.<C-m>
nmap [unite]o :<C-u>Unite outline -start-insert<C-m>
nmap  [unite]f  :<C-u>Unite file_rec/async -start-insert<C-m>
nmap  [unite]F  :<C-u>Unite file -start-insert<C-m>
nmap  [unite]g  :<C-u>Unite file_rec/git -start-insert<C-m>
nmap [unite]j :<C-u>Unite buffer -start-insert<C-m>
nmap [unite]k :<C-u>Unite tab:no-current<C-m>
nmap [unite]t :NERDTreeToggle<CR>
nmap [unite]u :GundoToggle<CR>
nmap [unite]r :!vagrant rsync<CR>
nmap [unite]h :set hlsearch!<CR>
nmap [unite]l :set list!<CR>

" Reload
map <silent> tu :call GHC_BrowseAll()<CR>
" Type Lookup
map <silent> tw :call GHC_ShowType(1)<CR>
autocmd BufRead,BufNewFile *.css,*.scss,*.less setlocal foldmethod=marker foldmarker={,}

command -nargs=+ Gadd Git add <q-args>
" set runtimepath+=/Applications/LilyPond.app/Contents/Resources/share/lilypond/current/vim
