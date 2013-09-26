" Initialization {{{1

set nocompatible

" General Preferences {{{1
" Plugin options {{{2

" Set up vundle...
filetype off
set rtp+=~/.vim/bundle/vundle
silent! call vundle#rc()

if exists(':Bundle')
  Bundle 'gmarix/vundle'

  Bundle 'bitc/vim-hdevtools.git'
  Bundle 'godlygeek/tabular'
  Bundle 'kien/ctrlp.vim'
  Bundle 'LaTeX-Box-Team/LaTeX-Box'
  Bundle 'michaeljsmith/vim-indent-object'
  Bundle 'Rip-Rip/clang_complete'
  Bundle 'scrooloose/syntastic.git'
  Bundle 'SirVer/ultisnips'
  Bundle 'tpope/vim-characterize'
  Bundle 'tpope/vim-commentary'
  Bundle 'tpope/vim-eunuch'
  Bundle 'tpope/vim-fugitive'
  Bundle 'tpope/vim-repeat'
  Bundle 'tpope/vim-rsi'
  Bundle 'tpope/vim-sensible'
  Bundle 'tpope/vim-sleuth'
  Bundle 'tpope/vim-speeddating'
  Bundle 'tpope/vim-surround'
  Bundle 'tpope/vim-tbone'
  Bundle 'tpope/vim-unimpaired'
  Bundle 'wlangstroth/vim-haskell'
endif

filetype plugin indent on
syntax on

" Support file locations {{{2

if has('unix')
    " swap file location
  set directory=$XDG_CACHE_HOME/vim,.,~/tmp,/var/tmp,/tmp
    " viminfo location
  set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
    " persistent undo location
  set undodir=$XDG_CACHE_HOME/vim,.,~/tmp,/var/tmp,/tmp
    " dictionary for spell check
  set dictionary+=/usr/share/dict/words
endif

" set tags+="~/.vim/commontags"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" Appearance {{{2

if has('gui_running')
  silent! colorscheme wombat      " Set the colorscheme
else
  silent! colorscheme wombat
endif

if has('gui_win32')
  set guifont=courier_new:h11
endif

" Tabbing {{{2
set shiftwidth=2        " Number of spaces to use in auto(indent)
set softtabstop=2       " Just to be clear
set expandtab           " Insert tabs as spaces
set smarttab

" Searching {{{2
set wrapscan            " Wrap searches
set ignorecase          " Ignore search term case...
set smartcase           " ... unless term contains an uppercase character
set incsearch           " Highlight search...
set hlsearch            " ... as you type
set gdefault            " replace every occurrance by default
" noremap / /\v
" noremap ? ?\v
" use aggressive regex by default

" Wrapping {{{2
set textwidth=80        " Hard-wrap text at nth column
set linebreak           " Break existing lines at sane places
set wrap                " Wrap long lines

" General {{{2
set ttyfast
set ruler               " Show [line,col] number (in status bar)
set history=1000        " Number of ":" commands and searches to remember
set wildmenu            " dmenu style menu for commands
set wildmode=list:longest " complete longest word part
set fillchars=""        " Remove characters in window split
set encoding=utf-8      " Default encoding
set scrolloff=3         " 3 lines of context
set hidden              " allow fast switching of files w/o saving
set nostartofline       " dont reset to start of line for large movements
set showcmd             " show partial commands
set mouse-=a            " disable mouse
set backspace=indent,eol,start " Allow backspacing on the given values
" set undofile            " Use a persistent undo file
" set formatoptions+=a    " auto format
set formatoptions+=1    " dont auto line break after one letter word if possible
set lazyredraw          " dont redraw screen during macro execution

" makes mistakes if there are numbers in a block of text and one ends up at the
" beginning of a line
" set formatoptions+=n    " format numbered lists correctly

" this results in an error on machines without a sufficient vim version
silent! set formatoptions+=j    " allow sensible joining of comments,

" ignore files for tab completion and ctrlp
set wildignore+=*.o,*.pdf,*.log,*.aux

" set default commentstring
set commentstring=#%s

let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_style_error_symbol='✗'
let g:syntastic_style_warning_symbol='⚠'

let g:SuperTabDefaultCompletionType="context"
let g:SuperTabContextDefaultCompletionType="<c-p>"

" Search runtimepath in forward order to be more efficient
let g:UltiSnipsDontReverseSearchPath=0

let g:tex_flavor = "latex"

" Mappings {{{1
let mapleader="-"

" Insert Mode {{{2
imap <C-U> <C-G>u<C-U>
imap jj <Esc>

" insert mode commands I care about:
" CTRL-r insert register
" CTRL-h backspace
" CTRL-w delete back word
" CTRL-v insert literal

imap <c-u> <c-x>
imap <c-l> <c-x><c-l>
imap <c-]> <c-x><c-]>
imap <c-t> <c-x><c-]>

" set so that <tab> and <s-tab> go forward and back
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" Normal/Visual modes {{{2
" Movement {{{3

" navigate by display lines
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" "me" - make current workspace large
map <leader>m :resize<cr>:vertical resize<cr>
map <leader>M <C-w>=

" make scrolling more convenient
map <c-j> <c-e>
map <c-k> <c-y>
map <Del> <C-e>
map <Insert> <C-y>

" make forward line searches easier to reach
noremap ; ,
noremap , ;

" Editing {{{3

" in case yankring not installed
nnoremap Y y$

" select last edited text
nmap gV `[v`]

" split line
nmap K i<cr><esc>
" TODO K in visual or operator mode splits at sensible places in the line
" (possibly filetype dependant). Ex. latex splits lines at binary relations

map <silent> <leader><space> :noh<cr>

" quick replace of highlighted strings
nmap <leader>r :%s/<c-r>//
vmap <leader>r :s/\%V<c-r>/\%V/

" File Navigation {{{3
" make tabs, windows, and buffers easier
"   only in normal mode because in visual these lose the selection
nmap <leader>j :bn<cr>
nmap <leader>k :bp<cr>

" Misc Mappings {{{3

" disable help key (especially important in gui)
map <F1> <nop>

" disable key which takes us away from buffer
map Q <nop>
" set keywordprg=man\ --regex

map <leader>gw :Gwrite<cr>
map <leader>gr :Gread<cr>
map <leader>gd :Gdiff<cr>
map <leader>gs :Gstatus<cr>
map <leader>gc :Gcommit<cr>
map <leader>gl :Glog<cr>
map <leader>gb :Gblame<cr>

" map coS :SyntasticToggleMode<cr>

" Command mode {{{2

" sudo write
command -nargs=* -complete=file Sw SudoWrite <args>
command -nargs=* -complete=file SW SudoWrite <args>

"Alias ex commands easily capitalized
" command -nargs=* -complete=file -bang W w<bang> <args>
command -bang Q q<bang>
command -nargs=* -complete=file -bang Make make<bang> <args>

" Todo {{{1
" make it so cpp and h file open in parallel in vsplitted windows
" select text just pasted
" make auto complete case insensitive
" visual paren matching plugin (what was that one called again?)

" vim: fdm=marker
