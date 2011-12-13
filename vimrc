
" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Pathogen plugin loader
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" General Preferences {{{1
" File based
set ttyfast
filetype plugin on      " Load file type plugins
filetype indent on      " Enable file type based indentation
syntax on               " Enable syntax highlighting
set background=dark
if ('gui_running')
  colorscheme solarized      " Set the colorscheme
else 
  colorscheme solarized
endif

" Tabbing
set tabstop=2           " The number of spaces a tab is
set shiftwidth=2        " Number of spaces to use in auto(indent)
set softtabstop=2       " Just to be clear
set expandtab           " Insert tabs as spaces
set smarttab

" Searching
set wrapscan            " Wrap searches
set ignorecase          " Ignore search term case...
set smartcase           " ... unless term contains an uppercase character
set incsearch           " Highlight search...
set hlsearch            " ... as you type
set gdefault            " replace every occurrance by default
noremap / /\v
" use aggressive regex by default

" Wrapping
"set textwidth=80        " Hard-wrap text at nth column
set linebreak           " Break existing lines at sane places
set wrap                " Wrap long lines

" General
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
set backspace=indent,eol,start
" Allow backspacing on the given values
set tags+=~/.vim/commontags
set tags+=./tag

" Requries Vim 7.3
" set relativenumber     " Use line numbers relative to current line
" set undofile           " Use a persistent undo file



" Mappings {{{1
let mapleader=","

inoremap <C-U> <C-G>u<C-U>
inoremap jj <Esc>

" Move left and right between splits with CTRL+[hl]
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
" todo: for some reason this also skips about 8 characters

noremap <C-j> <C-e>
noremap <C-k> <C-y>
noremap <Del> <C-e>
noremap <Insert> <C-y>

noremap ' `
noremap ` '

noremap j gj
noremap k gk
noremap gj j
noremap gk k

" todo set up command line editing with vi like vi readline,
cnoremap <c-a> <Home>
cnoremap <c-e> <End>
cnoremap <c-f> <Right>
cnoremap <c-b> <Left>
cnoremap <A-f> <S-Right>
" has some trouble with this, see i_CTRL-F
cnoremap <A-b> <S-Left>
cnoremap <c-d> <Del>

call togglebg#map("<F5>")
