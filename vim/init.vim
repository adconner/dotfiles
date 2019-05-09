" commands to remember 
" gv '[ ']
" :Rg pattern

" Set up vundle...
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
silent! call vundle#begin()

if exists(':Plugin')
  Plugin 'VundleVim/Vundle.vim'

  " heavyweight plugins
  Plugin 'SirVer/ultisnips'
  Plugin 'neoclide/coc.nvim'

  " Plugin 'prabirshrestha/vim-lsp'
  " Plugin 'w0rp/ale'
  " Plugin 'Shougo/deoplete.nvim'
    " for Vim8 only and deoplete support:
    " Plugin 'roxma/nvim-yarp'
    " Plugin 'roxma/vim-hug-neovim-rpc'

  Plugin 'junegunn/fzf.vim'
  Plugin 'benmills/vimux'
  Plugin 'kana/vim-textobj-user'
  Plugin 'glts/vim-textobj-comment'
  Plugin 'michaeljsmith/vim-indent-object'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-repeat'
  Plugin 'godlygeek/tabular'
  Plugin 'honza/vim-snippets'
  Plugin 'tpope/vim-characterize'
  Plugin 'tpope/vim-eunuch'
  Plugin 'tpope/vim-fugitive'
  Plugin 'airblade/vim-gitgutter'
  Plugin 'tpope/vim-rsi'
  Plugin 'tpope/vim-speeddating'
  Plugin 'tpope/vim-surround'
  Plugin 'tpope/vim-sensible'
  Plugin 'tpope/vim-unimpaired'
  Plugin 'Shougo/context_filetype.vim'
  Plugin 'Konfekt/FastFold'
  " Plugin 'christoomey/vim-tmux-navigator'

  " filetype plugins

  " Plugin 'deoplete-plugins/deoplete-jedi'
  " Plugin 'Rip-Rip/clang_complete'

  " Plugin 'bitc/vim-hdevtools.git'
  " Plugin 'LaTeX-Box-Team/LaTeX-Box'
  " Plugin 'Twinside/vim-haskellConceal'
  " Plugin 'wlangstroth/vim-haskell'
endif

call vundle#end()
filetype plugin indent on
syntax on

let g:gitgutter_map_keys = 0
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

let g:tex_flavor = "latex"

if has('nvim')
  " increases startup times
  if !isdirectory($XDG_CACHE_HOME . "/nvim")
    call mkdir($XDG_CACHE_HOME . "/nvim","p")
  endif
  " swap file location
  set directory=$XDG_CACHE_HOME/nvim,.
    " viminfo location
  " set viminfo+=n$XDG_CACHE_HOME/nvim/viminfo
    " persistent undo location
  set undodir=$XDG_CACHE_HOME/nvim,.
    " dictionary for spell check
  set dictionary+=/usr/share/dict/words
else
  if !isdirectory($XDG_CACHE_HOME . "/vim")
    call mkdir($XDG_CACHE_HOME . "/vim","p")
  endif
  " swap file location
  set directory=$XDG_CACHE_HOME/vim,.
    " viminfo location
  set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
    " persistent undo location
  set undodir=$XDG_CACHE_HOME/vim,.
    " dictionary for spell check
  set dictionary+=/usr/share/dict/words
endif

silent! colorscheme wombat

set shiftwidth=2        " Number of spaces to use in auto(indent)
set softtabstop=2       " Just to be clear
set expandtab           " Insert tabs as spaces
set smarttab

set wrapscan            " Wrap searches
set ignorecase          " Ignore search term case...
set smartcase           " ... unless term contains an uppercase character
set incsearch           " Highlight search...
set hlsearch            " ... as you type

set textwidth=80        " Hard-wrap text at nth column
set linebreak           " Break existing lines at sane places
set wrap                " Wrap long lines

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
" set mouse-=a            " disable mouse
set backspace=indent,eol,start " Allow backspacing on the given values
set undofile            " Use a persistent undo file
" set formatoptions+=a    " auto format
" set formatoptions+=1    " dont auto line break after one letter word if possible
set formatoptions+=j    " all sensible joining of comments
" makes mistakes if there are numbers in a block of text and one ends up at the
" beginning of a line
" set formatoptions+=n    " format numbered lists correctly
set lazyredraw          " dont redraw screen during macro execution
set exrc                " allow project local vimrc/exrc files
set secure              " but maintain security for the above
set splitbelow
set splitright
set updatetime=300      "for plugins (gitgutter)
" set autoread " maybe use this manually

" this block recommended by coc.nvim
set nobackup " file backup doesnt play nice with some language servers
set nowritebackup " on loss of power, we should have other means to recover
" set cmdheight=2
set shortmess+=c
" set signcolumn=yes

" ignore files for tab completion
set wildignore+=*.o,*.pdf,*.log,*.aux

" set default commentstring
" set commentstring=#%s

" insert mode commands I care about:
" CTRL-r insert register
" CTRL-h backspace
" CTRL-w delete back word
" CTRL-v insert literal

inoremap jj <Esc>
" tnoremap jj <C-\><C-n>

nnoremap <C-p> :Files<cr>

" nnoremap coS :SyntasticToggleMode<cr>
" nnoremap cop :call deoplete#toggle()<cr>
let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsJumpForwardTrigger="<c-e>"
let g:UltiSnipsJumpBackwardTrigger="<c-u>"


inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : 
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col-1] =~# '\s'
endfunction

" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<cr>"
inoremap <silent><expr> <c-space> coc#refresh()
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

nmap <silent> <space>k <Plug>(coc-diagnostic-prev)
nmap <silent> <space>j <Plug>(coc-diagnostic-next)

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

set formatexpr=CocAction('formatSelected')

nnoremap <silent> K :call <SID>show_documentation()<cr>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocActionAsync('doHover')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

" nnoremap <silent> <space>a :<C-u>CocList diagnostics<cr>
" nnoremap <silent> <space>e :<C-u>CocList extensions<cr>
" nnoremap <silent> <space>c :<C-u>CocList commands<cr>
" nnoremap <silent> <space>o :<C-u>CocList outline<cr>
" nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>
" nnoremap <silent> <space>j :<C-u>CocNext<cr>
" nnoremap <silent> <space>k :<C-u>CocPrev<cr>
" nnoremap <silent> <space>p :<C-u>CocListResume<cr>

noremap <Tab> %
noremap j gj
noremap k gk
noremap gj j
noremap gk k
noremap <Del> <C-e>
noremap <Insert> <C-y>
" make forward line searches easier to reach
noremap ; ,
noremap , ;
nnoremap Y y$
" disable help key (especially important in gui)
map <F1> <nop>
" disable key which takes us away from buffer
map Q <nop>

let mapleader="-"

noremap <silent> <leader><space> :noh<cr>
nnoremap <leader>j :bn<cr>
nnoremap <leader>k :bp<cr>
nnoremap <leader>b :Buffers<cr>

" "me" - make current pane large
nnoremap <leader>m :resize<cr>:vertical resize<cr>
nnoremap <leader>M <C-w>=

" use x-mode maps when mapping printible characters in visual mode

nnoremap <leader>t :VimuxPromptCommand<cr>
nnoremap <leader>c :up<cr>:VimuxRunLastCommand<cr> 
function! VimuxSlime()
  " let @v=substitute(@v,'\n','','')
  " TODO fix for sage
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
endfunction
vnoremap <leader>h "vy :call VimuxSlime()<cr>
nmap <leader>h ^v$<leader>h
" nmap <leader>h vip<leader>h

nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gC :Gcommit --amend<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gg :Ggrep 
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gr :Gread<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
" TODO fix below
nnoremap <leader>gp <Plug>GitGutterPreviewHunk
nnoremap <leader>ga <Plug>GitGutterStageHunk
nnoremap <leader>gu <Plug>GitGutterUndoHunk

nnoremap <leader>o  :RangerOpen<cr>

nnoremap <leader>r  :%s/<c-r>//
xnoremap <leader>r  :s/\%V<c-r>/\%V/
nnoremap <leader>a= :Tabularize /=<cr>
nnoremap <leader>a& :Tabularize /&<cr>

command -nargs=* -complete=file Sw SudoWrite <args>
command -nargs=* -complete=file SW SudoWrite <args>

command -bar Gc Gwrite|Gcommit -m "updates"
command Gcq Gc|q

command -bang Q q<bang>
command -bang Bd bd<bang>
command -nargs=* -complete=file -bang Make make<bang> <args>

highlight Error ctermbg=52
highlight NvimInternalError ctermfg=88 ctermbg=88
highlight SpellBad ctermbg=88 cterm=undercurl
highlight SpellCap ctermbg=17
highlight ColorColumn ctermbg=88
highlight Todo ctermfg=237 ctermbg=11 cterm=italic
highlight CocHighlightText ctermbg=236

" evaluate and map other fzf uses
" visual paren matching plugin (what was that one called again?)
" maybe more general semantic highlighting
" language server implementation
" better vimux binds
" better split navigation (does tmux-navigator hang tmux if vim is blocked?)
" git commit --amend shortcut
" better way of navigating splits, tmux navigator would be good if I could ensure it doesnt hang tmux if vim hangs

" vim: fdm=marker
