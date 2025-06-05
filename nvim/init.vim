" ThePrimeagen/refactoring.nvim
" glepnir/lspsaga
" dcampos/nvim-snippy
" vim-slime
call plug#begin()
Plug 'ThePrimeagen/vim-be-good'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
" Plug 'hrsh7th/cmp-cmdline'

Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/cmp-vsnip'
" Plug 'L3MON4D3/LuaSnip'
" Plug 'saadparwaiz1/cmp_luasnip'
" Plug 'rafamadriz/friendly-snippets'

" Plug 'dcampos/nvim-snippy'
" Plug 'dcampos/cmp-snippy'
" Plug 'honza/vim-snippets'

Plug 'tpope/vim-rsi'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'nvim-zh/better-escape.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'simnalamburt/vim-mundo'
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'lervag/vimtex'

call plug#end()

silent! colorscheme wombat

let mapleader="-"
let maplocalleader="-"

let g:better_escape_shortcut = 'jj'
let g:better_escape_interval = 250

noremap <Del> <C-e>
noremap <Insert> <C-y>
nnoremap gp `[v`]

nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

noremap <silent> <leader><space> :noh<cr>
nnoremap <leader>j :bn<cr>
nnoremap <leader>k :bp<cr>

nnoremap <leader>b :Buffers<cr>
nnoremap <C-p> :GFiles<cr>
nnoremap <A-p> :Files<cr>
nnoremap <C-g> :RG<cr>

" "me" - make current pane large
nnoremap <leader>m :resize<cr>:vertical resize<cr>
nnoremap <leader>M <C-w>=

nnoremap <leader>r  :%s/<c-r>//
xnoremap <leader>r  :s/\%V<c-r>/\%V/

nnoremap <leader>u :MundoToggle<cr>

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

nnoremap <leader>gs :Git<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gC :Git commit --amend<cr>
nnoremap <leader>gb :Git blame<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gg :Ggrep 
nnoremap <leader>gl :Gclog!<cr>
nnoremap <leader>gr :Gread<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gp <Plug>(GitGutterPreviewHunk)
nnoremap <leader>ga <Plug>(GitGutterStageHunk)
nnoremap <leader>gu <Plug>(GitGutterUndoHunk)
nnoremap <leader>gk <Plug>(GitGutterPrevHunk)
nnoremap <leader>gj <Plug>(GitGutterNextHunk)
nnoremap <leader>gn <Plug>(GitGutterNextHunk)
nnoremap [c <Plug>(GitGutterPrevHunk)
nnoremap ]c <Plug>(GitGutterNextHunk)

let g:vsnip_snippet_dir = stdpath("config") . '/snippets'
" imap <expr> <C-e>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-e>'
" smap <expr> <C-e>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-e>'
imap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-e>'
smap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-e>'
imap <expr> <c-j>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<c-j>'
smap <expr> <c-j>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<c-j>'
imap <expr> <c-k> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<c-k>'
smap <expr> <c-k> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<c-k>'
" xmap        <c-e>   <Plug>(vsnip-select-text)
xmap        <c-e>   <Plug>(vsnip-cut-text)

" " imap <expr> <c-e> snippy#can_expand_or_advance() ? '<Plug>(snippy-expand-or-advance)' : '<c-e>'
" " smap <expr> <c-e> snippy#can_jump(1) ? '<Plug>(snippy-next)' : '<c-e>'
" " imap <expr> <c-u> snippy#can_jump(-1) ? '<Plug>(snippy-previous)' : '<c-u>'
" " smap <expr> <c-u> snippy#can_jump(-1) ? '<Plug>(snippy-previous)' : '<c-u>'
" " xmap <c-e> <Plug>(snippy-cut-text)

set completeopt=menu,menuone,noselect
set shiftwidth=2        " Number of spaces to use in auto(indent)
set expandtab           " Insert tabs as spaces
set scrolloff=3
set undofile
set exrc
set secure
set updatetime=100
" set ignorecase
set number
set relativenumber
set virtualedit=block

set wildignore+=*.o,*.pdf,*.log,*.aux

highlight Error ctermbg=52
highlight NvimInternalError ctermfg=88 ctermbg=88
" highlight SpellBad cterm=undercurl
highlight SpellBad cterm=undercurl ctermbg=234
" highlight SpellBad ctermbg=88 cterm=undercurl
highlight SpellCap cterm=undercurl ctermbg=234
" highlight SpellCap cterm=bold ctermbg=234
highlight ColorColumn ctermbg=88
highlight Todo ctermfg=237 ctermbg=11 cterm=italic
highlight DiffAdd ctermbg=239
highlight DiffDelete ctermbg=16
highlight DiffChange ctermbg=236
highlight DiffText ctermbg=236 cterm=underline
highlight GitGutterAdd ctermfg=157 ctermbg=232
highlight GitGutterDelete ctermfg=210 ctermbg=232
highlight GitGutterChange ctermfg=229 ctermbg=232
highlight GitGutterChangeDelete ctermfg=229 ctermbg=232
highlight SignColumn ctermfg=229 ctermbg=232

command -bang Q q<bang>
command -bang W w<bang>
command -bang Bd bd<bang>
command -nargs=* -complete=file -bang Make make<bang> <args>

lua <<EOF

-- Setup nvim-cmp.
local cmp = require'cmp'

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("[A-Za-z_]") ~= nil
end

local feedkey = function(key, mode)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = {
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      -- elseif vim.fn["vsnip#available"](1) == 1 then 
      --   feedkey("<Plug>(vsnip-expand-or-jump)", "")
      -- elseif vim.fn["vsnip#jumpable"](1) == 1 then
      --   feedkey("<Plug>(vsnip-jump-next)", "")
      elseif has_words_before() then
        cmp.complete()
      else
        fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function()
      if cmp.visible() then
        cmp.select_prev_item()
      -- elseif vim.fn["vsnip#jumpable"](-1) == 1 then
      --   feedkey("<Plug>(vsnip-jump-prev)", "")
      end
    end, { "i", "s" }),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
  }, {
    { name = 'buffer' },
  })
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- Setup lspconfig.
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
end

-- servers = { 'pyright', 'texlab', 'clangd', 'rust_analyzer' }
-- servers = { 'pylsp', 'texlab', 'clangd', 'rust_analyzer' }
servers = { 'texlab', 'clangd', 'rust_analyzer', 'julials' }
for _, lsp in pairs(servers) do 
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities
  }
end

-- tree-sitter setup
require'nvim-treesitter.configs'.setup {
  -- ensure_installed = { "c", "lua", "python", "cpp", "latex", "rust" },
  -- ensure_installed = "all",
  sync_install = false,
  auto_install = true,
  ignore_install = { "phpdoc" },
  highlight = {
    enable = true,
    disable = { "latex", "python" },
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
    disable = { "python" }
  }
}

EOF
