call plug#begin()
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'
Plug 'rafamadriz/friendly-snippets'

Plug 'nvim-treesitter/nvim-treesitter'

Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

call plug#end()

silent! colorscheme wombat

let mapleader="-"
let maplocalleader="-"

inoremap jj <Esc>

noremap <silent> <leader><space> :noh<cr>
nnoremap <leader>j :bn<cr>
nnoremap <leader>k :bp<cr>

nnoremap <leader>b :Buffers<cr>
nnoremap <C-p> :Files<cr>
nnoremap <C-g> :RG<cr>

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

nnoremap <leader>gb :Git blame<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gC :Git commit --amend<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gg :Ggrep 
nnoremap <leader>gl :Glog<cr>
nnoremap <leader>gr :Gread<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nmap <leader>gp <Plug>(GitGutterPreviewHunk)
nmap <leader>ga <Plug>(GitGutterStageHunk)
nmap <leader>gu <Plug>(GitGutterUndoHunk)
nmap <leader>gk <Plug>(GitGutterPrevHunk)
nmap <leader>gj <Plug>(GitGutterNextHunk)
nmap <leader>gn <Plug>(GitGutterNextHunk)
nmap [c <Plug>(GitGutterPrevHunk)
nmap ]c <Plug>(GitGutterNextHunk)

" Expand or jump
imap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : ''
smap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : ''

" Jump forward or backward
imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

set completeopt=menu,menuone,noselect
set shiftwidth=2        " Number of spaces to use in auto(indent)
set expandtab           " Insert tabs as spaces

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
" highlight DiffAdd      gui=none    guifg=NONE          guibg=#bada9f
" highlight DiffChange   gui=none    guifg=NONE          guibg=#e5d5ac
" highlight DiffDelete   gui=bold    guifg=#ff8080       guibg=#ffb0b0
" highlight DiffText     gui=none    guifg=NONE          guibg=#8cbee2
highlight GitGutterAdd ctermfg=157 ctermbg=232
highlight GitGutterDelete ctermfg=210 ctermbg=232
highlight GitGutterChange ctermfg=229 ctermbg=232
highlight GitGutterChangeDelete ctermfg=229 ctermbg=232
highlight SignColumn ctermfg=229 ctermbg=232

lua <<EOF

-- Setup nvim-cmp.
local cmp = require'cmp'

cmp.setup({
snippet = {
-- REQUIRED - you must specify a snippet engine
expand = function(args)
vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
-- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
-- require('snippy').expand_snippet(args.body) -- For `snippy` users.
-- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
end,
},
window = {
-- completion = cmp.config.window.bordered(),
-- documentation = cmp.config.window.bordered(),
},
mapping = cmp.mapping.preset.insert({
['<C-b>'] = cmp.mapping.scroll_docs(-4),
['<C-f>'] = cmp.mapping.scroll_docs(4),
['<C-Space>'] = cmp.mapping.complete(),
['<C-e>'] = cmp.mapping.abort(),
['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
}),
sources = cmp.config.sources({
{ name = 'nvim_lsp' },
{ name = 'vsnip' }, -- For vsnip users.
-- { name = 'luasnip' }, -- For luasnip users.
-- { name = 'ultisnips' }, -- For ultisnips users.
-- { name = 'snippy' }, -- For snippy users.
}, {
{ name = 'buffer' },
})
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
sources = cmp.config.sources({
{ name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
}, {
{ name = 'buffer' },
})
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
mapping = cmp.mapping.preset.cmdline(),
sources = {
{ name = 'buffer' }
}
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
mapping = cmp.mapping.preset.cmdline(),
sources = cmp.config.sources({
{ name = 'path' }
}, {
{ name = 'cmdline' }
})
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
require'lspconfig'.pyright.setup{ capabilities = capabilities }
require'lspconfig'.texlab.setup{ capabilities = capabilities }
require'lspconfig'.clangd.setup{ capabilities = capabilities }

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "lua", "python", "cpp", "latex" },
  highlight = {
    enable = true,
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  }, 
  indent = {
    enable = true
  }
}

EOF
