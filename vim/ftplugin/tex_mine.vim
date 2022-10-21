map <leader>v :LatexView<cr>
map <leader>e :LatexErrors<cr>
map <leader>l :LatexMk<cr>

let g:syntastic_tex_checkers=['chktex']

let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_viewer="zathura"
" let g:LatexBox_build_dir="/tmp"
