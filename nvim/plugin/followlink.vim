" Follow symlinks so we're editing the actual file instead of the symlink
" (change the value returned by %).
"
" Requires readlink - part of GNU coreutils
" Uses vim-bufkill if available.
function! s:SwitchToActualFile()
  let fname = resolve(expand('%:p'))
  bwipeout
  exec "edit " . fname
endfunction
command FollowSymlink call s:SwitchToActualFile()
