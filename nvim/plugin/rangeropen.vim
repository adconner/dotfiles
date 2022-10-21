" Compatible with ranger 1.4.2 through 1.6.*
"
" Add ranger as a file chooser in vim

fun! RangerChooser()
    exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
    if filereadable('/tmp/chosenfile')
        exec 'edit ' . system('cat /tmp/chosenfile')
        call system('rm /tmp/chosenfile')
    endif
    redraw!
endfun
command RangerOpen call RangerChooser()
