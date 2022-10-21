augroup filetypedetect
  au! BufRead,BufNewFile *.g      setfiletype gap
  au! BufRead,BufNewFile *.gi     setfiletype gap
  au! BufRead,BufNewFile *.gd      setfiletype gap
  au! BufRead,BufNewFile .gaprc      setfiletype gap
  au! BufRead,BufNewFile *.sage      setfiletype python
augroup END
