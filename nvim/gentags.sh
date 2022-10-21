#!/bin/sh

ctags -R --excmd=number \
  --c++-kinds=+p \
  --fields=+iaS \
  --extra=+q \
  -f ~/.config/vim/commontags \
  /usr/include
