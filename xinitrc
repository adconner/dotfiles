/usr/bin/xrdb -load $HOME/.Xresources
/usr/bin/setxkbmap dvorak
/usr/bin/xmodmap $HOME/.Xmodmap
exec xmonad
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
