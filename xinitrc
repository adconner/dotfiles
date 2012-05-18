cd ~
/usr/bin/xrdb -load $HOME/.Xresources
/usr/bin/setxkbmap dvorak
/usr/bin/xmodmap $HOME/.Xmodmap
exec xmonad

# for some reason not working
#unclutter &
#autocutsel -fork &
#autocutsel -selection PRIMARY -fork &
