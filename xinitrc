cd ~
/usr/bin/xrdb -load ~/.Xresources
/usr/bin/setxkbmap dvorak
/usr/bin/xmodmap ~/.Xmodmap

sh ~/.fehbg &
unclutter &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

# This must be last for some reason
exec xmonad
