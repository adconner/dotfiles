cd ~
/usr/bin/xrdb -load ~/.Xresources &
/usr/bin/setxkbmap dvorak && /usr/bin/xmodmap ~/.Xmodmap &

sh ~/.fehbg &
unclutter -grab &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
syndaemon -k -i 2 -R -d &

# This must be last
exec xmonad
